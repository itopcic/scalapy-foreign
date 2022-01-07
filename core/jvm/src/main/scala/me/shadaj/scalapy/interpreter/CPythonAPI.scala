package me.shadaj.scalapy.interpreter

import com.sun.jna.{Native, NativeLong, Pointer, WString}

import scala.util.{Failure, Properties, Success}
import jdk.incubator.foreign._

import java.lang.invoke.{MethodHandle, MethodType}

class CPythonAPIInterface {

  val pythonLibrariesToTry =
    Option(System.getenv("SCALAPY_PYTHON_LIBRARY"))
      .orElse(Properties.propOrNone("scalapy.python.library"))
      .toSeq ++
      Seq(
        "python3",
        "python3.7", "python3.7m",
        "python3.8", "python3.8m",
        "python3.9", "python3.9m"
      )

  val loadAttempts = pythonLibrariesToTry.toStream.map(n => try {
    System.loadLibrary(n)
    Native.register(n)
    Success(true)
  } catch {
    case t: Throwable => Failure(t)
  })

  loadAttempts.find(_.isSuccess).getOrElse {
    loadAttempts.foreach(_.failed.get.printStackTrace())
    throw new Exception(s"Unable to locate Python library, tried ${pythonLibrariesToTry.mkString(", ")}")
  }

  val scope = ResourceScope.globalScope()

  def typeToMemoryLayout(cls: Class[_]): MemoryLayout = cls.getSimpleName match {
    case "int" | "Int" => CLinker.C_INT
    case "long" | "Long" => CLinker.C_LONG
    case "NativeLong" => CLinker.C_LONG_LONG
    case "float" | "Float" => CLinker.C_FLOAT
    case "char" | "Char" => CLinker.C_CHAR
    case "double" | "Double" => CLinker.C_DOUBLE
    case "short" | "Short" => CLinker.C_SHORT
    case _ => CLinker.C_POINTER
  }

  def scalaTypeToCType(cls: Class[_]): Class[_] = cls.getSimpleName match {
    case "Pointer" => classOf[MemoryAddress]
    case "PointerToPointer" => classOf[MemoryAddress]
    case "String" => classOf[MemoryAddress]
    case "WString" => classOf[MemoryAddress]
    case "NativeLong" => classOf[Long]
    case _ => cls
  }

  val downcallHandleMap: Map[String, MethodHandle] = {
    val pythonLookup = SymbolLookup.loaderLookup()
    val pythonFunctions = this.getClass.getMethods.filter(_.getName.contains("Py"))
    pythonFunctions.map {
      pyFunc => {
        val pythonFunction = pythonLookup.lookup(pyFunc.getName).get()
        val methodType = MethodType.methodType(scalaTypeToCType(pyFunc.getReturnType), pyFunc.getParameterTypes.map(scalaTypeToCType))

        val initFunctionDescriptor =
          if (pyFunc.getReturnType == classOf[Unit]) {
            FunctionDescriptor.ofVoid()
          } else {
            FunctionDescriptor.of(typeToMemoryLayout(pyFunc.getReturnType))
          }

        val functionDescriptor = pyFunc.getParameterTypes.
          foldLeft(initFunctionDescriptor)((fnDsc, tpe) => fnDsc.withAppendedArgumentLayouts(typeToMemoryLayout(tpe)))

        (pyFunc.getName, CLinker.getInstance().downcallHandle(pythonFunction, SegmentAllocator.arenaAllocator(scope), methodType, functionDescriptor))
      }
    }.toMap
  }

  def pointerToMemoryAddress(ptr: Platform.Pointer): MemoryAddress = MemoryAddress.ofLong(Pointer.nativeValue(ptr))

  def memoryAddressToPointer(mem: MemoryAddress): Platform.Pointer = new Platform.Pointer(mem.toRawLongValue)

  def pointerToUnitFunction(name: String, p: Platform.Pointer): Unit =
    downcallHandleMap(name).invoke(pointerToMemoryAddress(p))

  def pointerToPointerFunction(name: String, p: Platform.Pointer): Platform.Pointer = {
    val ptr = downcallHandleMap(name).invoke(pointerToMemoryAddress(p)).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def twoPointersToPointerFunction(name: String, p1: Platform.Pointer, p2: Platform.Pointer): Platform.Pointer = {
    val ptr = downcallHandleMap(name).invoke(pointerToMemoryAddress(p1), pointerToMemoryAddress(p2)).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def voidToPointerFunction(name: String): Platform.Pointer =
    Pointer.createConstant(downcallHandleMap(name).invoke().asInstanceOf[MemoryAddress].toRawLongValue)


  def Py_SetProgramName(str: WString): Unit = {
    val cString = CLinker.toCString(str.toString, scope)
    downcallHandleMap("Py_SetProgramName").invoke(cString.address())
  }

  def Py_Initialize(): Unit = downcallHandleMap("Py_Initialize").invoke()

  def Py_DecodeLocale(str: String, size: Platform.Pointer): WString = {
    val cString = CLinker.toCString(str, scope)
    val sizePtr = pointerToMemoryAddress(size)
    val ret = downcallHandleMap("Py_DecodeLocale").invoke(cString.address(), sizePtr).asInstanceOf[MemorySegment]
    new WString(ret.toString)
  }

  def PyMem_RawFree(p: Platform.Pointer): Unit = pointerToUnitFunction("PyMem_RawFree", p)

  def PyEval_SaveThread(): Platform.Pointer = voidToPointerFunction("PyEval_SaveThread")

  def PyGILState_Ensure(): Int = downcallHandleMap("PyGILState_Ensure").invoke().asInstanceOf[Int]
  def PyGILState_Release(state: Int): Unit = downcallHandleMap("PyGILState_Release").invoke(state)

  def PyRun_String(str: String, start: Int, globals: Platform.Pointer, locals: Platform.Pointer): Platform.Pointer = {
    val cString = CLinker.toCString(str, scope)
    val globalsNative = pointerToMemoryAddress(globals)
    val localsNative = pointerToMemoryAddress(locals)
    val ptr = downcallHandleMap("PyRun_String").invoke(cString.address(), start, globalsNative, localsNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def PyUnicode_FromString(cStr: String): Platform.Pointer = {
    val cString = CLinker.toCString(cStr, scope)
    val ptr = downcallHandleMap("PyUnicode_FromString").invoke(cString.address()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyUnicode_AsUTF8(pyString: Platform.Pointer): Platform.Pointer = pointerToPointerFunction("PyUnicode_AsUTF8", pyString)

  def PyBool_FromLong(long: NativeLong): Platform.Pointer = {
    val ptr = downcallHandleMap("PyBool_FromLong").invoke(long.longValue()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def PyNumber_Negative(o1: Platform.Pointer): Platform.Pointer = pointerToPointerFunction("PyNumber_Negative", o1)
  def PyNumber_Positive(o1: Platform.Pointer): Platform.Pointer = pointerToPointerFunction("PyNumber_Positive", o1)
  def PyNumber_Add(o1: Platform.Pointer, o2: Platform.Pointer): Platform.Pointer = twoPointersToPointerFunction("PyNumber_Add", o1, o2)
  def PyNumber_Subtract(o1: Platform.Pointer, o2: Platform.Pointer): Platform.Pointer = twoPointersToPointerFunction("PyNumber_Subtract", o1, o2)
  def PyNumber_Multiply(o1: Platform.Pointer, o2: Platform.Pointer): Platform.Pointer = twoPointersToPointerFunction("PyNumber_Multiply", o1, o2)
  def PyNumber_TrueDivide(o1: Platform.Pointer, o2: Platform.Pointer): Platform.Pointer = twoPointersToPointerFunction("PyNumber_TrueDivide", o1, o2)
  def PyNumber_Remainder(o1: Platform.Pointer, o2: Platform.Pointer): Platform.Pointer = twoPointersToPointerFunction("PyNumber_Remainder", o1, o2)

  def PyLong_FromLongLong(long: Long): Platform.Pointer = {
    val ptr = downcallHandleMap("PyLong_FromLongLong").invoke(long).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyLong_AsLong(pyLong: Platform.Pointer): Int =
    downcallHandleMap("PyLong_AsLong").invoke(pointerToMemoryAddress(pyLong)).asInstanceOf[Int]

  def PyLong_AsLongLong(pyLong: Platform.Pointer): Long =
    downcallHandleMap("PyLong_AsLongLong").invoke(pointerToMemoryAddress(pyLong)).asInstanceOf[Long]

  def PyFloat_FromDouble(double: Double): Platform.Pointer = {
    val ptr = downcallHandleMap("PyFloat_FromDouble").invoke(double).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyFloat_AsDouble(float: Platform.Pointer): Double =
    downcallHandleMap("PyFloat_AsDouble").invoke(pointerToMemoryAddress(float)).asInstanceOf[Double]

  def PyDict_New(): Platform.Pointer =
    voidToPointerFunction("PyDict_New")
  def PyDict_SetItem(dict: Platform.Pointer, key: Platform.Pointer, value: Platform.Pointer): Int = {
    val dictNative = pointerToMemoryAddress(dict)
    val keyNative = pointerToMemoryAddress(key)
    val valueNative = pointerToMemoryAddress(value)
    downcallHandleMap("PyDict_SetItem").invoke(dictNative, keyNative, valueNative).asInstanceOf[Int]
  }
  def PyDict_SetItemString(dict: Platform.Pointer, key: String, value: Platform.Pointer): Int = {
    val dictNative = pointerToMemoryAddress(dict)
    val keyString = CLinker.toCString(key, scope)
    val valueNative = pointerToMemoryAddress(value)
    downcallHandleMap("PyDict_SetItemString").invoke(dictNative, keyString.address(), valueNative).asInstanceOf[Int]
  }
  def PyDict_Contains(dict: Platform.Pointer, key: Platform.Pointer): Int = {
    val dictNative = pointerToMemoryAddress(dict)
    val keyNative = pointerToMemoryAddress(key)
    downcallHandleMap("PyDict_Contains").invoke(dictNative, keyNative).asInstanceOf[Int]
  }
  def PyDict_GetItem(dict: Platform.Pointer, key: Platform.Pointer): Platform.Pointer =
    twoPointersToPointerFunction("PyDict_GetItem", dict, key)
  def PyDict_GetItemString(dict: Platform.Pointer, key: String): Platform.Pointer = {
    val dictNative = pointerToMemoryAddress(dict)
    val keyString = CLinker.toCString(key, scope)
    val ptr = downcallHandleMap("PyDict_GetItemString").invoke(dictNative, keyString.address()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  @scala.native def PyDict_GetItemWithError(dict: Platform.Pointer, key: Platform.Pointer): Platform.Pointer/* =
    twoPointersToPointerFunction("PyDict_GetItemWithError", dict, key)*/
  def PyDict_DelItemString(dict: Platform.Pointer, key: String): Int = {
    val dictNative = pointerToMemoryAddress(dict)
    val keyString = CLinker.toCString(key, scope)
    downcallHandleMap("PyDict_DelItemString").invoke(dictNative, keyString.address()).asInstanceOf[Int]
  }
  def PyDict_Keys(dict: Platform.Pointer): Platform.Pointer = pointerToPointerFunction("PyDict_Keys", dict)

  def PyList_New(size: Int): Platform.Pointer = {
    val ptr = downcallHandleMap("PyList_New").invoke(size).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyList_Size(list: Platform.Pointer): NativeLong = {
    val listNative = pointerToMemoryAddress(list)
    new NativeLong(downcallHandleMap("PyList_Size").invoke(listNative).asInstanceOf[Long])
  }
  def PyList_GetItem(list: Platform.Pointer, index: NativeLong): Platform.Pointer = {
    val listNative = pointerToMemoryAddress(list)
    val ptr = downcallHandleMap("PyList_GetItem").invoke(listNative, index.longValue()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyList_SetItem(list: Platform.Pointer, index: NativeLong, item: Platform.Pointer): Int = {
    val listNative = pointerToMemoryAddress(list)
    val itemNative = pointerToMemoryAddress(item)
    downcallHandleMap("PyList_SetItem").invoke(listNative, index.longValue(), itemNative).asInstanceOf[Int]
  }

  def PyTuple_New(size: Int): Platform.Pointer = {
    val ptr = downcallHandleMap("PyTuple_New").invoke(size).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyTuple_Size(list: Platform.Pointer): NativeLong = {
    val listNative = pointerToMemoryAddress(list)
    new NativeLong(downcallHandleMap("PyTuple_Size").invoke(listNative).asInstanceOf[Long])
  }
  def PyTuple_GetItem(list: Platform.Pointer, index: NativeLong): Platform.Pointer = {
    val listNative = pointerToMemoryAddress(list)
    val ptr = downcallHandleMap("PyTuple_GetItem").invoke(listNative, index.longValue()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyTuple_SetItem(list: Platform.Pointer, index: NativeLong, item: Platform.Pointer): Int = {
    val listNative = pointerToMemoryAddress(list)
    val itemNative = pointerToMemoryAddress(item)
    downcallHandleMap("PyTuple_SetItem").invoke(listNative, index.longValue(), itemNative).asInstanceOf[Int]
  }

  def PyObject_Str(obj: Platform.Pointer): Platform.Pointer =
    pointerToPointerFunction("PyObject_Str", obj)
  def PyObject_GetItem(obj: Platform.Pointer, idx: Platform.Pointer): Platform.Pointer =
    twoPointersToPointerFunction("PyObject_GetItem", obj, idx)
  def PyObject_SetItem(obj: Platform.Pointer, key: Platform.Pointer, newValue: Platform.Pointer): Int = {
    val objNative = pointerToMemoryAddress(obj)
    val keyNative = pointerToMemoryAddress(key)
    val valueNative = pointerToMemoryAddress(newValue)
    downcallHandleMap("PyObject_SetItem").invoke(objNative, keyNative, valueNative).asInstanceOf[Int]
  }
  def PyObject_DelItem(obj: Platform.Pointer, idx: Platform.Pointer): Int = {
    val dictNative = pointerToMemoryAddress(obj)
    val keyNative = pointerToMemoryAddress(idx)
    downcallHandleMap("PyObject_DelItem").invoke(dictNative, keyNative).asInstanceOf[Int]
  }
  def PyObject_GetAttr(obj: Platform.Pointer, name: Platform.Pointer): Platform.Pointer =
    twoPointersToPointerFunction("PyObject_GetAttr", obj, name)
  def PyObject_GetAttrString(obj: Platform.Pointer, name: String): Platform.Pointer = {
    val dictNative = pointerToMemoryAddress(obj)
    val nameString = CLinker.toCString(name, scope)
    val ptr = downcallHandleMap("PyObject_GetAttrString").invoke(dictNative, nameString.address()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyObject_SetAttr(obj: Platform.Pointer, name: Platform.Pointer, newValue: Platform.Pointer): Platform.Pointer = {
    val objNative = pointerToMemoryAddress(obj)
    val nameNative = pointerToMemoryAddress(name)
    val valueNative = pointerToMemoryAddress(newValue)
    val ptr = downcallHandleMap("PyObject_SetAttr").invoke(objNative, nameNative, valueNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyObject_SetAttrString(obj: Platform.Pointer, name: String, newValue: Platform.Pointer): Platform.Pointer = {
    val objNative = pointerToMemoryAddress(obj)
    val nameString = CLinker.toCString(name, scope)
    val valueNative = pointerToMemoryAddress(newValue)
    val ptr = downcallHandleMap("PyObject_SetAttrString").invoke(objNative, nameString.address(), valueNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyObject_Call(obj: Platform.Pointer, args: Platform.Pointer, kwArgs: Platform.Pointer): Platform.Pointer = {
    val objNative = pointerToMemoryAddress(obj)
    val argsNative = pointerToMemoryAddress(args)
    val kwArgsNative = pointerToMemoryAddress(kwArgs)
    val ptr = downcallHandleMap("PyObject_Call").invoke(objNative, argsNative, kwArgsNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PyObject_Length(obj: Platform.Pointer): NativeLong = {
    val objNative = pointerToMemoryAddress(obj)
    new NativeLong(downcallHandleMap("PyObject_Length").invoke(objNative).asInstanceOf[Long])
  }

  def PySequence_GetItem(obj: Platform.Pointer, idx: Int): Platform.Pointer = {
    val objNative = pointerToMemoryAddress(obj)
    val ptr = downcallHandleMap("PySequence_GetItem").invoke(objNative, idx).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PySequence_SetItem(obj: Platform.Pointer, idx: Int, v: Platform.Pointer): Platform.Pointer = {
    val objNative = pointerToMemoryAddress(obj)
    val vNative = pointerToMemoryAddress(v)
    val ptr = downcallHandleMap("PySequence_SetItem").invoke(objNative, idx, vNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }
  def PySequence_Length(obj: Platform.Pointer): NativeLong = {
    val objNative = pointerToMemoryAddress(obj)
    val length = downcallHandleMap("PySequence_Length").invoke(objNative).asInstanceOf[Long]
    new NativeLong(length)
  }

  def PyErr_Occurred(): Platform.Pointer = voidToPointerFunction("PyErr_Occurred")
  def PyErr_Fetch(pType: Platform.PointerToPointer, pValue: Platform.PointerToPointer, pTraceback: Platform.PointerToPointer): Unit = {
    val pTypeNative = pointerToMemoryAddress(pType)
    val pValueNative = pointerToMemoryAddress(pValue)
    val pTracebackNative = pointerToMemoryAddress(pTraceback)
    downcallHandleMap("PyErr_Fetch").invoke(pTypeNative, pValueNative, pTracebackNative)
  }
  def PyErr_Print(): Unit = downcallHandleMap("PyErr_Print").invoke()
  def PyErr_Clear(): Unit = downcallHandleMap("PyErr_Clear").invoke()

  def PyEval_GetBuiltins(): Platform.Pointer = voidToPointerFunction("PyEval_GetBuiltins")

  def Py_BuildValue(str: String): Platform.Pointer = {
    val cString = CLinker.toCString(str, scope)
    val ptr = downcallHandleMap("Py_BuildValue").invoke(cString.address()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def PyLong_FromVoidPtr(ptr: Platform.Pointer): Unit = pointerToUnitFunction("PyLong_FromVoidPtr", ptr)
  def PyCFunction_NewEx(ptr: Platform.Pointer, self: Platform.Pointer, module: Platform.Pointer): Platform.Pointer = {
    val ptrNative = pointerToMemoryAddress(ptr)
    val selfNative = pointerToMemoryAddress(self)
    val moduleNative = pointerToMemoryAddress(module)
    val ptrResult = downcallHandleMap("PyCFunction_NewEx").invoke(ptrNative, selfNative, moduleNative).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptrResult)
  }
  def PyImport_ImportModule(str: String): Platform.Pointer = {
    val cString = CLinker.toCString(str, scope)
    val ptr = downcallHandleMap("PyImport_ImportModule").invoke(cString.address()).asInstanceOf[MemoryAddress]
    memoryAddressToPointer(ptr)
  }

  def PyErr_SetString(tpe: Platform.Pointer, message: String): Unit = {
    val tpeNative = pointerToMemoryAddress(tpe)
    val cString = CLinker.toCString(message, scope)
    downcallHandleMap("PyErr_SetString").invoke(tpeNative, cString.address())
  }

  def Py_IncRef(ptr: Platform.Pointer): Unit = pointerToUnitFunction("Py_IncRef", ptr)
  def Py_DecRef(ptr: Platform.Pointer): Unit = pointerToUnitFunction("Py_DecRef", ptr)
}

object CPythonAPI extends CPythonAPIInterface
