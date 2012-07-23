package ppl.dsl.optila

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer}

trait OptiLACppHostTransfer extends CppHostTransfer {

  val IR: Expressions
  import IR._

  override def emitSend(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseVector\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(IZ)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->length,%s->isRow);\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->FindClass(\"generated/scala/%sDenseMatrix\");\n".format(typeArg.toString))
            out.append("\tjmethodID mid = env->GetMethodID(cls,\"<init>\",\"(II)V\");\n")
            out.append("\tjobject obj = env->NewObject(cls,mid,%s->numRows,%s->numCols);\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn obj;\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("assert(false);")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSend(sym, host)
        }
    }
    else
      super.emitSend(sym, host)
  }


  override def emitRecv(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          
          /* SoA */          
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            val jniTypeDesc = "Lscala/Tuple3"
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()%s\");\n".format("Ljava/lang/Object;"))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format("jobject","jobject"))
            //out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tfloat* tuples = new float[3*%s->numRows*%s->numCols];\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_unbox = env->GetMethodID(env->FindClass(\"java/lang/Float\"), \"floatValue\", \"()F\");\n")
            // SoA'd -- each row of the input from scala fills 3 rows of the output in c
            out.append("\tfor(int i=0; i<%s->numRows; i++) {\n".format(quote(sym),quote(sym)))
            out.append("\t\tfor(int j=0; j<%s->numCols; j++) {\n".format(quote(sym),quote(sym)))
            out.append("\t\t\tint rawIdx = i*%s->numCols+j;\n".format(quote(sym)))
            out.append("\t\t\tjobject data_elem = env->GetObjectArrayElement(data,rawIdx);\n")
            out.append("\t\t\tjmethodID mid_tup1 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_1\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\t\tjobject elem1_boxed = (jobject) env->CallObjectMethod(data_elem, mid_tup1);\n")
            out.append("\t\t\tfloat elem1 = (float) env->CallFloatMethod(elem1_boxed, mid_unbox);\n")
            out.append("\t\t\tjmethodID mid_tup2 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_2\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\t\tjobject elem2_boxed = (jobject) env->CallObjectMethod(data_elem, mid_tup2);\n")
            out.append("\t\t\tfloat elem2 = (float) env->CallFloatMethod(elem2_boxed, mid_unbox);\n")
            out.append("\t\t\tjmethodID mid_tup3 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_3\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\t\tjobject elem3_boxed = (jobject) env->CallObjectMethod(data_elem, mid_tup3);\n")
            out.append("\t\t\tfloat elem3 = (float) env->CallFloatMethod(elem3_boxed, mid_unbox);\n")
            out.append("\t\t\ttuples[i*3*%s->numCols+j] = elem1;\n".format(quote(sym)))
            out.append("\t\t\ttuples[i*3*%s->numCols+%s->numCols+j] = elem2;\n".format(quote(sym),quote(sym)))
            out.append("\t\t\ttuples[i*3*%s->numCols+%s->numCols*2+j] = elem3;\n".format(quote(sym),quote(sym)))
            out.append("\t\t\t%s->data[i*%s->numCols+j] = &tuples[i*3*%s->numCols+j];\n".format(quote(sym),quote(sym),quote(sym))) 
            out.append("\t\t\tenv->DeleteLocalRef(elem1_boxed);\n")
            out.append("\t\t\tenv->DeleteLocalRef(elem2_boxed);\n")
            out.append("\t\t\tenv->DeleteLocalRef(elem3_boxed);\n")
            out.append("\t\t\tenv->DeleteLocalRef(data_elem);\n")
            out.append("\t\t}\n")
            out.append("\t}\n")
            //out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0)z;\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)

          /* AoS */
          /* 
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\t%s *%s = new %s(env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp)))
            val jniTypeDesc = "Lscala/Tuple3"
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()%s\");\n".format("Ljava/lang/Object;"))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format("jobject","jobject"))
            //out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tfloat* tuples = new float[3*%s->numRows*%s->numCols];\n".format(quote(sym),quote(sym)))
            out.append("\tjmethodID mid_unbox = env->GetMethodID(env->FindClass(\"java/lang/Float\"), \"floatValue\", \"()F\");\n")
            out.append("\tfor(int i=0; i<%s->numRows*%s->numCols; i++) {\n".format(quote(sym),quote(sym)))
            out.append("\t\tjmethodID mid_tup1 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_1\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\tjobject elem1_boxed = (jobject) env->CallObjectMethod(env->GetObjectArrayElement(data,i), mid_tup1);\n")
            out.append("\t\tfloat elem1 = (float) env->CallFloatMethod(elem1_boxed, mid_unbox);\n")
            out.append("\t\ttuples[3*i] = elem1;\n")
            out.append("\t\tjmethodID mid_tup2 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_2\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\tjobject elem2_boxed = (jobject) env->CallObjectMethod(env->GetObjectArrayElement(data,i), mid_tup2);\n")
            out.append("\t\tfloat elem2 = (float) env->CallFloatMethod(elem2_boxed, mid_unbox);\n")
            out.append("\t\ttuples[3*i+1] = elem2;\n")
            out.append("\t\tjmethodID mid_tup3 = env->GetMethodID(env->FindClass(\"scala/Tuple3\"), \"_3\", \"()Ljava/lang/Object;\");\n")
            out.append("\t\tjobject elem3_boxed = (jobject) env->CallObjectMethod(env->GetObjectArrayElement(data,i), mid_tup3);\n")
            out.append("\t\tfloat elem3 = (float) env->CallFloatMethod(elem3_boxed, mid_unbox);\n")
            out.append("\t\ttuples[3*i+2] = elem3;\n")
            out.append("\t\t%s->data[i] = &tuples[3*i];\n".format(quote(sym)))
            out.append("\t}\n")
            //out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0)z;\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
            */

          case _ => 
            //println("emitRecv no match for " + sym.tp)
            //println("remap returned: " + remap(sym.tp))
            super.emitRecv(sym, host)
        }
    }
    else
      super.emitRecv(sym, host)
  }

  override def emitSendView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" |
          "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tassert(false);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("assert(false);")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendView(sym, host)
        }
    }
    else
      super.emitSendView(sym, host)
  }


  override def emitRecvView(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_length = env->GetMethodID(cls,\"_length\",\"()I\");\n")
            out.append("\tjmethodID mid_isRow = env->GetMethodID(cls,\"_isRow\",\"()Z\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *%s = new %s((%s *)dataPtr,env->CallIntMethod(obj,mid_length),env->CallBooleanMethod(obj,mid_isRow));\n".format(remap(sym.tp),quote(sym),remap(sym.tp),remap(typeArg)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_numRows = env->GetMethodID(cls,\"_numRows\",\"()I\");\n")
            out.append("\tjmethodID mid_numCols = env->GetMethodID(cls,\"_numCols\",\"()I\");\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *%s = new %s((%s *)dataPtr,env->CallIntMethod(obj,mid_numRows),env->CallBooleanMethod(obj,mid_numCols));\n".format(remap(sym.tp),quote(sym),remap(sym.tp),remap(typeArg)))
            out.append("\tenv->DeleteLocalRef(data);\n")   //TODO: This should not be done at this point?
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("\treturn %s;\n".format(quote(sym)))
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("assert(false);")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitRecvView(sym, host)
        }
    }
    else
      super.emitRecvView(sym, host)
  }

  override def emitSendUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(dataPtr, %s->data, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("assert(false);")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(sym, host)
        }
    }
    else
      super.emitSendUpdate(sym, host)
  }

  override def emitRecvUpdate(sym: Sym[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
        remap(sym.tp) match {
          case "DenseVector< bool >" | "DenseVector< char >" | "DenseVector< CHAR >" | "DenseVector< short >" | "DenseVector< int >" | "DenseVector< long >" | "DenseVector< float >" | "DenseVector< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->length*sizeof(%s));\n".format(quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< bool >" | "DenseMatrix< char >" | "DenseMatrix< CHAR >" | "DenseMatrix< short >" | "DenseMatrix< int >" | "DenseMatrix< long >" | "DenseMatrix< float >" | "DenseMatrix< double >" =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *%s)".format(quote(sym),remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("\tjclass cls = env->GetObjectClass(obj);\n")
            out.append("\tjmethodID mid_data = env->GetMethodID(cls,\"_data\",\"()[%s\");\n".format(JNITypeDescriptor(typeArg)))
            out.append("\t%sArray data = (%sArray)(env->CallObjectMethod(obj,mid_data));\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\t%s *dataPtr = (%s *)env->GetPrimitiveArrayCritical(data,0);\n".format(JNIType(typeArg),JNIType(typeArg)))
            out.append("\tmemcpy(%s->data, dataPtr, %s->numRows*%s->numCols*sizeof(%s));\n".format(quote(sym),quote(sym),quote(sym),remap(typeArg)))
            out.append("\tenv->ReleasePrimitiveArrayCritical(data, dataPtr, 0);\n")
            out.append("\tenv->DeleteLocalRef(data);\n")
            out.append("\tenv->DeleteLocalRef(cls);\n")
            out.append("}\n")
            (signature+";\n", out.toString)
          case "DenseMatrix< float* >" if (sym.tp.typeArguments(0).erasure.getSimpleName == "Tuple3") =>
            val out = new StringBuilder
            val typeArg = sym.tp.typeArguments.head
            val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(sym.tp),quote(sym))
            out.append(signature + " {\n")
            out.append("assert(false);")
            out.append("}\n")
            (signature+";\n", out.toString)
          case _ => super.emitSendUpdate(sym, host)
        }
    }
    else
      super.emitRecvUpdate(sym, host)
  }

}
