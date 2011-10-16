package epfl.mdarrays.datastruct.scala

import scala.util.control.Breaks._

/**
 * Very limited runtime support for the SAC-staged DSL
 */
object MDArrayRuntimeSupport {

  def prod(a: Array[Int]) = {
    var i = 0
    var r = 1
    while (i < a.length) {
      r *= a(i)
      i += 1
    }
    r
  }

  def flatten(iv: Array[Int], shape: Array[Int]) = {
    if (iv.length == 0) {
      0
    } else {
      var i = 1
      var r = iv(0)
      while (i < iv.length) {
        r *= shape(i-1)
        r += iv(i)
        i += 1
      }
      r
    }
  }

  def flattenGenArray(shp1: Array[Int], shp2: Array[Int], iv: Array[Int]) = {
    if (iv.length == 0) {
      0
    } else {
      var i = 1
      var r = iv(0)
      while (i < iv.length) {
        r *= shp1(i)
        r += iv(i)
        i += 1
      }
      r * prod(shp2)
    }
  }

  def flattenModArray(shape: Array[Int], iv: Array[Int]) = {
    if (iv.length == 0) {
      0
    } else {
      var i = 1
      var r = iv(0)
      while (i < iv.length) {
        r *= shape(i-1)
        r += iv(i)
        i += 1
      }
      while (i < shape.length) {
        r *= shape(i-1)
        i += 1
      }
      r
    }
  }


  def getString[@specialized A](shape: Array[Int], value: Array[A]): String = {
    if (shape.length == 0)
      "Scalar: " + value(0)
    else if (shape.length == 1)
      "Vector(" + value.length + "):" + value.mkString(" ")
    else {
      val sb: StringBuffer = new StringBuffer()
      sb.append("Array(")
      sb.append(shape.mkString(" "))
      sb.append("):")

      var i = 0
      var j = 0
      var k = 0

      i = 0
      while (i < shape(0)) {
        sb.append("\n")
        j = 0
        while (j < shape(1)) {
          sb.append("<")
          val bp = i * (value.length / shape(0)) + j * (value.length / shape(0) / shape(1))
          k = 0
          while (k < (value.length / shape(0) / shape(1))) {
            if (k != 0) sb.append(" ")
            sb.append(value(bp + k))
            k += 1
          }
          sb.append(">  ")
          j += 1
        }
        i += 1
      }

      sb.toString()
    }
  }

  def readMDArrayInt(fileName: String): (Array[Int], Array[Int]) = {
    val source = scala.io.Source.fromFile(fileName)(scala.io.Codec.default)
    val lines = source.getLines
    var i = 0

    // read dimension
    if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain dimension!")
    val dimension = lines.next.toInt

    // read shape
    i = 0
    val shape: Array[Int] = new Array(dimension)
    while(i < dimension) {
      if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the entire shape!")
      shape(i) = readInt(lines.next)
      i += 1
    }

    i = 0
    val size = prod(shape)
    val data: Array[Int] = new Array(size)
    while (i < size) {
      if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the data!")
      data(i) = readInt(lines.next)
      i += 1
    }

    // close file
    source.close()

    (shape, data)
  }

  def readMDArrayDouble(fileName: String): (Array[Int], Array[Double]) = {
    val source = scala.io.Source.fromFile(fileName)(scala.io.Codec.default)
    val lines = source.getLines
    var i = 0

    // read dimension
    if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain dimension!")
    val dimension = lines.next.toInt

    // read shape
    i = 0
    val shape: Array[Int] = new Array(dimension)
    while(i < dimension) {
      if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the entire shape!")
      shape(i) = readInt(lines.next)
      i += 1
    }

    // read contents
    i = 0
    val size = prod(shape)
    val data: Array[Double] = new Array(size)
    while (i < size) {
      if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the data!")
      data(i) = readDouble(lines.next)
      i += 1
    }

    // close file
    source.close()

    (shape, data)
  }

  def readMDArrayFloat(fileName: String): (Array[Int], Array[Double]) = sys.error("Not implemented yet :(")
  def readMDArrayBoolean(fileName: String): (Array[Int], Array[Boolean]) = sys.error("Not implemented yet :(")
  def readMDArrayChar(fileName: String): (Array[Int], Array[Char]) = sys.error("Not implemented yet :(")

  def writeMDArray[@specialized A](fileName: String, shape: Array[Int], data: Array[A]) = {

    val output = new java.io.PrintStream(new java.io.FileOutputStream(fileName))
    var i = 0

    // write dimension
    output.println(shape.length)

    // write shape
    i = 0
    while (i < shape.length) {
      output.println(shape(i))
      i += 1
    }

    // write data
    i = 0
    while (i < data.length) {
      output.println(data(i))
      i += 1
    }

    // close file
    output.close()
  }

  private def readInt(line: String): Int = line.toInt
  private def readDouble(line: String): Double = line.toDouble
  private def readFloat(line: String): Float = line.toFloat
  private def readChar(line: String): Char = line(0)
  private def readBoolean(line: String): Boolean = line.toBoolean


  var timer: Long = 0
  def startTimer() = timer = System.currentTimeMillis
  def stopTimer() = println("Time elapsed: " + (System.currentTimeMillis - timer) + "ms")


  private def getNext(crt: Array[Int], ll: Array[Int], ul: Array[Int]): Array[Int] = {

    var i = crt.length - 1
    var result = crt

    breakable {
      while (i >= 0) {
        if (crt(i) <= ul(i) - 1) {
          crt(i) += 1
          while(i < crt.length - 1) {
            crt(i+1) = ll(i+1)
            i += 1
          }
          break
        }
        i -= 1
      }
      result = null
    }

    result
  }

  private def isGood(crt: Array[Int], lb: Array[Int], step: Array[Int], width: Array[Int]) = {

    var result = true
    var i = 0

    breakable {
      while (i >= 0) {
        if (((crt(i) - lb(i)) % step(i)) > width(i)) {
          result = false
          break
        }
        i -= 1
      }
    }

    result
  }

  def nextInLine(crt: Array[Int], ll: Array[Int], ul: Array[Int], lb: Array[Int], step: Array[Int], width: Array[Int]) = {
    var result = crt

    result = getNext(result, ll, ul)
    while ((result!=null) && (!isGood(result, lb, step, width)))
      result = getNext(result, ll, ul)

    result
  }
}
