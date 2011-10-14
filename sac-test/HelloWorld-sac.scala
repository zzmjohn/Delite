import epfl.mdarrays.staged._
//import epfl.mdarrays.datastruct.scala.MDArray

object HelloWorldRunner extends StagedSACApplicationRunner with HelloWorld 
trait HelloWorld extends StagedSACApplication {
    def main() = {
      val matrix = 1::0::0::1::Nil
      //val matrix2 = readMDArray[Int]("./mdarray.txt", true)
      //val matrix3 = With(function = iv => matrix2(iv) + iv(0::Nil)).GenArray(shape(matrix2))
      //println(matrix.getString)
      //println(matrix3.getString)
      //writeMDArray("/home/sun/sac-test/mdarray2.txt", matrix)
      //writeMDArray("/home/sun/sac-test/mdarray3.txt", matrix2)
    }
}
