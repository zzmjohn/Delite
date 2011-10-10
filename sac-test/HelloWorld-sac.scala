import epfl.mdarrays.staged._
//import epfl.mdarrays.datastruct.scala.MDArray

object HelloWorldRunner extends StagedSACApplicationRunner with HelloWorld 
trait HelloWorld extends StagedSACApplication {
    def main() = {
      //val matrix = reshape(2::2::Nil, 1::0::0::1::Nil)
      val matrix2 = readMDArray[Int]("/home/sun/sac-test/mdarray.txt", true)
      //println(matrix.getString)
      println(matrix2.getString)
      //writeMDArray("/home/sun/sac-test/mdarray2.txt", matrix)
      //writeMDArray("/home/sun/sac-test/mdarray3.txt", matrix2)
    }
}
