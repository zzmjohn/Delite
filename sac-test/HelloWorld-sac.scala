import epfl.mdarrays.staged._
import epfl.mdarrays.library.scala.MDArray // TODO: Get rid of this interweaving

object HelloWorldRunner extends StagedSACApplicationRunner with HelloWorld 
trait HelloWorld extends StagedSACApplication {
    def main() = {
      val matrix: Rep[MDArray[Int]] = 1::0::0::1::Nil
      println(matrix.getString)
      val matrix2 = readMDArray[Int]("./mdarray.txt", true)
      //val matrix3 = With(function = iv => matrix2(iv) + iv(0::Nil)).GenArray(shape(matrix2))
      //println(matrix.getString)
      //println(matrix3.getString)
      //writeMDArray("/home/sun/sac-test/mdarray2.txt", matrix)
      println(matrix2.getString)
      writeMDArray("mdarray3.txt", matrix2)
      val matrix3 = values(5,1)
      val matrix4 = values(5,3)
      println((matrix3 + matrix4).getString)
      println((matrix3 + 1).getString)
      val matrix5:Rep[MDArray[Int]] = matrix3::matrix4::Nil
      println(matrix5.getString)
    }
}
