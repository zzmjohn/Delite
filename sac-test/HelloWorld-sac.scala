import epfl.mdarrays.staged._
import epfl.mdarrays.library.scala.MDArray // TODO: Get rid of this interweaving

object HelloWorldRunner extends StagedSACApplicationRunner with HelloWorld 
trait HelloWorld extends StagedSACApplication {
    def main() = {
/*
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

      var i = 0
      var alive = ()
      while (i < 100) {
        alive = println("alive") //gameOfLife(alive)
        i += 1
      }
      alive
*/

      val l: Rep[MDArray[Int]] = reshape(5::5::Nil, 1::2::3::4::5::6::7::8::9::10::11::12::13::14::15::16::17::18::19::20::21::22::23::24::25::Nil)
      //println(sth.doToString)
      //println(tile(2::2::Nil, 0::0::Nil, l))
      val res = With(lbStrict=true, ubStrict=true, function=((iv)=> tile(3::3::Nil, iv-1, l))).GenArray(5::5::Nil)
      println(res)

    }
}
