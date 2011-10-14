import epfl.mdarrays.staged._
//import epfl.mdarrays.datastruct.scala.MDArray

object TransposeRunner extends StagedSACApplicationRunner with Transpose
trait Transpose extends StagedSACApplication {
    def main() = {
      val input = readMDArray[Int]("matrix.txt", true)
      val output = With(function = iv => input(cat(0, reshape(1::Nil,iv(1::Nil)), reshape(1::Nil,iv(0::Nil))))).GenArray(shape(input))
      writeMDArray("output.txt", output)
    }
}
