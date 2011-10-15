import epfl.mdarrays.staged._
//import epfl.mdarrays.datastruct.scala.MDArray

object TransposeRunner extends StagedSACApplicationRunner with Transpose
trait Transpose extends StagedSACApplication {
    def main() = {
      val input = readMDArray[Int]("matrix.txt", System.getProperty("Transpose.JIT", "true").toLowerCase == "true")
      startTimer(input::Nil)
      val output = With(function = iv => input(iv(1::Nil)::iv(0::Nil)::Nil)).GenArray(shape(input))
      stopTimer(output::Nil)
      //writeMDArray("output.txt", output)
    }
}
