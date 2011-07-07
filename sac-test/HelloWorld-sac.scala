import epfl.mdarrays.staged._
object HelloWorldRunner extends StagedSACApplicationRunner with HelloWorld 
trait HelloWorld extends StagedSACApplication { 
    def main() = {
      val matrix = reshape(2::2::Nil, 1::0::0::1::Nil)
      println(matrix.getString)
    }
}
