/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
var x1: example.profiling.HelloProfileRunner$@56d90453.type#scala.virtualization.lms.internal.Expressions$Variable[Double] = 0.0
val x8 = {
val out = new ProfileArray(100)
var i = 0
while (i < 100) {
  val start = System.currentTimeMillis()
var x3 : Int = 0
val x6 = while (x3 < 100000) {
x1 = 0
()
x3 = x3 + 1
}
  val end = System.currentTimeMillis()
  val duration = (end - start)/1000f 
  out.dcUpdate(i, duration)
  i += 1
}
out
}
val x9 = x8.dcSize
val x15_zero = {
0.0
}
var x15: Double = x15_zero
var x10 = 0
while (x10 < x9) {  // begin fat loop x15
val x11 = x8.dcApply(x10)
val x12 = x15
val x13 = x11
val x14 = x12 + x13
x15 = x14
x10 += 1
} // end fat loop x15
val x16 = x8.size
val x17 = x16
val x18 = x15 / x17
val x19 = "average loop time "+x18
val x20 = println(x19)
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
