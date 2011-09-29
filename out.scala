/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
var x1: example.profiling.HelloProfileRunner$@9ac5f13.type#scala.virtualization.lms.internal.Expressions$Variable[Double] = 0.0
var x3 : Int = 0
val x11 = while (x3 < 100000) {
val x4 = x3
val x5 = java.lang.Math.exp(x4)
val x6 = java.lang.Math.pow(x4,10.0)
val x7 = x5 * x6
val x8 = x7 * 42.0
val x9 = x1 += x8
()
x3 = x3 + 1
}
val x13 = {
val out = new ProfileArray(100)
var i = 0
while (i < 100) {
  val start = System.currentTimeMillis()
  val end = System.currentTimeMillis()
  val duration = (end - start)/1000f 
  out.dcUpdate(i, duration)
  i += 1
}
out
}
val x14 = x13.dcSize
val x20_zero = {
0.0
}
var x20: Double = x20_zero
var x15 = 0
while (x15 < x14) {  // begin fat loop x20
val x16 = x13.dcApply(x15)
val x17 = x20
val x18 = x16
val x19 = x17 + x18
x20 = x19
x15 += 1
} // end fat loop x20
val x21 = x13.size
val x22 = x21
val x23 = x20 / x22
val x24 = "average loop time "+x23
val x25 = println(x24)
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
