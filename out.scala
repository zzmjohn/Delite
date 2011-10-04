/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = new ppl.delite.framework.collections.datastruct.scala.ArraySeqImpl[Int](5)
val x2 = x1.size
val x3 = "hello array: "+x2
val x4 = println(x3)
val x5 = println("Now printing in foreach.")
// a *thin* loop follows: x6
var x6: Unit = ()
var x16 = 0
while (x16 < x2) {  // begin fat loop x6
val x17 = x1.dcApply(x16)
val x18 = println(x17)
x6 = {
x18
}
x16 += 1
} // end fat loop x6
val x7 = println("Now mapping and printing in foreach.")
val x9 = new ppl.delite.framework.collections.datastruct.scala.ArraySeqImpl[Int](x2)
val x12 = {
x9
}
var x8 = 0
while (x8 < x2) {  // begin fat loop x12
val x10 = x1.dcApply(x8)
val x11 = x10 + 1
x12.dcUpdate(x8, x11)
x8 += 1
} // end fat loop x12
val x13 = x12.size
// a *thin* loop follows: x14
var x14: Unit = ()
var x21 = 0
while (x21 < x13) {  // begin fat loop x14
val x22 = x12.dcApply(x21)
val x23 = println(x22)
x14 = {
x23
}
x21 += 1
} // end fat loop x14
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
