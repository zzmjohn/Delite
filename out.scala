/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = new generated.scala.ArraySeqImpl[Int](6)
val x2 = x1.size
val x3 = "hello array: "+x2
val x4 = println(x3)
val x5 = println("Now printing in foreach.")
// a *thin* loop follows: x6
var x6: Unit = ()
var x27 = 0
while (x27 < x2) {  // begin fat loop x6
val x28 = x1.dcApply(x27)
val x29 = println(x28)
x6 = {
x29
}
x27 += 1
} // end fat loop x6
val x7 = println("Now mapping and printing in foreach.")
val x9 = new generated.scala.ArraySeqImpl[Int](x2)
// a *thin* loop follows: x8
val x8 = {
x9
}
var x10 = 0
while (x10 < x2) {  // begin fat loop x8
val x11 = x1.dcApply(x10)
val x12 = x11 + 1
x8.dcUpdate(x10, x12)
x10 += 1
} // end fat loop x8
val x13 = x8.size
// a *thin* loop follows: x14
var x14: Unit = ()
var x32 = 0
while (x32 < x13) {  // begin fat loop x14
val x33 = x8.dcApply(x32)
val x34 = println(x33)
x14 = {
x34
}
x32 += 1
} // end fat loop x14
val x15 = println("Now some filtering.")
val x17 = new generated.scala.ArraySeqImpl[Int](x13)
// a *thin* loop follows: x16
val x16 = {
//TODO: buffer size might be wrong (loop has conditions)
x17
}
var x18 = 0
while (x18 < x13) {  // begin fat loop x16
val x19 = x8.dcApply(x18)
val x20 = x19 % 2
val x21 = x20==1
if (x21) x16.+=(x19)
x18 += 1
} // end fat loop x16
val x22 = x16.size
val x23 = "Elements left after filter: "+x22
val x24 = println(x23)
// a *thin* loop follows: x25
var x25: Unit = ()
var x37 = 0
while (x37 < x22) {  // begin fat loop x25
val x38 = x16.dcApply(x37)
val x39 = println(x38)
x25 = {
x39
}
x37 += 1
} // end fat loop x25
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
