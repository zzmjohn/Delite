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
var x29 = 0
while (x29 < x2) {  // begin fat loop x6
val x30 = x1.dcApply(x29)
val x31 = println(x30)
x6 = {
x31
}
x29 += 1
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
var x34 = 0
while (x34 < x13) {  // begin fat loop x14
val x35 = x8.dcApply(x34)
val x36 = println(x35)
x14 = {
x36
}
x34 += 1
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
var x39 = 0
while (x39 < x22) {  // begin fat loop x25
val x40 = x16.dcApply(x39)
val x41 = println(x40)
x25 = {
x41
}
x39 += 1
} // end fat loop x25
val x26 = println("Nested computation.")
// a *thin* loop follows: x27
var x27: Unit = ()
var x44 = 0
while (x44 < x2) {  // begin fat loop x27
val x45 = x1.dcApply(x44)
val x53 = x45 + 1
// a *thin* loop follows: x46
val x46 = {
//TODO: buffer size might be wrong (loop has conditions)
x17
}
var x50 = 0
while (x50 < x13) {  // begin fat loop x46
val x51 = x8.dcApply(x50)
val x52 = x51 % 2
val x54 = x52==x53
if (x54) x46.+=(x51)
x50 += 1
} // end fat loop x46
val x47 = println(x46)
x27 = {
x47
}
x44 += 1
} // end fat loop x27
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
