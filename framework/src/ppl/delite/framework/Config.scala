package ppl.delite.framework

object Config {
  //var degFilename = System.getProperty("delite.deg.filename", "")
  var degFilename = System.getProperty("delite.deg.filename", "out.deg")
  var opfusionEnabled = {
    val f = System.getProperty("delite.opfusion.enabled", "false")
    val g = true //(f != "false")
    println("delite.opfusion.enabled.f = " + f)
    println("delite.opfusion.enabled.g = " + g)
    g
  }
  var homeDir = System.getProperty("delite.home.dir", System.getProperty("user.dir"))
  //var buildDir = System.getProperty("delite.build.dir", homeDir + java.io.File.separator + "generated")
  var buildDir = System.getProperty("delite.build.dir", "generated")
  var blasHome = System.getProperty("blas.home")
  var useBlas = if (blasHome == null) false else true
  var nestedVariantsLevel = System.getProperty("nested.variants.level", "0").toInt

}
