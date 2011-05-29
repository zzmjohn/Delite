package ppl.delite.framework

object Config {
  //var degFilename = System.getProperty("delite.deg.filename", "")
  var degFilename = System.getProperty("delite.deg.filename", "out.deg")
  var opfusionEnabled = {
    val f = System.getProperty("delite.opfusion.enabled", "true")
    val g = f == "true"
    println("delite.opfusion.enabled=" + g)
    g
  }
  var homeDir = System.getProperty("delite.home.dir", System.getProperty("user.dir"))
  //var buildDir = System.getProperty("delite.build.dir", homeDir + java.io.File.separator + "generated")
  var buildDir = System.getProperty("delite.build.dir", "generated")
  var blasHome = System.getProperty("blas.home")
  var useBlas = if (blasHome == null) false else true
  var nestedVariantsLevel = System.getProperty("nested.variants.level", "0").toInt
  val profileCoreOps: Boolean = {
    val f = System.getProperty("delite.profile.coreops", "false")
    val g = true // f == "true"
    println("delite.profile.coreops=" + g)
    g
  }
}
