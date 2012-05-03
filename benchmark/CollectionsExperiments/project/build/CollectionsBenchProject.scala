import sbt._



import java.io.File
import Process._


class CollectionsBenchProject(info: ProjectInfo) extends DefaultProject(info) {

  /* source directory layout */
  
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"

  override def testScalaSourcePath = "test" / "src"
  override def testResourcesPath = "test" / "resources"
  
  /* tasks */
  
  def classpath = "%s:%s:%s".format(
    packageTestJar,
    buildScalaInstance.libraryJar,
    jarPath
  )
  
  lazy val bench = task {
    args =>
    val jcomm = "java -server -Xmx6g -cp " + classpath + " " + args.mkString(" ")
    println("running: " + jcomm)
    task {
      jcomm !;
      None
    } dependsOn (`package`)
  }
  
}
