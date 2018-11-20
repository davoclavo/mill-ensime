import mill._
import mill.scalalib._
import publish._

object ensime extends ScalaModule with PublishModule {

  def scalaVersion = "2.12.7"

  def publishVersion = "0.0.3"

  def artifactName = "mill-ensime"

  def m2 = T {
    val pa = publishArtifacts()
    val wd = T.ctx().dest
    val ad = pa.meta.group.split("\\.").foldLeft(wd)((a, b) => a / b) / pa.meta.id / pa.meta.version
    os.makeDir.all(ad)
    pa.payload.map { case (f,n) => os.copy(f.path, ad/n) }
  }

  def pomSettings = PomSettings(
    description = "Ensime support for Mill builds",
    organization = "fun.valycorp",
    url = "https://github.com/yyadavalli/mill-ensime",
    licenses = Seq(License.`GPL-3.0+`),
    versionControl = VersionControl.github("yyadavalli", "mill-ensime"),
    developers = Seq(
      Developer("yyadavalli", "Yashwanth Yadavalli", "https://github.com/yyadavalli")
    )
  )

  def compileIvyDeps = Agg(
    ivy"com.lihaoyi::mill-scalalib:0.3.5",
    ivy"com.lihaoyi::geny:0.1.2",
  )

}
