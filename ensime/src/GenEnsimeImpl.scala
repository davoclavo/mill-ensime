package fun.valycorp.mill

import ammonite.ops._
import coursier.{Cache, Repository, Dependency}
import coursier.maven.MavenRepository
import mill.define._
import mill.eval.{Evaluator, PathRef, Result}
import mill.modules.Jvm._
import mill.T
import mill.scalalib._
import mill.util.Ctx.{Home, Log}
import mill.util.{Loose, PrintLogger, Strict}
import mill.util.Strict.Agg

import scala.util.Try

import java.io.FileNotFoundException
import java.io.File

case class EnsimeConfig(
    root: String,
    cacheDir: String,
    scalaCompilerJars: Set[PathRef],
    ensimeServerJars: Set[PathRef],
    ensimeServerVersion: String,
    name: String,
    scalaVersion: String,
    javaHome: File,
    javaFlags: Seq[String],
    javaSrc: Set[File],
    projects: Seq[EnsimeProject]
)

case class EnsimeProjectId(
    project: String,
    config: String
)

case class EnsimeProject(
    id: EnsimeProjectId,
    depends: Seq[EnsimeProjectId],
    sources: Set[PathRef],
    targets: Set[PathRef],
    scalacOptions: Seq[String],
    javacOptions: Seq[String],
    libraryJars: Set[PathRef],
    librarySources: Set[PathRef],
    libraryDocs: Set[PathRef]
)

object GenEnsime extends ExternalModule {

  def ensimeConfig(ev: Evaluator, server: String = "2.0.0") = T.command {
    fun.valycorp.mill.GenEnsimeImpl(
      implicitly,
      ev.rootModule,
      ev.rootModule.millDiscover,
      server
    )
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  lazy val millDiscover = Discover[this.type]
}

object GenEnsimeImpl {

  def apply(ctx: Log with Home,
            rootModule: BaseModule,
            discover: Discover[_],
            server: String): Unit = {

    import SExpFormatter._

    val evaluator =
      new Evaluator(ctx.home, pwd / 'out, pwd / 'out, rootModule, ctx.log)

    val config = toSExp(ensimeGenerateConfig(evaluator, rootModule, server))

    rm ! pwd / ".ensime"

    write.over(pwd / ".ensime", config)
  }

  def evalOrElse[T](evaluator: Evaluator, e: Task[T], default: => T): T = {
    evaluator.evaluate(Agg(e)).values match {
      case Seq()     => default
      case Seq(e: T) => e
    }
  }

  def moduleProjectId(base: String): EnsimeProjectId = {
    val name =
      if (!base.endsWith(".test")) base else base.stripSuffix(".test")
    val config = if (!base.endsWith(".test")) "compile" else "test"
    EnsimeProjectId(name, config)
  }

  def resolveJars(deps: Seq[Dep], scalaV: String): Set[PathRef] =
    resolveDependencies(
      Seq(Cache.ivy2Local,
          MavenRepository("https://repo1.maven.org/maven2/"),
          MavenRepository(
            "https://oss.sonatype.org/content/repositories/releases")),
      deps.map(Lib.depToDependency(_, scalaV)),
      deps.map(Lib.depToDependency(_, scalaV))
    ) match {
      case Result.Success(e)      => e.items.toSet
      case Result.Skipped         => Set()
      case Result.Exception(_, _) => Set()
      case Result.Failure(_, _)   => Set()
    }

  def ensimeGenerateConfig(evaluator: Evaluator,
                           rootModule: mill.Module,
                           server: String): EnsimeConfig = {

    val allModules: Seq[ScalaModule] = rootModule.millInternal.modules.collect {
      case s: ScalaModule => s
    }

    // Determine scala version to be used for the project
    val ensimeScalaVersion = allModules
      .map(m => evalOrElse(evaluator, m.scalaVersion, ""))
      .groupBy(identity)
      .map { case (sv, svs) => sv -> svs.size }
      .toList
      .sortWith { case ((_, c1), (_, c2)) => c1 > c2 }
      .head
      ._1

    val major = Lib.scalaBinaryVersion(ensimeScalaVersion)

    val canon = allModules.filter(s =>
      (s.toString matches s""".*\\[$ensimeScalaVersion\\].*"""))
    val rest = allModules.filter(s => !(s.toString matches """.*\[.*\].*"""))

    val scalaModules = canon ++ rest

    val scalaCompilerJars: Set[PathRef] = Strict.Agg
      .from(
        evalOrElse(evaluator,
                   scalaModules.head.scalaCompilerClasspath,
                   Strict.Agg.empty))
      .toSet

    val ensimeServerJars = resolveJars(
      Seq(ivy"org.ensime::server:$server"),
      ensimeScalaVersion
    )

    val serverJars = ensimeServerJars -- scalaCompilerJars

    val javaFlags = Seq(
      "-Dscala.classpath.closeZip=true",
      "-Xss2m",
      "-Xms512m",
      "-Xmx4g",
      "-XX:MaxMetaspaceSize=256m",
      // these improve ensime-server performance (ignoring jdk version)
      "-XX:StringTableSize=1000003",
      "-XX:+UnlockExperimentalVMOptions",
      "-XX:SymbolTableSize=1000003"
    )

    // Code taken from sbt-ensime
    val javaHome: File = List(
      // manual
      sys.env.get("JDK_HOME"),
      sys.env.get("JAVA_HOME"),
      // fallback
      sys.props.get("java.home").map(new File(_).getParent),
      sys.props.get("java.home"),
      // osx
      Try(sys.process.Process("/usr/libexec/java_home").!!.trim).toOption
    ).flatten
      .filter { n =>
        new File(n + "/lib/tools.jar").exists || new File(n, "jmods").isDirectory
      }
      .headOption
      .map(new File(_).getCanonicalFile)
      .getOrElse(
        throw new FileNotFoundException(
          """Could not automatically find the JDK home.
           |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin
        )
      )

    val javaSrc: Set[File] = {
      new File(javaHome.getAbsolutePath + "/src.zip") match {
        case f if f.exists => Set(f)
        case _             => Set.empty
      }
    }
    val ensimeProjects = for (m <- scalaModules) yield {

      val deps = m.moduleDeps.map(_.toString).map(moduleProjectId(_))

      val scalacOpts = evalOrElse(evaluator, m.scalacOptions, Seq())
      val javacOpts = evalOrElse(evaluator, m.javacOptions, Seq())

      val sources =
        Strict.Agg
          .from(evalOrElse(evaluator, m.allSources, Strict.Agg.empty))
          .toSet

      val allDeps = T.task {
        m.ivyDeps() ++ m.compileIvyDeps() ++
          m.scalaLibraryIvyDeps()
      }
      val externalDependencies = T.task {
        m.resolveDeps(allDeps)()
      }
      val externalSources = T.task {
        m.resolveDeps(allDeps, sources = true)()
      }

      val libJars: Set[PathRef] =
        Strict.Agg
          .from(evalOrElse(evaluator, externalDependencies, Strict.Agg.empty))
          .toSet
      val libSrcs: Set[PathRef] =
        Strict.Agg
          .from(evalOrElse(evaluator, externalSources, Strict.Agg.empty))
          .toSet
      val libDocs: Set[PathRef] = Strict.Agg.empty.toSet

      val compileTargets: Set[PathRef] =
        Strict.Agg
          .from(evalOrElse(evaluator, T.task{Seq(m.compile().classes)}, Strict.Agg.empty))
          .toSet

      EnsimeProject(
        moduleProjectId(m.toString),
        deps,
        sources.toSet,
        compileTargets.toSet,
        scalacOpts,
        javacOpts,
        libJars,
        libSrcs,
        libDocs
      )
    }

    EnsimeConfig(
      pwd.toString,
      (pwd / ".ensime_cache").toString,
      scalaCompilerJars,
      ensimeServerJars,
      server,
      pwd.last,
      ensimeScalaVersion,
      javaHome,
      javaFlags,
      javaSrc,
      ensimeProjects
    )
  }

}

object SExpFormatter {

  def toSExp(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def toSExp(pr: PathRef): String = toSExp(pr.path.toString)

  def toSExp(f: File): String = toSExp(f.getAbsolutePath)

  def ssToSExp(ss: Iterable[String]): String =
    if (ss.isEmpty) "nil"
    else ss.toSeq.map(toSExp).mkString("(", " ", ")")

  def fsToSExp(fs: Iterable[File]): String =
    if (fs.isEmpty) "nil"
    else fs.toSeq.map(toSExp).mkString("(", " ", ")")

  def prsToSExp(prs: Iterable[PathRef]): String =
    if (prs.isEmpty) "nil"
    else prs.toSeq.map(toSExp).mkString("(", " ", ")")

  def psToSExp(ss: Iterable[EnsimeProject]): String =
    if (ss.isEmpty) "nil"
    else ss.toSeq.sortBy(_.id.toString).map(toSExp).mkString("(", " ", ")")

  def sToSExp(key: String, op: Option[String]): String =
    op.map { f =>
        s":$key ${toSExp(f)}"
      }
      .getOrElse("")

  // a lot of legacy key names and conventions
  def toSExp(c: EnsimeConfig): String = s"""(
 :root-dir ${toSExp(c.root)}
 :cache-dir ${toSExp(c.cacheDir)}
 :scala-compiler-jars ${prsToSExp(c.scalaCompilerJars)}
 :ensime-server-jars ${prsToSExp(c.ensimeServerJars)}
 :ensime-server-version ${toSExp(c.ensimeServerVersion)}
 :name "${c.name}"
 :java-home ${toSExp(c.javaHome)}
 :java-flags ${ssToSExp(c.javaFlags)}
 :java-sources ${fsToSExp(c.javaSrc)}
 :scala-version ${toSExp(c.scalaVersion)}
 :projects ${psToSExp(c.projects)}
)"""

  def toSExp(p: EnsimeProject): String = s"""(
    :id ${toSExp(p.id)}
    :depends ${idsToSExp(p.depends)}
    :sources ${prsToSExp(p.sources)}
    :targets ${prsToSExp(p.targets)}
    :scalac-options ${ssToSExp(p.scalacOptions)}
    :javac-options ${ssToSExp(p.javacOptions)}
    :library-jars ${prsToSExp(p.libraryJars)}
    :library-sources ${prsToSExp(p.librarySources)}
    :library-docs ${prsToSExp(p.libraryDocs)}
  )"""

  def toSExp(id: EnsimeProjectId): String =
    s"""(:project ${toSExp(id.project)} :config ${toSExp(id.config)})"""

  def idsToSExp(ids: Iterable[EnsimeProjectId]): String =
    if (ids.isEmpty) "nil"
    else ids.toSeq.sortBy(_.toString).map(toSExp).mkString("(", " ", ")")

}
