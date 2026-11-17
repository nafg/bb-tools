package mavencore

import subscriptionsource.Recognition

class MavenTest extends munit.FunSuite:
  test("parses Maven Central artifacts from coordinates and URLs"):
    val versioned = Some(MavenArtifact("io.github.nafg", "slick-additions_3", Some("0.18.0")))
    val any       = Some(MavenArtifact("io.github.nafg", "slick-additions_3", None))
    assertEquals(MavenArtifact.parse("maven:io.github.nafg:slick-additions_3:0.18.0"), versioned)
    assertEquals(MavenArtifact.parse("maven:io.github.nafg:slick-additions_3"), any)
    assertEquals(MavenArtifact.parse("https://central.sonatype.com/artifact/io.github.nafg/slick-additions_3/0.18.0"), versioned)
    assertEquals(MavenArtifact.parse("https://mvnrepository.com/artifact/io.github.nafg/slick-additions_3"), any)
    assertEquals(MavenArtifact.parse("https://repo1.maven.org/maven2/io/github/nafg/slick-additions_3/0.18.0/"), versioned)
    assertEquals(MavenArtifact.parse("https://repo1.maven.org/maven2/io/github/nafg/slick-additions_3/"), any)
    val t = MavenArtifact("org.scala-lang", "scala3-library_3", Some("3.9.0"))
    assertEquals(MavenArtifact.parse(t.url), Some(t))
    assertEquals(t.label, "org.scala-lang:scala3-library_3:3.9.0")

  test("bare coordinates are refused with a hint; other sources' targets are not this source's"):
    assertEquals(
      MavenArtifact.recognize("io.github.nafg:slick-additions_3"),
      Recognition.Invalid("for a Maven artifact, write maven:io.github.nafg:slick-additions_3")
    )
    assertEquals(MavenArtifact.recognize("https://github.com/slick/slick/pull/1"), Recognition.NotMine)
    assertEquals(MavenArtifact.recognize("bb:thread/thr_x/archived"), Recognition.NotMine)

  test("a watched version is reported once when it appears, which ends the subscription"):
    val a = MavenArtifact("g", "a", Some("1.0"))
    assertEquals(MavenWatch.check(a, None, MavenState.Version(false)).messages, Nil)
    val appeared = MavenWatch.check(a, Some(MavenState.Version(false)), MavenState.Version(true))
    assertEquals(appeared.messages, List("g:a:1.0 is now on Maven Central and can be resolved."))
    assert(appeared.finished)
    assertEquals(MavenWatch.check(a, Some(MavenState.Version(false)), MavenState.Version(false)).messages, Nil)

  test("new versions of an artifact are reported"):
    val a = MavenArtifact("g", "a", None)
    assertEquals(
      MavenWatch.check(a, Some(MavenState.Versions(List("1.0"))), MavenState.Versions(List("1.0", "1.1"))).messages,
      List("New version of g:a on Maven Central: 1.1.")
    )

  test("stored states round-trip"):
    List(MavenState.Version(true), MavenState.Version(false), MavenState.Versions(List("1.0", "1.1")))
      .foreach(st => assertEquals(MavenState.decode(st.encode), Right(st)))
