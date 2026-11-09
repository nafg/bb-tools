package billingcore

class MappingTest extends munit.FunSuite:
  private val acme   = ZohoTarget("p1", "Acme", "t1", "Development", "c1", "Acme Inc")
  private val globex = ZohoTarget("p2", "Globex", "t2", "Support", "c2", "Globex Corp")

  test("a thread's own entry wins"):
    val entries: String => MappingEntry = { case "child" => MappingEntry.Assigned(acme); case _ => MappingEntry.Absent }
    assertEquals(
      Mapping.resolve("child", Seq("parent", "root"), entries, Some(ProjectRule.Default(globex))),
      Resolved(Billability.Billable(acme), Some(MappingSource.Thread("child")))
    )

  test("an absent entry inherits from the nearest ancestor before the project default"):
    val entries: String => MappingEntry = { case "root" => MappingEntry.Assigned(acme); case _ => MappingEntry.Absent }
    assertEquals(
      Mapping.resolve("child", Seq("parent", "root"), entries, Some(ProjectRule.Default(globex))),
      Resolved(Billability.Billable(acme), Some(MappingSource.Thread("root")))
    )

  test("an explicit null stops inheritance and the project default"):
    val entries: String => MappingEntry = {
      case "parent" => MappingEntry.Unbillable
      case "root"   => MappingEntry.Assigned(acme)
      case _        => MappingEntry.Absent
    }
    assertEquals(
      Mapping.resolve("child", Seq("parent", "root"), entries, Some(ProjectRule.Default(globex))),
      Resolved(Billability.NotBillable, Some(MappingSource.Thread("parent")))
    )

  test("with no entry anywhere the project default applies"):
    assertEquals(
      Mapping.resolve("child", Seq("parent"), _ => MappingEntry.Absent, Some(ProjectRule.Default(globex))),
      Resolved(Billability.Billable(globex), Some(MappingSource.Project))
    )

  test("a thread follows its parent's bb project rule, not its own project's"):
    val rules: String => Option[ProjectRule] = { case "root" => Some(ProjectRule.Default(acme)); case _ => None }
    assertEquals(Mapping.projectRuleFor("child", Seq("parent", "root"), rules), Some(ProjectRule.Default(acme)))
    assertEquals(
      Mapping.projectRuleFor("child", Seq("parent"), { case "child" => Some(ProjectRule.NotClientWork); case _ => None }),
      Some(ProjectRule.NotClientWork)
    )

  test("a bb project marked not client work decides, unlike one that says nothing"):
    assertEquals(
      Mapping.resolve("child", Seq("parent"), _ => MappingEntry.Absent, Some(ProjectRule.NotClientWork)),
      Resolved(Billability.NotBillable, Some(MappingSource.Project))
    )

  test("no entry anywhere and no project default means not billable"):
    assertEquals(
      Mapping.resolve("child", Seq("parent"), _ => MappingEntry.Absent, None),
      Resolved(Billability.NotBillable, None)
    )

  test("a thread may choose only a Zoho project and task its bb project bills to"):
    val mapped = Seq(ProjectZoho("p1", "t1"), ProjectZoho("p2", "t2"))
    assertEquals(ProjectMapping.mayChoose(mapped, "p2", "t2"), Right(()))
    assert(ProjectMapping.mayChoose(mapped, "p1", "t2").isLeft, "another task of a mapped Zoho project")
    assert(ProjectMapping.mayChoose(mapped, "p9", "t9").isLeft, "a Zoho project the bb project does not bill to")
    assert(ProjectMapping.mayChoose(Nil, "p1", "t1").isLeft, "a bb project that bills to nothing")

  test("a project default exists only for a bb project with exactly one Zoho project"):
    val lookup: (String, String) => Option[ZohoTarget] = (p, t) => Option.when(p == "p1" && t == "t1")(acme)
    assertEquals(ProjectMapping.default(Seq(ProjectZoho("p1", "t1")), lookup), Some(acme))
    assertEquals(ProjectMapping.default(Seq(ProjectZoho("p1", "t1"), ProjectZoho("p2", "t2")), lookup), None)
    assertEquals(ProjectMapping.default(Nil, lookup), None)
    assertEquals(ProjectMapping.default(Seq(ProjectZoho("p9", "t9")), lookup), None)
