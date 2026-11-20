package agentmailcore

class UnthreadedReplyTest extends munit.FunSuite:
  private def known(
      id: String,
      subject: String,
      correspondents: Set[String],
      createdAt: Long = 0,
      bbThreadId: String = "thr_a"
  ) = KnownEmailThread(id, bbThreadId, subject, correspondents, createdAt)

  // From the inbox: the email sent to Jivetel, and two unrelated sent threads.
  private val jivetel = known(
    "d8fa601d",
    "URGENT: portal.jivetel.net down since ~8:00 AM ET, LRBCOL call integration offline",
    Set("tech@jivetel.com", "efraim@lrbcol.org", "yehudah@lrbcol.org"),
    bbThreadId = "thr_a4sv7isk63"
  )
  private val entrance = known(
    "93f2b3d9",
    "URGENT: texts from +1 341-254-6807 (Chat 10, W#3867) not reaching recipients since ~1:25 PM ET today",
    Set("support@entrancegrp.com"),
    bbThreadId = "thr_7ky7ikamze"
  )
  private val proposal =
    known("0fbeda1d", "Texts via Jivetel and the text activity view: two questions", Set("yehudah@lrbcol.org"))
  private val inbox = List(jivetel, entrance, proposal)

  test("a ticket system's reply, from another address at a subdomain and with the ticket number first, finds the sent thread"):
    assertEquals(
      UnthreadedReply.findOriginal(
        "Ticket #1069268: URGENT: portal.jivetel.net down since ~8:00 AM ET, LRBCOL call integration offline",
        "Jivetel Technical Support <tech@tickets.jivetel.com>",
        inbox
      ),
      Some(jivetel)
    )

  test("a Re: reply sent without reply headers, from a colleague of the recipient, finds the sent thread"):
    assertEquals(
      UnthreadedReply.findOriginal(
        "Re: URGENT: texts from +1 341-254-6807 (Chat 10, W#3867) not reaching recipients since ~1:25 PM ET today",
        "Jack Gabbay <jack@entrancegrp.com>",
        inbox
      ),
      Some(entrance)
    )

  test("a reply from a cc'd correspondent's domain finds the sent thread"):
    assertEquals(
      UnthreadedReply.findOriginal(
        "Re: URGENT: portal.jivetel.net down since ~8:00 AM ET, LRBCOL call integration offline",
        "someone@lrbcol.org",
        inbox
      ),
      Some(jivetel)
    )

  test("a matching subject from a domain the thread never corresponded with matches nothing"):
    assertEquals(
      UnthreadedReply.findOriginal(
        "Ticket #1: URGENT: portal.jivetel.net down since ~8:00 AM ET, LRBCOL call integration offline",
        "help@example.com",
        inbox
      ),
      None
    )

  test("mail from a correspondent's domain whose subject doesn't contain the thread's matches nothing"):
    assertEquals(UnthreadedReply.findOriginal("Jivetel Quality Control", "John Maskott <john@jivetel.com>", inbox), None)

  test("subjects are compared word by word, ignoring case and punctuation"):
    val plan = known("t1", "Art show plan", Set("a@example.com"))
    assertEquals(UnthreadedReply.findOriginal("Re: smart show planning", "a@example.com", List(plan)), None)
    assertEquals(UnthreadedReply.findOriginal("[#12] ART -- show; plan!", "a@example.com", List(plan)), Some(plan))

  test("a subject of fewer than three words, not counting leading reply markers, never matches"):
    val short = known("t1", "Re: Quick question", Set("a@example.com"))
    assertEquals(UnthreadedReply.findOriginal("Re: Quick question", "a@example.com", List(short)), None)
    val threeWords = known("t2", "Re: Fwd: Quick question today", Set("a@example.com"))
    assertEquals(UnthreadedReply.findOriginal("Re: Quick question today", "a@example.com", List(threeWords)), Some(threeWords))

  test("when several threads match, the longest subject wins, then the most recent thread"):
    val general  = known("general", "Invoice for October work", Set("a@example.com"), createdAt = 30)
    val specific = known("specific", "Invoice for October work: second reminder", Set("a@example.com"), createdAt = 10)
    val again    = known("again", "Invoice for October work: second reminder", Set("a@example.com"), createdAt = 20)
    assertEquals(
      UnthreadedReply.findOriginal(
        "Re: Invoice for October work: second reminder",
        "a@example.com",
        List(general, specific, again)
      ),
      Some(again)
    )

  test("a sender field without an address matches nothing"):
    assertEquals(
      UnthreadedReply.findOriginal(
        "Re: Texts via Jivetel and the text activity view: two questions",
        "undisclosed-recipients",
        inbox
      ),
      None
    )

  test("bare addresses are taken from display forms and lowercased"):
    assertEquals(EmailAddress.bare("Jivetel Technical Support <Tech@Tickets.Jivetel.com>"), Some("tech@tickets.jivetel.com"))
    assertEquals(EmailAddress.bare("\"Gugenheim, Naftoli <x>\" <naftoligug@agentmail.to>"), Some("naftoligug@agentmail.to"))
    assertEquals(EmailAddress.bare(" yehudah@lrbcol.org "), Some("yehudah@lrbcol.org"))
    assertEquals(EmailAddress.bare(""), None)
    assertEquals(EmailAddress.bare("Undisclosed recipients <>"), None)

  test("domains are related when equal or one is a subdomain of the other, not when they only share a suffix"):
    assert(EmailAddress.sameOrSubdomain("tickets.jivetel.com", "jivetel.com"))
    assert(EmailAddress.sameOrSubdomain("jivetel.com", "tickets.jivetel.com"))
    assert(EmailAddress.sameOrSubdomain("jivetel.com", "jivetel.com"))
    assert(!EmailAddress.sameOrSubdomain("notjivetel.com", "jivetel.com"))
    assert(!EmailAddress.sameOrSubdomain("tickets.jivetel.com", "support.jivetel.com"))
