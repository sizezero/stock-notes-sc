package org.kleemann.stocknotes.command

class TestOldest extends munit.FunSuite {

  test("succeed at simple options") {
    assertEquals(
      Oldest.parse(Vector[String]()),
      Right(Oldest.ParseArgs(false,false,None)))
    assertEquals(
      Oldest.parse(Vector("-r")),
      Right(Oldest.ParseArgs(true,false,None)))
    assertEquals(
      Oldest.parse(Vector("-t")),
      Right(Oldest.ParseArgs(false,true,None)))
    assertEquals(
      Oldest.parse(Vector("-t","-r")),
      Right(Oldest.ParseArgs(true,true,None)))
    assertEquals(
      Oldest.parse(Vector("-r","-t")),
      Right(Oldest.ParseArgs(true,true,None)))
  }

  test("succeed keyword") {
    assertEquals(
      Oldest.parse(Vector("-k","Snoodle")),
      Right(Oldest.ParseArgs(false,false,Some("Snoodle"))))
    assertEquals(
      Oldest.parse(Vector("-r","-k","Snoodle","-t")),
      Right(Oldest.ParseArgs(true,true,Some("Snoodle"))))
  }

  test("bad options") {
    assert(Oldest.parse(Vector("-foo")).isLeft)
    assert(Oldest.parse(Vector("-bar")).isLeft)
    assert(Oldest.parse(Vector("-baz")).isLeft)
    assert(Oldest.parse(Vector("-foo","-bar","-baz")).isLeft)
  }
  
  test("missing keyword") {
    assert(Oldest.parse(Vector("-k")).isLeft)
    assert(Oldest.parse(Vector("-r", "-k")).isLeft)
  }  
}
