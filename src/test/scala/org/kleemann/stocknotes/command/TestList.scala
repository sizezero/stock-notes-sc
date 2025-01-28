package org.kleemann.stocknotes.command

class TestList extends munit.FunSuite {

  test("succeed at simple options") {
    assertEquals(
      ListCommand.parse(Vector[String]()),
      Right(ListCommand.ParseArgs(false,false,None)))
    assertEquals(
      ListCommand.parse(Vector("-r")),
      Right(ListCommand.ParseArgs(true,false,None)))
    assertEquals(
      ListCommand.parse(Vector("-t")),
      Right(ListCommand.ParseArgs(false,true,None)))
    assertEquals(
      ListCommand.parse(Vector("-t","-r")),
      Right(ListCommand.ParseArgs(true,true,None)))
    assertEquals(
      ListCommand.parse(Vector("-r","-t")),
      Right(ListCommand.ParseArgs(true,true,None)))
  }

  test("succeed keyword") {
    assertEquals(
      ListCommand.parse(Vector("-k","Snoodle")),
      Right(ListCommand.ParseArgs(false,false,Some("Snoodle"))))
    assertEquals(
      ListCommand.parse(Vector("-r","-k","Snoodle","-t")),
      Right(ListCommand.ParseArgs(true,true,Some("Snoodle"))))
  }

  test("bad options") {
    assert(ListCommand.parse(Vector("-foo")).isLeft)
    assert(ListCommand.parse(Vector("-bar")).isLeft)
    assert(ListCommand.parse(Vector("-baz")).isLeft)
    assert(ListCommand.parse(Vector("-foo","-bar","-baz")).isLeft)
  }
  
  test("missing keyword") {
    assert(ListCommand.parse(Vector("-k")).isLeft)
    assert(ListCommand.parse(Vector("-r", "-k")).isLeft)
  }  
}
