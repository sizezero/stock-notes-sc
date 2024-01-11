class TestMain extends munit.FunSuite {
  test("fail when the program receives no arguments") {
    val ret = funcMain(List())
    assert(ret != None)
  }

  test("fail when the program receives an unknown command") {
    assert(funcMain(List("unknown-command")) != None)
    assert(funcMain(List("unknown-command", "otherargs")) != None)
  }

}
