package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  
  import Main.balance

  // existing tests
  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }
  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }
  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }
  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }
  
  // other tests with right balance 
  test("balance: Nil") {
    assert(balance(Nil.toList))
  }
  test("balance: empty") {
    assert(balance("".toList))
  }  
  test("balance: tab") {
    assert(balance("	".toList))
  }
  test("balance: ()") {
    assert(balance("()".toList))
  }
  test("balance: (ciao(x)(()()))") {
    assert(balance("(ciao(x)(()()))".toList))
  }  
  
  // other tests with wrong balance
  test("balance: ((((x)y))") {
    assert(!balance("((((x)y))".toList))
  }
  test("balance: )") {
    assert(!balance(")".toList))
  }
  test("balance: )(((") {
    assert(!balance(")(((".toList))
  }  
  test("balance: ())") {
    assert(!balance("())".toList))
  }  
}
