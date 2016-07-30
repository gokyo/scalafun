package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {

  import Main.countChange
  
  // existing tests
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }
  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }
  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }
  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
  
  // other tests
  test("countChange: List()") {
    assert(countChange(5,List()) === 0)
  }
  test("countChange: Nil") {
    assert(countChange(5,Nil) === 0)
  }  
  test("countChange: no enough money") {
    assert(countChange(5,List(50)) === 0)
  }
  test("countChange: no money") {
    assert(countChange(0,List(50)) === 1)
  }
  test("countChange: no money 2") {
    assert(countChange(0,List()) === 1)
  }  
  test("countChange: 10") {
    assert(countChange(10,List(2,5)) === 2)
  }
  test("countChange: 15") {
    assert(countChange(15,List(3,2,5)) === 7)
  }
  test("countChange: 4") {
    assert(countChange(4,List(3,2,1)) === 4)
  }  
}
