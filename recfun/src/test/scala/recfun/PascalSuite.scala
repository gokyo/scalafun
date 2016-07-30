package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  
  import Main._

  /**
   * Tests to make:
   *  - for all x < 0, and for all y, pascal(x, y) = pascal(y, x) = 0
   *  - for all x, pascal(x, x) = pascal(0, x) = pascal(0, x) = 1   
   *  - for all r > 1, pascal(1, r) = pascal(r-1, r) = r
   *  - for all c > r, pascal(c, r) = 0
   */

  // not in the triangle
  test("pascal: col=2,row=1") {
    assert(pascal(2,1) === 0)
  }
  test("pascal: col=18,row=17") {
    assert(pascal(18,17) === 0)
  }
  test("pascal: col=3,row=1") {
    assert(pascal(3,1) === 0)
  }
  test("pascal: col=9,row=1") {
    assert(pascal(9,1) === 0)
  }
  test("pascal: col=1,row=0") {
    assert(pascal(1,0) === 0)
  }
  
  // edges
  test("pascal: col=0,row=0") {
    assert(pascal(0,0) === 1)
  }
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }
  test("pascal: col=0,row=12") {
    assert(pascal(0,12) === 1)
  }
  test("pascal: col=1,row=1") {
    assert(pascal(1,1) === 1)
  }
  test("pascal: col=8,row=8") {
    assert(pascal(8,8) === 1)
  }
  test("pascal: col=14,row=14") {
    assert(pascal(14,14) === 1)
  }
  
  // c = 1 || c = r-1 
  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }
  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  test("pascal: col=1,row=9") {
    assert(pascal(1,9) === 9)
  }
  test("pascal: col=8,row=9") {
    assert(pascal(8,9) === 9)
  }
  
  // other tests
  test("pascal: col=2,row=6") {
    assert(pascal(2,6) === 15)
  }
  test("pascal: col=3,row=6") {
    assert(pascal(3,6) === 20)
  }
  test("pascal: col=5,row=9") {
    assert(pascal(5,9) === 126)
  }
  test("pascal: col=6,row=13") {
    assert(pascal(6, 13) === 1716)
  }    
}
