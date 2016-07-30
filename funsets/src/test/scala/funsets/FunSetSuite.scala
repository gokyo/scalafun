package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.reflect.runtime.TwoWayCache

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains((x: Int) => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val emptySet = (x: Int) => false
    val fullSet = (x: Int) => true
    
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    
    val positiveNumbers = (x: Int) => x > 0
    val cubes = map(positiveNumbers, (x: Int) => x * x * x)
    
    val evenNumbers = (x: Int) => x % 2 == 0
    val oddNumbers = (x: Int) => x % 2 != 0
    
    val twoAndThree = union(s2, s3);
    val allButNotThree = diff(fullSet, s3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singleton") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "{1} contain 1")
      assert(!contains(s2, 1), "{2} does not contain 1")
    }
  }

  test("basic sets test") {
    new TestSets {
      assert(!contains(evenNumbers, 1), "1 is not even")
      assert(contains(oddNumbers, 1), "1 is odd")
      assert(!contains(twoAndThree, 4), "4 is not 2 or 3")
      assert(!contains(positiveNumbers, -1), "-1 is not positive")
      assert(contains(positiveNumbers, 1), "1 is positive")
      assert(contains(cubes, 8), "8 is a cube")
      assert(contains(cubes, 1), "1 is a cube")
      assert(!contains(cubes, 9), "9 is not a cube")
      assert(!cubes(2), "2 is not a cube")
    }
  }

  test("toString test") {
    new TestSets {
      assert(FunSets.toString(emptySet).equals("{}"), "{} toString")
      assert(FunSets.toString(twoAndThree) == ("{2,3}"), "{2,3} toString")
      assert(FunSets.toString(fullSet).startsWith("{-1000,-999,"), "full set = begin toString")
      assert(FunSets.toString(fullSet).endsWith(",999,1000}"), "full set = end toString")
    }
  }  
  
  test("union test") {
    new TestSets {
      val universe = union(positiveNumbers, (x: Int) => x <= 0)
      assert(forall(universe, x => true) === forall(fullSet, x => true))
      
      val u1 = union(s1, s2)
      assert(contains(u1, 1), "Union 1")
      assert(contains(u1, 2), "Union 2")
      assert(!contains(u1, 3), "Union 3")
      
      val u2 = union (emptySet, s2)
      assert(!contains(u2, 1), "{} + {2} does not contain 1")
      assert(contains(u2, 2), "{} + {2} contains 2")

      assert(contains(twoAndThree, 2))
      assert(contains(twoAndThree, 3))
      assert(!contains(twoAndThree, 1))
    }
  }

  test("intersect test") {
    new TestSets {
      val i1 = intersect(s1, s2)
      assert(!contains(i1, 1), "{1} ^ {2} does not contain 1")
      assert(!contains(i1, 2), "{1} ^ {2} does not contain 2")
      assert(!contains(i1, 3), "{1} ^ {2} does not contain 3")
      
      val i2 = intersect(fullSet, s1)
      assert(contains(i2, 1), "ALL ^ {1} contains 1")
      assert(!contains(i2, 2), "ALL ^ {1} does not contain 2")
      
      val i3 = intersect(fullSet, emptySet)
      assert(!contains(i3, 1), "ALL ^ {} does not contain 1")
      assert(!contains(i3, 2), "ALL ^ {} does not contain 2")
      assert(!contains(i3, 3), "ALL ^ {} does not contain 3")
      
      val i4 = intersect(emptySet, s3)
      assert(!contains(i4, 1), "{} ^ {3} does not contain 1")
      assert(!contains(i4, 2), "{} ^ {3} does not contain 2")
      assert(!contains(i4, 3), "{} ^ {3} does not contain 3")
    }    
  }
  
  test("difference test") {
    new TestSets {
      val d1 = diff(s1, s2)
      assert(contains(d1, 1), "{1} - {2} contains 1")
      
      val d2 = diff(fullSet, s1)
      assert(contains(d2, 9), "ALL - {1} contains 9")
      assert(!contains(d2, 1), "ALL - {1} does not contain 1")
      
      val d3 = diff(fullSet, emptySet)
      assert(contains(d3, 99), "ALL - {} contains 99")
      
      val d4 = diff(emptySet, s3)
      assert(!contains(d4, 1), "{} - {3} does not contain 1")
      assert(!contains(d4, 2), "{} - {3} does not contain 2")
      assert(!contains(d4, 3), "{} - {3} does not contain 3")
    }    
  }

  test("filter test") {
    new TestSets {
      assert(contains(filter(fullSet, fullSet), 0), "ALL after filter")
      assert(contains(filter(s2, evenNumbers), 2), "{2} contains 2")
      assert(!contains(filter(evenNumbers, s2), 0), "{2} contains 0")    
      assert(!contains(filter(fullSet, evenNumbers), 3), "Even numbers do not contain 3")
      assert(!contains(filter(fullSet, emptySet), 2), "{} is empty")
      assert(contains(filter(s1, oddNumbers), 1), "{1} contains 1")
      assert(!contains(filter(s1, evenNumbers), 2), "{} does not contain 2")
    }
  }
  
  test("forall test") {
    new TestSets {
      assert(forall(fullSet, fullSet), "ALL contains ALL")
      assert(forall(emptySet, emptySet), "{} does not contain any element")
      assert(forall(emptySet, fullSet), "{} contains any element")
      assert(forall(emptySet, (x: Int) => x == 2 || x == 5), "{} contains satisfies any predicate")
      assert(!forall(s3, emptySet), "{3} contains elements in [-1000, 1000]")
      assert(!forall(singletonSet(1000), emptySet), "{1000} contains elements in [-1000, 1000]")
      assert(forall(singletonSet(-1001), emptySet), "{-1001} does not contain any element in [-1000, 1000]")
      assert(forall(diff(fullSet, s3), fullSet), "ALL - {3} contains any element")
      assert(forall(diff(fullSet, s3), (x: Int) => x != 3), "ALL - {3} contains elements <> 3")
      assert(forall(fullSet, (x: Int) => x < 1001), "ALL contains only elements < 1001")
      assert(forall(s3, (x: Int) => x == 3), "{3} contains only 3")
      assert(forall(union(s1, s2), (x: Int) => x == 1 || x == 2), "{1, 2} contains elements == 1 || == 2 ")
      assert(forall(union(s1, s2), (x: Int) => x != 3), "{1, 2} contains elements <> 3")
      assert(!forall(union(s1, s3), (x: Int) => x == 3), "{1, 3} does not contain only  3")
      assert(!forall(union(s1, s2), emptySet), "{1, 2} is not empty")
      assert(!forall(allButNotThree, evenNumbers), "ALL - {3} contains even numbers")
      assert(forall(evenNumbers, allButNotThree), "The even numbers set does not contain 3")
      assert(forall(filter(fullSet, (x: Int) => x > 100), (x: Int) => x - 100 >  0), "filter and forall")
     }
  }
  
  test("exists test") {
    new TestSets {
      assert(!exists(emptySet, (x: Int) => true), "{} has not elements")
      assert(!exists(fullSet, emptySet), "ALL ... exists on {}")
      assert(!exists(emptySet, emptySet), "{} ... exists on {}")
      assert(!exists(s1, emptySet), "{1} ... exists on {}")
      assert(!exists(emptySet, s1), "{} contains any element")
      assert(exists(allButNotThree, fullSet), "ALL - {3} contains satisfies any predicate")
      assert(!exists(allButNotThree, s3), "ALL - {3} does not contain 3")
      assert(exists(s1, fullSet), "{1} contains at least one element")
      assert(!exists(s1, evenNumbers), "{1} does not contain even numbers")
      assert(exists(s1, oddNumbers), "{1} contains at least one odd number")
      assert(exists(positiveNumbers, (x: Int) => x == 4), "4 > 0")
      assert(!exists(positiveNumbers, (x: Int) => x == -1), "-1 < 0")
     }    
  }

  test("map test") {
    new TestSets {
      assert(forall(map(emptySet, (x: Int) => x * 3), (x: Int) => false), "we still have {}")
      assert(!(map(s1, (x: Int) => x + 1)(1)), "{2} does not contain 1")
      assert((map(s1, (x: Int) => x + 1)(2)), "{2} contains 2")
      assert((map(evenNumbers, (x: Int) => x + 1)(1)), "1 is in the new set")
      assert(!(map(evenNumbers, (x: Int) => x + 1)(2)), "we do not have 2 anymore")
      assert(!map(fullSet, (x: Int) => x + 1)(-1000), "we have lost -1000 from the set")
      assert(map(fullSet, (x: Int) => x + 1)(-999), "-999 is still in the set")
      assert(FunSets.toString(cubes).startsWith("{1,8,27,"), "to String() of the cubes")
    }
  }  
  
}
