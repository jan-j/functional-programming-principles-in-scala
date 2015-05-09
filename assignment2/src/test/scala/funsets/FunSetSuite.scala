package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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
    assert(contains(x => true, 100))
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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)

    val u1 = union(union(s1, s2), s3) // 1 2 3
    val u2 = union(union(s3, s4), s5) // 3 4 5
    val u3 = union(union(s4, s5), s6) // 4 5 6
    val u12 = union(u1, u2) // 1 2 3 4 5
    val u23 = union(u2, u3) // 3 4 5 6
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements that are in both sets") {
    new TestSets {
      val uu1 = union(s1, s2)
      val uu2 = union(s2, s3)
      val s = intersect(uu1, uu2)

      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains only elements from first set that are not in second set") {
    new TestSets {
      val uu1 = union(s1, s2)
      val uu2 = union(s2, s3)
      val s = diff(uu1, uu2)

      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains only elements that pass test") {
    new TestSets {
      val s = filter(u12, (elem) => elem % 2 == 0)

      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
      assert(contains(s, 4), "Filter 4")
      assert(!contains(s, 5), "Filter 5")
    }
  }

  test("forall tests if all elements pass test") {
    new TestSets {
      assert(!forall(u12, (elem) => elem % 2 == 0), "Forall 1")
      assert(forall(u12, (elem) => elem > 0), "Forall 2")
      assert(!forall(u12, (elem) => elem < 0), "Forall 3")
    }
  }

  test("exists tests if any elements pass test") {
    new TestSets {
      assert(exists(u12, (elem) => elem % 2 == 0), "Exists 1")
      assert(exists(u12, (elem) => elem == 5), "Exists 2")
      assert(!exists(u12, (elem) => elem == 9), "Exists 3")
    }
  }

  test("map transforms set by applying function") {
    new TestSets {
      val nu12 = map(u12, (elem) => -elem)
      val mu12 = map(u12, (elem) => elem * 2)
      val ou12 = map(u12, (elem) => 1)

      assert(forall(nu12, (elem) => elem < 0), "Map 1")
      assert(forall(mu12, (elem) => elem % 2 == 0), "Map 2")
      assert(forall(ou12, (elem) => elem == 1), "Map 3")
    }
  }
}
