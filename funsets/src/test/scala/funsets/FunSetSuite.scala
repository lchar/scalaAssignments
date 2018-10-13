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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    /* Singletons */
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    /* ranges */
    val r1: Set = x => x > 0 && x < 101
    val r2: Set = x => x > 49 && x < 101
    val r3: Set = x => x > -51 && x < 51
    val r4: Set = x => x > 200 && x < 301

    /* empty set */
    val empty: Set = x => false
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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      val ru1 = union(r1, r2)
      val ru2 = union(r1, r3)
      val ru3 = union(r2, r3)
      val ru4 = union(r1, r4)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      assert(contains(ru1, 25), "Union 4")
      assert(contains(ru1, 50), "Union 5")
      assert(!contains(ru1, 0), "Union 6")
      assert(contains(ru2, -25), "Union 7")
      assert(contains(ru2, 25), "Union 8")
      assert(contains(ru2, 75), "Union 9")
      assert(contains(ru3, -25), "Union 10")
      assert(contains(ru3, 25), "Union 11")
      assert(contains(ru3, 75), "Union 12")
      assert(!contains(ru4, 0), "Union 13")
      assert(contains(ru4, 50), "Union 14")
      assert(!contains(ru4, 150), "Union 15")
      assert(contains(ru4, 250), "Union 16")

    }
  }

  test("intersection contains all elements common to each set") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val u = intersect(u1, u2)
      val ri1 = intersect(r1, r2)
      val ri2 = intersect(r1, r3)
      val ri3 = intersect(r2, r3)
      val ri4 = intersect(r1, r4)
      assert(!contains(u, 1), "Intersect 1")
      assert(contains(u, 2), "Intersect 2")
      assert(!contains(u, 3), "Intersect 3")
      assert(!contains(ri1, 25), "Intersect 4")
      assert(contains(ri1, 75), "Intersect 5")
      assert(!contains(ri1, 101), "Intersect 6")
      assert(!contains(ri2, 75), "Intersect 7")
      assert(contains(ri2, 25), "Intersect 8")
      assert(!contains(ri2, -25), "Intersect 9")
      assert(!contains(ri3, 49), "Intersect 10")
      assert(contains(ri3, 50), "Intersect 11")
      assert(!contains(ri3, 51), "Intersect 12")
      assert(!contains(ri4, 50), "Intersect 13")
      assert(!contains(ri4, 150), "Intersect 14")
      assert(!contains(ri4, 250), "Intersect 15")
    }
  }

  test("difference contains all elements of the first set not present in the second") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val u = diff(u1, u2)
      val ri1 = diff(r1, r2)
      val ri2 = diff(r1, r3)
      val ri3 = diff(r2, r3)
      val ri4 = diff(r1, r4)
      assert(contains(u, 1), "Diff 1")
      assert(!contains(u, 2), "Diff 2")
      assert(!contains(u, 3), "Diff 3")
      assert(contains(ri1, 25), "Diff 4")
      assert(!contains(ri1, 75), "Diff 5")
      assert(!contains(ri1, 101), "Diff 6")
      assert(contains(ri2, 75), "Diff 7")
      assert(!contains(ri2, 25), "Diff 8")
      assert(!contains(ri2, -25), "Diff 9")
      assert(!contains(ri3, 49), "Diff 10")
      assert(!contains(ri3, 50), "Diff 11")
      assert(contains(ri3, 51), "Diff 12")
      assert(contains(ri4, 50), "Diff 13")
      assert(!contains(ri4, 150), "Diff 14")
      assert(!contains(ri4, 250), "Diff 15")
    }
  }

  test("filter contains all elements of each set complying with predicate") {
    new TestSets {
      assert(contains(filter(s1, x => x < 3), 1), "Filter 1")
      assert(contains(filter(s2, x => x < 3), 2), "Filter 2")
      assert(!contains(filter(s3, x => x < 3), 3), "Filter 3")
      assert(contains(filter(r1, x => x < 50), 25), "Filter 4")
      assert(!contains(filter(r1, x => x < 50), 50), "Filter 5")
      assert(!contains(filter(r1, x => x < 50), 75), "Filter 6")
      assert(!contains(filter(r2, x => x < 50), 75), "Filter 7")
      assert(!contains(filter(r2, x => x < 50), 25), "Filter 8")
      assert(contains(filter(r2, x => contains(r1, x)), 75), "Filter 9")
      assert(contains(filter(r3, x => x < 1), 0), "Filter 10")
      assert(contains(filter(r3, x => x < 0), -50), "Filter 11")
      assert(!contains(filter(r3, x => x > 50), 50), "Filter 12")
      assert(!contains(filter(r4, x => x < 50), 250), "Filter 13")
      assert(contains(filter(r4, x => x > 250), 275), "Filter 14")
      assert(!contains(filter(r4, x => x > 350), 250), "Filter 15")
    }
  }

  test("forall tests") {
    new TestSets {
      assert(forall(s1, x => x > 0), "For all 1")
      assert(!forall(s3, x => x < 0), "For all 2")
      assert(forall(r1, x => x > 0), "For all 3")
      assert(forall(r1, x => x < 101), "For all 4")
      assert(!forall(r2, x => x > 50), "For all 5")
      assert(forall(r2, x => x > 49), "For all 6")
      assert(!forall(r3, x => x > 0), "For all 7")
      assert(!forall(r3, x => x < 0), "For all 8")
      assert(forall(r4, x => x > 200), "For all 9")
    }
  }

  test("exists tests") {
    new TestSets {
      assert(exists(s1, x => x == 1), "exists 1")
      assert(!exists(s3, x => x == 1), "exists 2")
      assert(exists(r1, x => x > 99), "exists 3")
      assert(!exists(r1, x => x > 100), "exists 4")
      assert(exists(r2, x => x < 51), "exists 5")
      assert(!exists(r2, x => x < 50), "exists 6")
      assert(exists(r3, x => x > 0), "exists 7")
      assert(exists(r3, x => x < 0), "exists 8")
      assert(exists(r4, x => x > 0), "exists 9")
    }
  }

  test("map tests") {
    new TestSets {
      assert(contains(map(s1, x => x + 2 ), 3), "Map 1")
      assert(!contains(map(s3, x => x + 2 ), 3), "Map 2")
      assert(contains(map(r1, x => 2 * x ), 176), "Map 3")
      assert(!contains(map(r1, x => 2 * x ), 175), "Map 4")
      assert(contains(map(r2, x => 1 ), 1), "Map 5")
      assert(!contains(map(r2, x => 1 ), 75), "Map 6")
      assert(contains(map(r3, x => x * x ), 25), "Map 7")
      assert(!contains(map(r3, x => x * x ), -25), "Map 8")
      assert(contains(map(r4, x => x / 2 ), 125), "Map 9")
    }
  }

  test("empty set") {
    new TestSets {
      assert(!contains(empty, 88), "empty 1")
      assert(contains(union(empty, s1), 1), "empty 2")
      assert(!contains(intersect(empty, s2), 2), "empty 3")
      assert(!contains(diff(empty, s3), 3), "empty 4")
      assert(!contains(filter (empty, x => true), 88), "empty 5")
      assert(forall(empty, x => false), "empty 6")
      assert(!exists(empty, x => false), "empty 7")
      assert(!contains(map(empty, x => x + 1), 88), "empty 8")
    }
  }


}
