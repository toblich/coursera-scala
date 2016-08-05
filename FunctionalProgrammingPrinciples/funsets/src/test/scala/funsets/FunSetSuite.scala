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
      assert(!contains(s1, 2), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(!contains(s2, 1), "Singleton")
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
  
  test("intersection contains only elements in both sets") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersection 1")
      assert(!contains(s, 2), "Intersection 2")
      assert(!contains(s, 3), "Intersection 3")
      
      val si = intersect(s2, s2)
      assert(contains(si,2), "Intersection 4")
      assert(!contains(si,3), "Intersection 5")
    }
  }
  
  test("difference contains only elements in the first but not the second set") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Difference 1")
      assert(!contains(s, 2), "Difference 2")
      assert(!contains(s, 3), "Difference 3")
      
      val si = diff(s2, s2)
      assert(!contains(si,2), "Difference 4")
      assert(!contains(si,3), "Difference 5")
    }
  }
  
  test("filter contains only elemnts in the set taken by the predicate") {
    new TestSets {
      val s = filter(s1, (x: Int) => x==1 )
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
      
      val si = filter(s2, (x: Int) => x==1 )
      assert(!contains(si,1), "Filter 4")
      assert(!contains(si,2), "Filter 5")
      assert(!contains(si,3), "Filter 6")
    }
  }
  
  test("forall") {
     new TestSets {
      assert(forall(s1, (x:Int)=> x==1), "Forall 1")
      assert(!forall(s1, (x:Int)=> x==2), "Forall 2")
      assert(forall(s2, (x:Int)=> x==2), "Forall 3")
     }
  }
  
   test("exists") {
     new TestSets {
      assert(exists(s1, (x:Int)=> x==1), "Exisits 1")
      assert(!exists(s1, (x:Int)=> x==2), "Exisits 2")
      assert(exists(s2, (x:Int)=> x==2), "Exisits 3")
      assert(!exists(s2, (x:Int)=> x!=2), "Exisits 4")
     }
  }
   
   test("map") {
     new TestSets {
      assert( exists( map(s1, (x: Int) => x*x), (x:Int)=> x==1), "Map 1")
      assert(!exists( map(s1, (x: Int) => x*x), (x:Int)=> x==2), "Map 2")
      assert(exists( map(s2, (x: Int) => x*x), (x:Int)=> x==4), "Map 3")
      assert(!exists( map(s2, (x: Int) => x*x), (x:Int)=> x==2), "Map 4")
      assert(!exists( map(s2, (x: Int) => x*x), (x:Int)=> x!=4), "Map 5")
     }
  }
}
