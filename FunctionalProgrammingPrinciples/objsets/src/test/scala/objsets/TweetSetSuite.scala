package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("union 1") {
    new TestSets {
      val setA = (new Empty).incl(new Tweet("A", "bodyA", 1)).incl(new Tweet("B", "bodyB", 2)).incl(new Tweet("C", "bodyC", 3))
      val setB = (new Empty).incl(new Tweet("A", "bodyAB", 1)).incl(new Tweet("B", "bodyBB", 2)).incl(new Tweet("C", "bodyCB", 3))
      val set = setA.union(setB).descendingByRetweet
      set foreach println
//      assert(size(set) === 6)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  test("custom descending") {
    new TestSets {
      val set = (new Empty).incl(new Tweet("A", "bodyA", 1)).incl(new Tweet("B", "bodyB", 2)).incl(new Tweet("C", "bodyC", 3))
      val trends = set.descendingByRetweet
      assert(!trends.isEmpty, "Esta vacia")
      assert(trends.head.user == "C", "Mal el 1° lugar")
      assert(trends.tail.head.user == "B", "Mal el 2° lugar")
      assert(trends.tail.tail.head.user == "A", "Mal el 3° lugar")
      assert(trends.tail.tail.tail.isEmpty, "Demasiado larga")
    }
  }
  
  test("custom descending2") {
    new TestSets {
      val set = (new Empty).incl(new Tweet("D", "bodyA", 1)).incl(new Tweet("B", "bodyB", 6)).incl(new Tweet("C", "bodyC", 3))
      val trends = set.descendingByRetweet
      assert(!trends.isEmpty, "Esta vacia")
      assert(trends.head.user == "B", "Mal el 1° lugar")
      assert(trends.tail.head.user == "C", "Mal el 2° lugar")
      assert(trends.tail.tail.head.user == "D", "Mal el 3° lugar")
      assert(trends.tail.tail.tail.isEmpty, "Demasiado larga")
    }
  }
}
