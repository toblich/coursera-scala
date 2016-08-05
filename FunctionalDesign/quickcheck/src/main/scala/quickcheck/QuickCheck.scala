package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("correct minimum") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum of (3-1) elems is middle elem") = forAll { (a: A, b: A, c: A) =>
    val inserted = insert(c, insert(b, insert(a, empty)))
    val h = deleteMin(inserted)
    val middle = List(a, b, c).sorted.tail.head
    findMin(h) == middle
  }

}