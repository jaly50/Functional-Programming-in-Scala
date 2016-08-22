package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
     x <- arbitrary[A]
     m <- oneOf(const(empty), genHeap)
  } yield insert(x, m)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("hint1") = forAll { (a1: A, a2:A ) =>
    val h = insert(a1, empty)
    val smaller = if (ord.lteq(a1,a2)) a1 else a2
    findMin(insert(a2, h)) == smaller
  }

//If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("hint2") = forAll {
    (a: A) =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  //delete and compare
  def dac(h: H, a:A):Boolean = {
      val next = deleteMin(h)
    (next == empty) || {
      val next_min = findMin(next)
      ord.lteq(a,findMin(next))  && dac(next, next_min)
    }
    }

  property("hint3") = forAll {
    (h: H) => (h == empty) || dac(h, findMin(h))

    }



  property("hint4") = forAll {
    (h1: H, h2: H) => {
      val min = findMin(meld(h1,h2))
      (min == findMin(h1)) || min == findMin(h2)
    }

  }

  def findMid(a1: A, a2: A, a3:A): A = {
     val l = List(a1,a2,a3).sorted(ord)
     l.indexOf(1)
  }

// For any 3 elements, if I delete min, then find min, it should return the middle one . == To deal with  bug4
  // Can't not use list.sorted() to sort the 3 number, don't know why
  property("bug4") = forAll {
    (a1: A, a2: A, a3: A) => {
      var mid = a1
      if ((ord.lteq(a1,a2) && ord.lteq(a2,a3)) || (ord.lteq(a3,a2) && ord.lteq(a2,a1)) ) mid = a2
      if ((ord.lteq(a1,a3) && ord.lteq(a3,a2)) || (ord.lteq(a2,a3) && ord.lteq(a3,a1)) ) mid = a3
      val h = insert(a3, insert(a2, insert(a1, empty)))
      mid == findMin(deleteMin(h))
    }

  }

}
