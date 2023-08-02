package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
      val h = insert(a, empty)
      findMin(h) == a
    }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get " +
    "the smallest of the two elements back") = forAll { (elem1: A, elem2: A) =>
      val h = insert(elem1, insert(elem2, empty))
      findMin(h) == elem1.min(elem2)
    }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") =
    forAll { (elem: A) =>
      val h = insert(elem, empty)
      isEmpty(deleteMin(h))
    }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minims") =
    forAll { (h: H) =>
      @tailrec
      def loop(h: H): Boolean = {
        if isEmpty(h) then true
        else {
          val new_h = deleteMin(h)
          if (!isEmpty(new_h) && findMin(new_h) < findMin(h))
            false
          else
            loop(new_h)
        }
      }

      loop(h)
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (heap1: H, heap2: H) =>
      val minHeap1 = findMin(heap1)
      val minHeap2 = findMin(heap2)
      findMin(meld(heap1, heap2)) == minHeap1.min(minHeap2)
    }

  property("Inserting the minimum element of the first heap into the melding of the two heaps should be the same as " +
    "inserting it into the first heap and then melding it with the second") = forAll { (heap1: H, heap2: H) =>
    @tailrec
    def loop(heap1: H, heap2: H): Boolean = {
      if (isEmpty(heap1) && isEmpty(heap2))
        true
      else
        findMin(heap1) == findMin(heap2) && loop(deleteMin(heap1), deleteMin(heap2))
    }

    loop(insert(findMin(heap1), meld(heap1, heap2)), meld(insert(findMin(heap1), heap1), heap2))
  }