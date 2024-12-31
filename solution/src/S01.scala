package solution

import javax.naming.directory.InvalidAttributesException

object Solution {
  def last[A](input: List[A]): A = input.last
  def penultimate[A](input: List[A]): A = input match {
    case res :: _ :: Nil => res
    case _ :: tail       => penultimate(tail)
    case _               => throw new NoSuchElementException
  }
  def nth[A](n: Int, input: List[A]): A = {
    def nthR(count: Int, list: List[A]): A = {
      if (count < 0) throw new InvalidAttributesException
      if (count == 0)
        return list.head
      nthR(count - 1, list.tail)
    }
    nthR(n, input)
  }
  def length[A](input: List[A]): Int = {
    var count = 0
    def cnt(list: List[A]): Unit = {
      list match {
        case _ :: tail => {
          count += 1
          cnt(tail)
        }
        case Nil => ()
      }
    }
    cnt(input)
    count
  }
  def reverse[A](input: List[A]): List[A] = {
    input.foldLeft(List[A]()) { (r, h) => h :: r }
  }
  def isPalindrome[A](input: List[A]): Boolean = {
    input.reverse == input
  }
  def flatten(input: List[Any]): List[Any] = input flatMap {
    case ms: List[_] => flatten(ms)
    case e           => List(e)
  }
  def compress[A](input: List[A]): List[A] = input.foldRight(List[A]()) {
    (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
  }
  def pack[A](input: List[A]): List[List[A]] = input match {
    case Nil => List(List())
    case l => {
      val (packed, next) = l span { _ == l.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
  def encode[A](input: List[A]): List[(Int, A)] = input match {
    case Nil => List()
    case l => {
      val (packed, next) = l span { _ == l.head }
      val elem = (packed.length, packed.head)
      if (next == Nil) List(elem)
      else elem :: encode(next)
    }
  }
  def main(args: Array[String]): Unit =
    print("hello\n")
}
