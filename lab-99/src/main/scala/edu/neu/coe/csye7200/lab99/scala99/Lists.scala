/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import edu.neu.coe.csye7200.lab99.scala99.P01.last

import scala.+:
import scala.annotation.tailrec
import scala.collection.View.Empty

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

//  @scala.annotation.tailrec
  def last[X](xs: List[X]): X = xs match {
    case Nil => throw new NoSuchElementException("last: empty")
    case h :: Nil => h
    case _ :: t => last(t)
  }
}

object P02 {

//  @scala.annotation.tailrec
  def penultimate[X](xs: List[X]): X = xs match {
    case Nil => throw new NoSuchElementException("penultimate: empty")
    case h :: _ :: Nil => h
    case _ :: h => penultimate(h)
  }
}

object P03 {

//  @scala.annotation.tailrec
  def kth[X](k: Int, xs: List[X]): X = (k, xs) match {
    case (n, _) if n < 0 => throw new NoSuchElementException("kth: error")
    case (0, h :: _) => h
    case (n, _ :: t) => kth(n - 1, t)
    case (_, _) => throw new NoSuchElementException("kth: error")
  }
}

object P04 {
  def length[X](xs: List[X]): Int = {
    @scala.annotation.tailrec
    def inner[X](r: List[X], l : Int): Int = r match {
      case Nil => l
      case _ :: t => inner(t, l + 1)
    }
    inner(xs, 0)
  }
}

object P05 {

  def reverse[X](xs: List[X]): List[X] = {
    @scala.annotation.tailrec
    def inner[X](r: List[X], l: List[X]): List[X] = r match {
      case Nil => l
      case h :: t => inner(t, h :: l)
    }
    inner(xs, Nil)
  }
}

object P06 {

  //@tailrec
  def isPalindrome[X](xs: List[X]): Boolean = {
    @scala.annotation.tailrec
    def reverse[X](r: List[X], l: List[X]): List[X] = r match {
      case Nil => l
      case h :: t => reverse(t, h :: l)
    }
    xs.equals(reverse(xs, Nil))
  }
}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny = xs match {
    case Nil => Nil
    case (h : ListAny) :: t => flatten(h) appendedAll flatten(t)
    case h :: t => h :: flatten(t)
  }
}

object P08 {

  def compress[X](xs: List[X]): List[X] = xs match {
      case Nil => Nil
      case h :: t => if (Nil != t && h == t.head) compress(t) else h :: compress(t)
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    // TO BE IMPLEMENTED
    def inner(xs: List[X], left: List[List[X]]) : List[List[X]] = (xs, left) match {
      case (Nil, r) => r
      case (h :: t, Nil) => inner(t, List(List(h)))
      case (h :: t, l :: r) => if (l != Nil && l.head == h) inner(t, (h :: l) :: r) else inner(t, List(h) :: left)
    }
    inner(xs, Nil).reverse
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] = {
    def inner(l: List[X], r: List[(Int, X)]): List[(Int, X)] = (l, r) match {
      case (Nil, r) => r
      case (h :: t, Nil) => inner(t, List((1 -> h)))
      case (h :: t, x -> y :: q) => if (h == y) inner(t, x + 1 -> y :: q) else inner(t, 1 -> h :: r)
    }
    inner(xs, Nil).reverse
  }
}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] = {
    def inner(l: List[X], r: List[Any]): List[Any] = (l, r) match {
      case (Nil, r) => r
      case (h :: t, Nil) => inner(t, List(h))
      case (h :: t, (x : Int) -> y :: q) => if (h == y) inner(t, 1 + x -> y :: q) else inner(t, h :: r)
      case (h :: t, p :: q) => if (h == p) inner(t, 2 -> p :: q) else inner(t, h :: r)
    }
    inner(xs, Nil).reverse
  } // TO BE IMPLEMENTED
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = xIs match {
    case Nil => Nil
    case x -> y :: t => if (x == 1) y :: decode(t) else y :: decode(x - 1 -> y :: t)
  }
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    def inner(l: List[X], r: List[(Int, X)]): List[(Int, X)] = (l, r) match {
      case (Nil, r) => r
      case (h :: t, Nil) => inner(t, List(1 -> h))
      case (h :: t, x -> y :: q) => if (h == y) inner(t, x + 1 -> y :: q) else inner(t, 1 -> h :: r)
    }
    inner(xs, Nil).reverse
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
    def encode(l: List[X], r: List[(Int, X)]): List[(Int, X)] = (l, r) match {
      case (Nil, r) => r
      case (h :: t, Nil) => encode(t, List(1 -> h))
      case (h :: t, x -> y :: q) => if (h == y) encode(t, x + 1 -> y :: q) else encode(t, 1 -> h :: r)
    }
    def double(l: List[(Int, X)]) : List[(Int, X)] = l match {
      case Nil => Nil
      case ((x : Int) -> y) :: t => ((2 * x) -> y) :: double(t)
    }

    def decode(l: List[(Int, X)]) : List[X] = l match {
      case Nil => Nil
      case 1 -> y :: t => y :: decode(t)
      case (x: Int) -> y :: t => y :: decode(x - 1 -> y :: t)
    }

    decode(double(encode(xs, Nil).reverse))
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
    def encode(l: List[X], r: List[(Int, X)]): List[(Int, X)] = (l, r) match {
      case (Nil, r) => r
      case (h :: t, Nil) => encode(t, List(1 -> h))
      case (h :: t, x -> y :: q) => if (h == y) encode(t, x + 1 -> y :: q) else encode(t, 1 -> h :: r)
    }
    def N(l: List[(Int, X)], n: Int) : List[(Int, X)] = (l, n) match {
      case (_, 0) => Nil
      case (Nil, n) => Nil
      case (((x : Int) -> y) :: t, n) => ((n * x) -> y) :: N(t, n)
    }

    def decode(l: List[(Int, X)]) : List[X] = l match {
      case Nil => Nil
      case 1 -> y :: t => y :: decode(t)
      case (x: Int) -> y :: t => y :: decode(x - 1 -> y :: t)
    }
    decode(N(encode(xs, Nil).reverse, n))
  }
}
