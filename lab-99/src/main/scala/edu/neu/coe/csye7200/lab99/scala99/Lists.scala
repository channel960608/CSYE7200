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

  def flatten(xs: ListAny): ListAny = ???
//  {
//    def inner[X](l: ListAny, result: ListAny): ListAny = l match {
//      case h :: t => inner(h, Nil) ::: inner(t, Nil) ::: result
//      case e => result :+ e
//    }
//    inner(xs, Nil)
//  }
    // TO BE IMPLEMENTED
}

object P08 {

  def compress[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] = ??? // TO BE IMPLEMENTED
}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] = ??? // TO BE IMPLEMENTED
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = ??? // TO BE IMPLEMENTED
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}
