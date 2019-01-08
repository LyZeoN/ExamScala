package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS
import scala.annotation.tailrec


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2

  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(x) => Option(mul2(x))
    case _       => Option.empty
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = i.map(x => mul2(x))

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat          extends Animal
  case object Bird         extends Animal
  case class Dog(age: Int) extends Animal

  def formatAnimal(animal: Animal): String =
    animal match {
      case Cat    => "It's a cat"
      case Bird   => "It's a bird"
      case x: Dog => s"It's a ${x.age} year old dog"
      case _      => "This should not happen but I'm a Java developer !"
    }

  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = {
    @tailrec
    def index(i: List[A],a:A, in: Int): Option[Int] = {
      i match {
        case Nil => Option.empty
        case x :: tail =>
          if (x.equals(a)){
            Option(in)
          } else {
            index(tail,a,in+1)
          }
      }
    }
    index(l, a,0)
  }

  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)
  def keepValid[A](l: List[Either[Error, A]]): List[A] = l.flatMap(_.right.toOption)

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = {
    if (l.nonEmpty) {
      l.reduce(combine)
    } else {
      empty
    }
  }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = {
    val list =  keepValid(l)
    aggregate(list,combine,empty)
  }

  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  def aggregateValidM[A] (l: List[Either[Error, A]], combine: (A, A) => A, empt: A)  :Monoid[A] = new Monoid[A] {
    override def empty: A               = empt
    override def combine(x: A, y: A): A = combine(x,y)
  }


  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)

  implicit val monoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x+y
  }

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)

  sealed trait FinancialAsset extends Earnings{
    def computeEarnings: Double
  }

  class FlatRateAsset (protected val rate: Double,protected val amount: Double) extends FinancialAsset with Earnings {
    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  final case class LivretA(override protected val rate : Double = LivretA.Rate,override val amount: Double) extends FlatRateAsset (rate,amount)


  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  final case class Pel(override protected val rate : Double = Pel.Rate, override val amount: Double, creation: Instant) extends FlatRateAsset (rate,amount) with Earnings{
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }
  final case class CarSale(amount: Int, horsePower: Int) extends FinancialAsset with Earnings{
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  trait Earnings{
    def computeEarnings:Double
  }

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[Earnings]): Double =
    assets.map(_.computeEarnings).sum

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)
  object StringUtils {
    implicit class StringImprovements(val s: String) {
      def atoi(n: Int):String = s.substring(0,n)
    }
  }
}
