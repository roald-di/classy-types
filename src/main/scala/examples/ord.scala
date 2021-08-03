package classytypes
package examples

// Eq
trait Eq[A]:
  def equal(left: A, right: A): Boolean

extension [A](self: A)(using instance: Eq[A])
  infix def === (right: A) = instance.equal(self, right)

object Eq extends Typeclass[Eq]:

  transparent trait Base[A]:
    def eq: Eq[A]

  def extract[A](base: Base[A]): Eq[A] = base.eq

  given Eq[Int] with
    def equal(left: Int, right: Int): Boolean =
      left == right

// PartialOrd
enum PartialOrdering:
  case Equal
  case Smaller
  case Larger
  case Incomparable

trait PartialOrd[A](base: Eq[A]) extends Eq.Base[A] with Priority0:
  val eq = base
  export base.*

  def comparePartial(left: A, right: A): PartialOrdering

extension [A](self: A)(using e: PartialOrd[A])
  infix def =??= (right: A) = e.comparePartial(self, right)

object PartialOrd extends Typeclass[PartialOrd]:
  trait Test

  transparent trait Base[A] extends Eq.Base[A]:
    def partialOrd: PartialOrd[A]

  def extract[A](base: Base[A]): PartialOrd[A] = base.partialOrd

  given PartialOrd[Int](resolve) with
    def comparePartial(left: Int, right: Int): PartialOrdering = PartialOrdering.Equal

// Ord
enum Ordering:
  case Equal
  case Smaller
  case Larger

trait Ord[A](base: PartialOrd[A]) extends PartialOrd.Base[A] with Priority1:
  val partialOrd = base
  export base.*

  def compare(left: A, right: A): Ordering

extension [A](self: A)(using e: Ord[A])
  infix def =?= (right: A) = e.compare(self, right)

object Ord extends Typeclass[Ord]:

  transparent trait Base[A] extends PartialOrd.Base[A]:
    def ord: Ord[A]

  def extract[A](base: Base[A]): Ord[A] = base.ord

  given Ord[Int](resolve) with
    def compare(left: Int, right: Int): Ordering = Ordering.Equal

