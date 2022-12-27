package classytypes
package examples

import scala.annotation.{experimental, targetName}
import scala.language.experimental.erasedDefinitions

enum Ordering:
  case LT, EQ, GT

trait Equals[A] extends Typeclass:
  def eq(left: A, right: A): Boolean

object Equals:
  given Equals[Int] with
    def eq(left: Int, right: Int): Boolean = left == right

@experimental
trait PartialCompare[A](using erased Equals[A]) extends Typeclass:
  def partialCompare(left: A, right: A): Option[Ordering]

object PartialCompare:
  @experimental
  given PartialCompare[Int] with
    def partialCompare(left: Int, right: Int): Option[Ordering] =
      if left < right then Some(Ordering.LT)
      else if left > right then Some(Ordering.GT)
      else Some(Ordering.EQ)

@experimental
trait Compare[A](using erased PartialCompare[A]) extends Typeclass:
  def compare(left: A, right: A): Ordering

object Compare:
  @experimental
  given Compare[Int] with
    def compare(left: Int, right: Int): Ordering =
      if left < right then Ordering.LT
      else if left > right then Ordering.GT
      else Ordering.EQ



@experimental
type Eq[A] = Equals[A]

@experimental
type PartialOrd[A] = Eq[A] & PartialCompare[A]

@experimental
type Ord[A] = PartialOrd[A] & Compare[A]