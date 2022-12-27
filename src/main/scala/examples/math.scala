package classytypes
package examples

import scala.annotation.targetName
import scala.compiletime.summonInline

trait Combine[A] extends Typeclass:
  def combine(left: A, right: A): A

  extension (left: A)
    @targetName("combine_op")
    def |+|(right: A): A = combine(left, right)


object Combine:
  given Combine[Int] with
    def combine(left: Int, right: Int): Int = left + right


trait CombineIdentity[A] extends Typeclass:
  def identity: A

object CombineIdentity:
  given CombineIdentity[Int] with
    def identity: Int = 0


type Semigroup[A] = Combine[A]
object Semigroup:
  inline def apply[A]: Semigroup[A] = summonInline[Semigroup[A]]

type Monoid[A] = Semigroup[A] & CombineIdentity[A]
object Monoid:
  inline def apply[A]: Monoid[A] = summonInline[Monoid[A]]