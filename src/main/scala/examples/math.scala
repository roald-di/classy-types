package classytypes
package examples

import scala.annotation.targetName
import scala.compiletime.summonInline

trait Combine[A] extends Typeclass:
  extension (left: A) def <>(right: A): A

object Combine:
  given Combine[Int] with
    extension (left: Int) def <>(right: Int): Int = left + right

trait Additive[A] extends Combine[A]:
  extension (left: A) def |+|(right: A): A = left <> right

trait Multiplicative[A] extends Combine[A]:
  extension (left: A) def |*|(right: A): A = left <> right

trait CombineIdentity[A] extends Typeclass:
  def identity: A

object CombineIdentity:
  given CombineIdentity[Int] with
    def identity: Int = 0

trait AdditiveIdentity[A] extends CombineIdentity[A]
trait MultiplicativeIdentity[A] extends CombineIdentity[A]

type Semigroup[A] = Combine[A]
object Semigroup extends Typeclass.Companion[Semigroup]

type AdditiveSemigroup[A] = Additive[A]
object AdditiveSemigroup extends Typeclass.Companion[AdditiveSemigroup]

type MultiplicativeSemigroup[A] = Multiplicative[A]
object MultiplicativeSemigroup extends Typeclass.Companion[MultiplicativeSemigroup]

type Monoid[A] = Semigroup[A] & CombineIdentity[A]
object Monoid extends Typeclass.Companion[Monoid]

type AdditiveMonoid[A] = AdditiveSemigroup[A] & AdditiveIdentity[A]
object AdditiveMonoid extends Typeclass.Companion[AdditiveMonoid]

type MultiplicativeMonoid[A] = MultiplicativeSemigroup[A] & MultiplicativeIdentity[A]
object MultiplicativeMonoid extends Typeclass.Companion[MultiplicativeMonoid]

trait Show[A] extends Typeclass:
  extension (a: A)
    def show: String

case class Complex(real: Double, imaginary: Double)

object Complex:
  given Additive[Complex] with
    extension (left: Complex) def <>(right: Complex): Complex =
      Complex(left.real + right.real, left.imaginary + right.imaginary)

  given Multiplicative[Complex] with
    extension (left: Complex) def <>(right: Complex): Complex =
      Complex(left.real * right.real - left.imaginary * right.imaginary, left.real * right.imaginary + left.imaginary * right.real)

  given AdditiveIdentity[Complex] with
    def identity: Complex = Complex(0, 0)

  given MultiplicativeIdentity[Complex] with
    def identity: Complex = Complex(1, 0)

  given Show[Complex] with
    extension (c: Complex)
      def show: String = s"${c.real} + ${c.imaginary}i"
