package classytypes

import scala.annotation.targetName
import scala.compiletime.summonInline

trait Typeclass
object Typeclass :
  inline given [T <: Typeclass] : T =
    summonTypeclass[T]

  trait Companion[T[_] <: Typeclass] :
    inline def apply[A] : T[A] = summonInline

  trait CompanionF[T[_[_]] <: Typeclass]:
    inline def apply[F[_]]: T[F] = summonInline

@targetName("intersect")
type &&[F[_], G[_]] = [A] =>> F[A] & G[A]

@targetName("intersectF")
type &&&[F[_[_]], G[_[_]]] = [A[_]] =>> F[A] & G[A]