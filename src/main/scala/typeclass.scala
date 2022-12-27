package classytypes

import scala.annotation.targetName

trait Typeclass
object Typeclass :
  inline given [A <: Typeclass] : A =
    summonTypeclass[A]

@targetName("intersect")
type &&[F[_], G[_]] = [A] =>> F[A] & G[A]

@targetName("intersectF")
type &&&[F[_[_]], G[_[_]]] = [A[_]] =>> F[A] & G[A]