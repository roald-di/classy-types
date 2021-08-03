package classytypes

import examples.*

@main
def main() =

  summon[Bottom[Wrapper]]
  summon[Left[Wrapper]]
  summon[Right[Wrapper]]
  summon[Top[Wrapper]]

  def diamond[F[_] : Top](fa: F[Int]) =
    summon[Bottom[F]].bottomOp(fa)

    summon[Left[F]].bottomOp(fa)
    summon[Left[F]].leftOp(fa)

    summon[Right[F]].bottomOp(fa)
    summon[Right[F]].rightOp(fa)

    summon[Top[F]].bottomOp(fa)
    summon[Top[F]].leftOp(fa)
    summon[Top[F]].rightOp(fa)
    summon[Top[F]].topOp(fa)

  def diamond2[F[_]: Left: Right] =
    summon[Bottom[F]]

  diamond(Wrapper(8))
  diamond2[Wrapper]

  summon[Eq[Int]]
  summon[PartialOrd[Int]]
  summon[Ord[Int]]

  summon[Eq[Wrapper[Int]]]
  summon[PartialOrd[Wrapper[Int]]]
  summon[Ord[Wrapper[Int]]]

  summon[Functor[Id]]
  summon[Applicative[Id]]
  summon[Traversable[Id]]
  summon[Monad[Id]]

  def category[F[_]: Monad: Traversable](fa: F[Int]) =
    summon[Functor[F]]
    summon[Applicative[F]]
    summon[Monad[F]]
    summon[Traversable[F]]

    fa.zip(8.pure).bind(_.pure[F]).traverse(Option(_)).map(_.map(identity))

  category(Id(8))
  
  def single[A: Ord]: Unit =
    summon[Eq[A]]
    summon[PartialOrd[A]]
    summon[Ord[A]]

  def many[A: PartialOrd : Ord]: Unit =
    summon[Eq[A]]
    summon[PartialOrd[A]]
    summon[Ord[A]]

  def all[A: Eq : PartialOrd : Ord]: Unit =
    summon[Eq[A]]
    summon[PartialOrd[A]]
    summon[Ord[A]]

  def extensions[A: Ord](left: A, right: A) =
    (left === right) &&
    (left =??= right) == PartialOrdering.Equal &&
    (left =?= right) == Ordering.Equal

  single[Int]
  many[Int]
  all[Int]
  extensions(0, 1)

  println("ok!")
