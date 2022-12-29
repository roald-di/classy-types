package classytypes

import examples.*

def categoryExample(): Unit =
  def fancyAddition[F[_] : Monad &&& Traversable, A: Monoid](fa: F[A], a: A): F[A] =
    for
      x <- fa.traverse(a => Applicative[F].any.map(_ => a)).flatten
      y <- Monad[F].pure(a)
    yield x <> y <> Monoid[A].identity

  println(fancyAddition(List(1, 2, 3), 4))
  println(fancyAddition(Id(1), 4))


def newtypeAlternativeExample(): Unit =
  def fold[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].identity)(_ <> _)

  def foldShow[A: Monoid && Show](as: List[A]): String =
    as.foldLeft(Monoid[A].identity)(_ <> _).show

  val xs = List(Complex(1, 2), Complex(3, 4))

  println(fold(xs)(using AdditiveMonoid[Complex]).show)
  println(fold(xs)(using MultiplicativeMonoid[Complex]).show)

  given Monoid[Complex] = AdditiveMonoid[Complex]
  println(foldShow(xs))

  {
    given Monoid[Complex] = MultiplicativeMonoid[Complex]
    println(foldShow(xs))
  }

@main
def main(): Unit =
  categoryExample()
  newtypeAlternativeExample()


