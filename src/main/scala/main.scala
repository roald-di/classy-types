package classytypes

import examples.*

@main
def main(): Unit =
  def example[F[_] : Monad &&& Traversable, A: Monoid](fa: F[A], a: A): F[A] =
    for
      x <- fa.traverse(a => Applicative[F].any.map(_ => a)).flatten
      y <- Monad[F].pure(a)
    yield x |+| y |+| Monoid[A].identity

  println(example(List(1, 2, 3), 4))
  println(example(Id(1), 4))
