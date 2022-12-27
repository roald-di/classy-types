package classytypes
package examples
case class Id[A](value: A)
object Id:
  given Map[Id] with
    def map[A, B](f: A => B): Id[A] => Id[B] =
      w => Id(f(w.value))

  given Zip[Id] with
    def zip[A, B](a: Id[A], b: Id[B]): Id[(A, B)] =
      Id((a.value, b.value))

  given ZipIdentity[Id] with
    def any: Id[Any] =
      Id[Any](())

  given Join[Id] with
    def join[A](ffa: Id[Id[A]]): Id[A] =
      ffa.value

  given JoinIdentity[Id] with
    def pure[A](value: A): Id[A] =
      Id(value)

  given Traverse[Id] with
    def traverse[G[_], A, B](fa: Id[A], f: A => G[B])(using ops: Map[G] & Zip[G] & ZipIdentity[G]): G[Id[B]] =
      f(fa.value).map(Id(_))