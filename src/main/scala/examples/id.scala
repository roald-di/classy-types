package classytypes
package examples

case class Id[A](value: A)
object Id:
  given Map[Id] with
    extension [A] (fa: Id[A]) def map[B](f: A => B): Id[B] = Id(f(fa.value))

  given Zip[Id] with
    extension [A] (fa: Id[A]) def zip[B](fb: Id[B]): Id[(A, B)] = Id((fa.value, fb.value))

  given ZipIdentity[Id] with
    def any: Id[Any] = Id[Any](())

  given Join[Id] with
    extension [A] (fa: Id[Id[A]]) def join: Id[A] = fa.value

  given JoinIdentity[Id] with
    def pure[A](value: A): Id[A] = Id(value)

  given Traverse[Id] with
    extension [A] (fa: Id[A])
      def traverse[G[_], B](f: A => G[B])(using ops: Map[G] & Zip[G] & ZipIdentity[G]): G[Id[B]] =
        f(fa.value).map(Id(_))