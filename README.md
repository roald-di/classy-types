# Better typclasses for Scala 3

This is an attempt to fix some problems with the way typclasses are encoded in scala.  
Based on an excellent idea by [@regiskuckaertz](https://github.com/regiskuckaertz).

#### Example typeclass declaration

```scala
trait Map[F[_]] extends Typeclass:
  def map[A, B](f: A => B): F[A] => F[B]

trait Zip[F[_]] extends Typeclass:
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

trait ZipIdentity[F[_]] extends Typeclass:
  def any: F[Any]

trait Join[F[_]] extends Typeclass:
  def join[A](ffa: F[F[A]]): F[A]

trait JoinIdentity[F[_]] extends Typeclass:
  def pure[A](value: A): F[A]

type Functor[F[_]] = Map[F]
type Applicative[F[_]] = Functor[F] & Zip[F] & ZipIdentity[F]
type Monad[F[_]] = Applicative[F] & Join[F] & JoinIdentity[F]

```



#### Example instance declarations

```scala
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
```

#### And the following works too!

```scala
def infamous[F[_] : Monad &&& Traversable](fa: F[Int]) =
  fa.map(_ + 1)
```

### More info

https://contributors.scala-lang.org/t/encoding-type-class-hierarchies/4626/6   
https://github.com/zio/zio-prelude/issues/485


#### Run with

```scala
sbt run
```