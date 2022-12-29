package classytypes
package examples

import scala.annotation.targetName
import scala.collection.immutable
import scala.compiletime.summonInline

trait Map[F[_]] extends Typeclass:
  extension[A] (fa: F[A]) def map[B](f: A => B): F[B]

object Map:
  given Map[List] with
    extension[A] (fa: List[A]) def map[B](f: A => B): List[B] = fa.map(f)


trait Zip[F[_]] extends Typeclass:
  extension[A] (fa: F[A]) def zip[B](fb: F[B]): F[(A, B)]

object Zip:
  given Zip[List] with
    extension[A] (fa: List[A]) def zip[B](fb: List[B]): List[(A, B)] =
      fa.flatMap(a => fb.map(b => (a, b)))

trait Join[F[_]] extends Typeclass:

  extension[A] (fa: F[F[A]])
    def join: F[A]
    final def flatten: F[A] = join

  extension[A] (fa: F[A])
    def flatMap[B](f: A => F[B])(using map: Map[F]): F[B] =
      fa.map(f).join

object Join:
  given Join[List] with
    extension[A] (fa: List[List[A]]) def join: List[A] = fa.flatten

trait ZipIdentity[F[_]] extends Typeclass:
  def any: F[Any]

object ZipIdentity:
  given ZipIdentity[List] with
    def any: List[Any] = List(())

trait JoinIdentity[F[_]] extends Typeclass:
  def pure[A](value: A): F[A]

object JoinIdentity:
  given JoinIdentity[List] with
    def pure[A](value: A): List[A] = List(value)

trait Traverse[F[_]] extends Typeclass:
  extension[A] (fa: F[A]) def traverse[G[_] : Map &&& Zip &&& ZipIdentity, B](f: A => G[B]): G[F[B]]

object Traverse:
  given Traverse[List] with
    extension[A] (fa: List[A])
      def traverse[G[_], B](f: A => G[B])(using ops: Map[G] & Zip[G] & ZipIdentity[G]): G[List[B]] =
        val empty = ops.any.map(_ => List.empty[B])
        fa.foldRight(empty)((a, bs) => f(a).zip(bs).map(_ :: _))


type Functor[F[_]] = Map[F]
object Functor extends Typeclass.CompanionF[Functor]

type Applicative[F[_]] = Functor[F] & Zip[F] & ZipIdentity[F]
object Applicative extends Typeclass.CompanionF[Applicative]

type Monad[F[_]] = Applicative[F] & Join[F] & JoinIdentity[F]
object Monad extends Typeclass.CompanionF[Monad]

type Traversable[F[_]] = Functor[F] & Traverse[F]
object Traversable extends Typeclass.CompanionF[Traversable]