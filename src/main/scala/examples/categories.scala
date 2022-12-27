package classytypes
package examples

import scala.annotation.targetName
import scala.collection.immutable
import scala.compiletime.summonInline

trait Map[F[_]] extends Typeclass:
  self =>
  def map[A, B](f: A => B): F[A] => F[B]
  extension[A] (fa: F[A])
    def map[B](f: A => B): F[B] = self.map(f)(fa)

object Map:
  given Map[List] with
    def map[A, B](f: A => B): List[A] => List[B] = _.map(f)


trait Zip[F[_]] extends Typeclass:
  self =>
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  extension[A] (fa: F[A])
    @targetName("zip_ext")
    def zip[B](fb: F[B]): F[(A, B)] = self.zip(fa, fb)

object Zip:
  given Zip[List] with
    def zip[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa.flatMap(a => fb.map(b => (a, b)))

trait Join[F[_]] extends Typeclass:
  self =>
  def join[A](ffa: F[F[A]]): F[A]

  extension[A] (fa: F[A])
    def flatMap[B](f: A => F[B])(using map: Map[F]): F[B] =
      self.join(map.map(f)(fa))

  extension[A] (fa: F[F[A]])
    def flatten: F[A] = self.join(fa)

object Join:
  given Join[List] with
    def join[A](ffa: List[List[A]]): List[A] = ffa.flatten

trait ZipIdentity[F[_]] extends Typeclass:
  def any: F[Any]

object ZipIdentity:
  given ZipIdentity[List] with
    def any: List[Any] = List(())

trait JoinIdentity[F[_]] extends Typeclass:
  def pure[A](value: A): F[A]


object JoinIdentity:
  given joinIdentityList: JoinIdentity[List] with
    def pure[A](value: A): List[A] = List(value)

trait Traverse[F[_]] extends Typeclass:
  self =>
  def traverse[G[_]: Map &&& Zip &&& ZipIdentity, A, B](fa: F[A], f: A => G[B]): G[F[B]]
  extension [A] (fa: F[A])
    @targetName("traverse_ext")
    def traverse[G[_]: Map &&& Zip &&& ZipIdentity, B](f: A => G[B]): G[F[B]] = self.traverse(fa, f)

object Traverse:
  given Traverse[List] with
    def traverse[G[_], A, B](fa: List[A], f: A => G[B])(using ops: Map[G] & Zip[G] & ZipIdentity[G]): G[List[B]] =
      val empty = ops.any.map(_ => List.empty[B])
      fa.foldRight(empty)((a, bs) => f(a).zip(bs).map(_ :: _))


type Functor[F[_]] = Map[F]
object Functor:
  inline def apply[F[_]]: Functor[F] = summonInline

type Applicative[F[_]] = Functor[F] & Zip[F] & ZipIdentity[F]
object Applicative:
  inline def apply[F[_]]: Applicative[F] = summonInline

type Monad[F[_]] = Applicative[F] & Join[F] & JoinIdentity[F]
object Monad:
  inline def apply[F[_]]: Monad[F] = summonInline

type Traversable[F[_]] = Functor[F] & Traverse[F]
object Traversable:
  inline def apply[F[_]]: Traversable[F] = summonInline