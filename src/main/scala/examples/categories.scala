package classytypes
package examples

trait Functor[F[_]]:
  def map[A, B](f: A => B): F[A] => F[B]

object Functor extends TypeclassF[Functor]:
  transparent trait Base[F[_]]:
    def functor: Functor[F]

  def extract[F[_]](base: Base[F]): Functor[F] = base.functor

  given Functor[Option] with
    override def map[A, B](f: A => B): Option[A] => Option[B] =
      _.map(f)

extension [F[_], A](self: F[A])(using instance: Functor[F])
  def map[B](f: A => B): F[B] = instance.map(f)(self)


trait Applicative[F[_]](base: Functor[F]) extends Functor.Base[F] with Priority0:
  val functor = base
  export base.*

  def unit: F[Any]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

extension [F[_], A](self: F[A])(using instance: Applicative[F])
  def zip[B](fb: F[B]) = instance.zip(self, fb)

extension [A](self: A)
  def pure[F[_]](using instance: Applicative[F]): F[A] =
    instance.unit.map(_ => self)

object Applicative extends TypeclassF[Applicative]:
  transparent trait Base[F[_]] extends Functor.Base[F]:
    def applicative: Applicative[F]

  def extract[F[_]](base: Base[F]): Applicative[F] = base.applicative

  given Applicative[Option](resolve) with Monad.ApplicativeDefaults[Option] with
    def unit: Option[Any] = Some(())

trait Monad[F[_]](base: Applicative[F]) extends Applicative.Base[F] with Priority1:
  val applicative = base
  export base.*

  def join[A](self: F[F[A]]): F[A]

extension [F[_], A](self: F[F[A]])(using instance: Monad[F])
  def join: F[A] = instance.join(self)

extension [F[_]: Monad, A](self: F[A])
  def bind[B](f: A => F[B]): F[B] =
    self.map(f).join

object Monad extends TypeclassF[Monad]:
  transparent trait Base[F[_]] extends Applicative.Base[F]:
    def monad: Monad[F]

  def extract[F[_]](base: Base[F]): Monad[F] = base.monad

  trait ApplicativeDefaults[F[_]](using monad: Monad[F]) extends Applicative[F]:
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      fa.bind(a => fb.map(b => (a, b)))

  given Monad[Option](resolve) with
    def join[A](fa: Option[Option[A]]): Option[A] =
      fa.flatMap(identity)

trait Traversable[F[_]](base: Functor[F]) extends Functor.Base[F] with Priority2:
  val functor = base
  export base.*

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

object Traversable extends TypeclassF[Traversable]:
  transparent trait Base[F[_]] extends Functor.Base[F]:
    def traversable: Traversable[F]

  def extract[F[_]](base: Base[F]): Traversable[F] = base.traversable

extension [F[_], A](self: F[A])(using instance: Traversable[F])
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = instance.traverse(self)(f)

type Id[A] >: A <: A

object Id:
  def apply[A](value: A): Id[A] = value

  given Functor[Id] with
    def map[A, B](f: A => B): Id[A] => Id[B] = f

  given Applicative[Id](resolve) with Monad.ApplicativeDefaults[Id] with
    def unit: Id[Any] = ()

  given Monad[Id](resolve) with
    def join[A](fa: Id[Id[A]]): Id[A] = fa

  given Traversable[Id](resolve) with
    def traverse[G[_]: Applicative, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] = f(fa)