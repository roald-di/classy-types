package classytypes

import scala.util.NotGiven

transparent trait Priority0
transparent trait Priority1
transparent trait Priority2
transparent trait Priority3
// ..
// 22 should be enough for everyone :)
transparent trait Priority22

trait Typeclass[Class[_]] extends Typeclass.Typeclass_2[Class]:
  given fromBase_3[A](using base: Base[A] & Priority3, disable: NotGiven[Disable[A]]): Class[A] =
    extract(base)

object Typeclass:
  trait Typeclass_2[Class[_]] extends Typeclass_1[Class]:
    given fromBase_2[A](using base: Base[A] & Priority2, disable: NotGiven[Disable[A]]): Class[A] =
      extract(base)

  trait Typeclass_1[Class[_]] extends Typeclass_0[Class]:
    given fromBase_1[A](using base: Base[A] & Priority1, disable: NotGiven[Disable[A]]): Class[A] =
      extract(base)

  trait Typeclass_0[Class[_]] extends Typeclass_Types[Class]:
    given fromBase_0[A](using base: Base[A] & Priority0, disable: NotGiven[Disable[A]]): Class[A] =
      extract(base)

  trait Typeclass_Types[Class[_]]:
    trait Disable[A]
    type Base[A]

    def extract[A](base: Base[A]): Class[A]


trait TypeclassF[Class[_[_]]] extends TypeclassF.TypeclassF_2[Class]:
  given fromBase_3[F[_]](using base: Base[F] & Priority3, disable: NotGiven[Disable[F]]): Class[F] =
    extract(base)

object TypeclassF:
  trait TypeclassF_2[Class[_[_]]] extends TypeclassF_1[Class]:
    given fromBase_2[F[_]](using base: Base[F] & Priority2, disable: NotGiven[Disable[F]]): Class[F] =
      extract(base)

  trait TypeclassF_1[Class[_[_]]]extends TypeclassF_0[Class]:
    given fromBase_1[F[_]](using base: Base[F] & Priority1, disable: NotGiven[Disable[F]]): Class[F] =
      extract(base)

  trait TypeclassF_0[Class[_[_]]] extends TypeclassF_Types[Class]:
    given fromBase_0[F[_]](using base: Base[F] & Priority0, disable: NotGiven[Disable[F]]): Class[F] =
      extract(base)

  trait TypeclassF_Types[Class[_[_]]]:
    trait Disable[F[_]]
    type Base[F[_]]

    def extract[F[_]](base: Base[F]): Class[F]

// etc..
trait TypeclassF2[Class[_[_, _]]]
trait TypeclassK[Class[_[_[_], _]]]