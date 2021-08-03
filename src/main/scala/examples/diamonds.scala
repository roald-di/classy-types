package classytypes
package examples

trait Bottom[F[_]]:
  def bottomOp[A](fa: F[A]): F[A]

object Bottom extends TypeclassF[Bottom]:

  transparent trait Base[F[_]]:
    def bottom: Bottom[F]

  def extract[F[_]](base: Base[F]): Bottom[F] = base.bottom


trait Left[F[_]](base: Bottom[F]) extends Bottom.Base[F] with Priority0:
  val bottom = base
  export base.*

  def leftOp[A](fa: F[A]): F[A]

object Left extends TypeclassF[Left]:

  transparent trait Base[F[_]] extends Bottom.Base[F]:
    def left: Left[F]

  def extract[F[_]](base: Base[F]): Left[F] = base.left

trait Right[F[_]](base: Bottom[F]) extends Bottom.Base[F] with Priority1:
  val bottom = base
  export base.*

  def rightOp[A](fa: F[A]): F[A]

object Right extends TypeclassF[Right]:

  transparent trait Base[F[_]] extends Bottom.Base[F]:
    def right: Right[F]

  def extract[F[_]](base: Base[F]): Right[F] = base.right

trait Top[F[_]](l: Left[F], r: Right[F]) extends Left.Base[F] with Right.Base[F] with Priority2:
  val left = l
  val right = r
  export l.*
  export r.{bottom as _, bottomOp as _, *}

  def topOp[A](fa: F[A]): F[A]


object Top extends TypeclassF[Top]:

  transparent trait Base[F[_]] extends Left.Base[F] with Right.Base[F]:
    def top: Top[F]

  def extract[F[_]](base: Base[F]): Top[F] = base.top