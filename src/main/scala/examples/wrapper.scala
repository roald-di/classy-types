package classytypes
package examples

case class Wrapper[A](value: A)

object Wrapper:

  given[A: Eq]: Eq[Wrapper[A]] with
    def equal(left: Wrapper[A], right: Wrapper[A]): Boolean = left.value === right.value

  given[A: PartialOrd]: PartialOrd[Wrapper[A]](resolve) with
    def comparePartial(left: Wrapper[A], right: Wrapper[A]): PartialOrdering = left.value =??= right.value

  given[A: Ord]: Ord[Wrapper[A]](resolve) with
    def compare(left: Wrapper[A], right: Wrapper[A]): Ordering = left.value =?= right.value

  given Bottom[Wrapper] with
    def bottomOp[A](fa: Wrapper[A]): Wrapper[A] = fa

  given Left[Wrapper](resolve) with
    def leftOp[A](fa: Wrapper[A]): Wrapper[A] = fa

  given Right[Wrapper](resolve) with
    def rightOp[A](fa: Wrapper[A]): Wrapper[A] = fa

  given Top[Wrapper](resolve, resolve) with
    def topOp[A](fa: Wrapper[A]): Wrapper[A] = fa