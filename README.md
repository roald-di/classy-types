# Better typclasses for Scala 3

This is an attempt to fix some problems with the way typclasses are encoded in scala.  
Based on an excellent idea by [@regiskuckaertz](https://github.com/regiskuckaertz).

#### Example typeclass declaration

```scala
trait Ord[A](base: PartialOrd[A]) extends PartialOrd.Base[A] with Priority1:
  val partialOrd = base
  export base._

  def compare(left: A, right: A): Ordering

extension [A](self: A)(using e: Ord[A])
  infix def =?= (right: A) = e.compare(self, right)

object Ord extends Typeclass[Ord]:

  transparent trait Base[A] extends PartialOrd.Base[A]:
    def ord: Ord[A]

  def extract[A](base: Base[A]): Ord[A] = base.ord

  given Ord[Int](resolve) with
    def compare(left: Int, right: Int): Ordering = Ordering.Equal
```



#### Example instance declarations

```scala
case class Wrapper[A](value: A)

object Wrapper:

  given[A: Eq]: Eq[Wrapper[A]] with
    def equal(left: Wrapper[A], right: Wrapper[A]): Boolean = left.value === right.value

  given[A: PartialOrd]: PartialOrd[Wrapper[A]](resolve) with
    def comparePartial(left: Wrapper[A], right: Wrapper[A]): PartialOrdering = left.value =??= right.value

  given[A: Ord]: Ord[Wrapper[A]](resolve) with
    def compare(left: Wrapper[A], right: Wrapper[A]): Ordering = left.value =?= right.value

```

#### And the following works too!

```scala
def infamous[F[_]: Monad: Traversable](fa: F[Int]) =
  fa.map(_ + 1)
```

### More info

https://contributors.scala-lang.org/t/encoding-type-class-hierarchies/4626/6   
https://github.com/zio/zio-prelude/issues/485


#### Run with

```
mill root
```

or 

```scala
sbt run
```