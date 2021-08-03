package classytypes

import scala.compiletime._
import scala.quoted._
import scala.util._

// Macro output example for resolve[Eq[Int]]:
// {
//   inline given Eq.Disable[Int] = error("Could not resolve typeclass instance Eq[Int]")
//   delayedSummonInline[Eq[Int]]
// }
//
// the implementation is a bit weird due to this and related issues: https://github.com/lampepfl/dotty/issues/12997
transparent inline def resolve[A]: A =
  ${resolveImpl[A]}

inline def delayedSummonInline[T] = summonInline[T]

def resolveImpl[A: Type](using quotes: Quotes): Expr[A] =

  import quotes.reflect._

  val typeclass = TypeRepr.of[A]
  val companion = typeclass.typeSymbol.companionModule
  val typeArguments = typeclass match
    case AppliedType(_, args) => args
    case _ => List.empty

  val disable = TypeSelect(Ref(companion), "Disable").tpe

  val symbol = Symbol.newMethod(Symbol.spliceOwner, "", disable.appliedTo(typeArguments), Flags.Given | Flags.Final | Flags.Inline, Symbol.noSymbol)
  val message = Expr(s"Could not resolve typeclass instance ${typeclass.show}")
  val body = '{error(${message})}.asTerm

  val disableConversion = DefDef(symbol, _ => Some(body))

  val summon = '{delayedSummonInline[A]}.asTerm
  (Block(List(disableConversion), summon).asExprOf[A])
