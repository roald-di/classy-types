package classytypes

import scala.quoted.*
import scala.annotation.experimental

inline def summonTypeclass[A]: A = ${Macro.summonTypeclassMacro[A]}

object Macro:
  @experimental
  def summonTypeclassMacro[A: Type](using quotes: Quotes): Expr[A] =
    import quotes.reflect._

    case class Export(target: ValDef):
      val methods: List[Symbol] = target.symbol.methodMembers
        .filterNot(_.isClassConstructor)
        .filter(_.flags.is(Flags.Deferred))
        .filterNot(_.flags.is(Flags.Private | Flags.Protected | Flags.PrivateLocal | Flags.Synthetic))


    def newOverride(cls: Symbol, method: Symbol) =
      Symbol.newMethod(
        cls,
        method.name,
        cls.typeRef.memberType(method),
        Flags.Method | Flags.Override | Flags.Final | (method.flags & Flags.ExtensionMethod),
        Symbol.noSymbol)

    def applyAll(target: Term, parameters: List[List[Tree]]) =
      parameters.foldLeft(target: Term)((target, section) =>
        val (values, types) = section.partitionMap({
          case parameter@Ident(_) => Left(parameter.underlying)
          case parameter@Inferred() => Right(parameter)
        })

        if values.nonEmpty then
          Apply(target, values)
        else if types.nonEmpty then
          TypeApply(target, types)
        else
          target
      )

    def splitIntersectionType(tpe: TypeRepr): List[TypeRepr] =
      val result = scala.collection.mutable.Set.empty[TypeRepr]

      def go(tpe: TypeRepr): Unit =
        tpe.dealias match {
          case branch: AndType =>
            go(branch.left)
            go(branch.right)
          case leaf  => result.add(leaf)
        }

      go(tpe)

      result.toList

    val types = splitIntersectionType(TypeRepr.of[A])

    val exports = types.zipWithIndex.map { (tpe, i) =>
      val value = Implicits.search(tpe) match
        case result: ImplicitSearchSuccess =>
          result.tree
        case error: ImplicitSearchFailure =>
          report.errorAndAbort(s"Could not resolve typeclass ${tpe.show(using Printer.TypeReprAnsiCode)}: ${error.explanation}")

      val target = ValDef(Symbol.newVal(Symbol.spliceOwner, s"implementation$i", tpe, Flags.EmptyFlags, Symbol.noSymbol), Some(value))

      Export(target)
    }

    def decls(cls: Symbol): List[Symbol] =
      exports.flatMap(_.methods).map(newOverride(cls, _))

    val name = types.map(_.typeSymbol.name).mkString("_")
    val parents = TypeRepr.of[Any] +: types
    val proxySymbol = Symbol.newClass(Symbol.spliceOwner, name, parents, decls, None)

    val body = exports.flatMap(exp =>
      val ref = Ref(exp.target.symbol)
      exp.methods.map(method => DefDef(proxySymbol.declaredMethod(method.name).head, params => Some(applyAll(Select(ref, method), params))))
    )

    val proxyClass = ClassDef(proxySymbol, parents.map(Inferred(_)), body)
    val ctor = Select(New(TypeIdent(proxySymbol)), proxySymbol.primaryConstructor)
    val newClass = Typed(Apply(ctor, Nil), TypeTree.of[A])

    val valDefs = exports.map(_.target)

    Block(valDefs :+ proxyClass, newClass).asExprOf[A]