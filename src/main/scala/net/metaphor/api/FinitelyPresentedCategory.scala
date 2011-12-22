package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategory[O, M, C] { self: C =>
  def relations(source: O, target: O): List[M]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[M] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  // FIXME implement toString, hashcode, equals

  trait WithTerminalObject extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C => }
  val adjoinTerminalObject: WithTerminalObject

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def colimit = functorsToSet.colimit(functorToSet)
    def colimitFunctor = colimit.initialObject.asInstanceOf[adjoinTerminalObject.FunctorToSet]
    def colimitSet = {
      val c = colimitFunctor
      c(c.source.asInstanceOf[TerminalObject[O, M]].terminalObject)
    }

    trait CoCone extends adjoinTerminalObject.FunctorToSet {
      def terminalSet: Set
      def mapToTerminalSet(o: O): Function

      override def onObjects(o: O): Set = {
        if (o == adjoinTerminalObject.terminalObject) {
          terminalSet
        } else {
          functorToSet(o)
        }
      }
      override def onMorphisms(m: M): Function = {
        if (self.target(m) == adjoinTerminalObject.terminalObject) {
          mapToTerminalSet(self.source(m))
        } else {
          functorToSet(m)
        }
      }
    }
    trait CoConeMap extends adjoinTerminalObject.NaturalTransformationToSet[CoCone] {
      def terminalMap: Function

      override def apply(o: O) = {
        if (o == adjoinTerminalObject.terminalObject) {
          terminalMap
        } else {
          functorToSet(o).identity
        }
      }
    }

  }

  // TODO Lift these higher? Tried once, and ran into some trouble; making functor categories have these types gets confusing fast.
  // Alternatively, we should drop FunctorToSet stuff down below the most general Category level.
  type F <: FunctorToSet
  type T <: NaturalTransformationToSet[F]
  type CSets <: FunctorsToSet[F, T, CSets]

  val functorsToSet: CSets

  abstract class FunctorsToSet[F <: FunctorToSet, T <: NaturalTransformationToSet[F], FC <: FunctorsToSet[F, T, FC]] extends super.FunctorsToSet[F, T, FC] { functorsToSet: FC =>

    def colimit(functor: self.FunctorToSet): InitialObject[functor.CoCone, functor.CoConeMap] = {

      // This is where all the work happens.
      def concreteColimit[A](objects: Iterable[O], sets: O => Iterable[A], functions: O => (O => (A => Iterable[A]))): (Iterable[List[(O, A)]], O => (A => List[(O, A)])) = {
        /**
         * finds all the clumps containing an element of slice, and smushes them together
         */
        def combineClumps[B](clumps: Iterable[List[B]], slice: List[B]): Iterable[List[B]] = {
          val (toCombine, toLeave) = clumps.partition(_.find(a => slice.contains(a)).nonEmpty)
          toLeave ++ List(toCombine.flatten.toList)
        }
        val initialClumps = for (o <- objects; x <- sets(o)) yield List((o, x))
        val arrows = for (s <- objects; x <- sets(s); t <- objects; y <- functions(s)(t)(x)) yield List((s, x), (t, y))

        val resultClumps = arrows.foldLeft(initialClumps)(combineClumps _)
        def resultFunctions(o: O)(a: A) = resultClumps.find(_.contains((o, a))).get

        (resultClumps, resultFunctions _)
      }

      val (clumps, functions) = concreteColimit(
        objects,
        { o: O => functor(o).toIterable },
        { s: O => { t: O => { a: Any => for (g <- generators(s, t)) yield functor(g).toFunction(a) } } })

      val resultSet = new Set {
        override def toIterable = clumps
      }
      val resultFunctions: (O => Function) = { o: O =>
        new Function {
          override def toFunction = functions(o)
        }
      }

      new InitialObject[functor.CoCone, functor.CoConeMap] {
        def initialObject = new functor.CoCone {
          override def terminalSet = resultSet
          override def mapToTerminalSet(o: O) = resultFunctions(o)
        }
        def morphismTo(other: functor.CoCone) = {
          new functor.CoConeMap {
            override def source = initialObject
            override def target = other
            override def terminalMap = ???
          }
        }
      }
    }

  }
}


trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategories[O, M, C] { FPCAT =>
//  //  class PullbackTwoFunctor extends CategoricalTwoFunctor[O, M, C, HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] {
//  //    def source = FPCAT
//  //    def target = ???
//  //
//  //    def onZeroMorphisms(m0: C) = new FunctorsToSet[O, M, C](m0)
//  //    def onOneMorphisms(m1: Functor[O, M, C]): Functor[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackFunctor[O, M, C, Set, Function, Sets](m1, Sets)
//  //    def onTwoMorphisms(m2: NaturalTransformation[O, M, C]): NaturalTransformation[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackNaturalTransformation[O, M, C, Set, Function, Sets](m2, Sets)
//  //  }
}

