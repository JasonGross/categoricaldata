package net.metaphor.api

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[M] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g
}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Categories[O, M, C] { fgCAT =>
}

trait FinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategory[O, M, C] { self: C =>
  def relations(source: O, target: O): List[M]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[M] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  // FIXME implement toString, hashcode, equals

  // TODO  
  def wordsOfLength(k: Int)(source: O, target: O) = ???
  def reducedWordsOfLength(k: Int)(source: O, target: O) = ???

  trait WithTerminalObject extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C => }
  val adjoinTerminalObject: WithTerminalObject

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def colimit = functorsToSet.colimit(functorToSet).initialObject.asInstanceOf[adjoinTerminalObject.FunctorToSet]
    def colimitSet = {
      val c = colimit
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
    trait CoConeMap extends adjoinTerminalObject.NaturalTransformationToSet {
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

  def morphismsFrom(s: O): FunctorToSet = new FunctorToSet {
    def onObjects(t: O) = ???
    def onMorphisms(m: M) = ???
  }
  
  val functorsToSet: FunctorsToSet

  class FunctorsToSet extends super.FunctorsToSet {

    def colimit(functor: self.FunctorToSet): InitialObject[functor.CoCone, functor.CoConeMap] = {
      /**
       * finds all the clumps containing an element of slice, and smushes them together
       */
      def combineClumps[A](clumps: List[List[A]], slice: List[A]): List[List[A]] = {
        val (toCombine, toLeave) = clumps.partition(_.find(a => slice.contains(a)).nonEmpty)
        toCombine.flatten :: toLeave
      }

      val initialClumps = for (
        o <- self.objects;
        x <- functor(o).toIterable
      ) yield List((o, x))
      val arrows = for (
        m <- self.allGenerators;
        s = self.source(m);
        t = self.target(m);
        x <- functor(s).toIterable
      ) yield List((s, x), (t, functor(m).toFunction(x)))

      lazy val clumps = arrows.foldLeft(initialClumps)(combineClumps _)

      val resultSet = new Set {
        override def toIterable = clumps.map(_.toString)
      }
      val resultFunctions: (O => Function) = { o: O =>
        new Function {
          override def toFunction = { x => clumps.find(_.contains((o, x))).get.toString }
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

trait TerminalFinitelyGeneratedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C =>
  override def objects = List(terminalObject)
  override def generators(source: O, target: O) = List(morphismFrom(terminalObject))
}
trait TerminalFinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends TerminalFinitelyGeneratedCategory[O, M, C] { self: C =>
  override def relations(source: O, target: O) = Nil
}

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategories[O, M, C] { FPCAT =>
  class PullbackTwoFunctor extends CategoricalTwoFunctor[O, M, C, HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] {
    def source = FPCAT
    def target = ???

    def onZeroMorphisms(m0: C) = new FunctorsToSet[O, M, C](m0)
    def onOneMorphisms(m1: Functor[O, M, C]): Functor[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackFunctor[O, M, C, Set, Function, Sets](m1, Sets)
    def onTwoMorphisms(m2: NaturalTransformation[O, M, C]): NaturalTransformation[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackNaturalTransformation[O, M, C, Set, Function, Sets](m2, Sets)
  }

}

