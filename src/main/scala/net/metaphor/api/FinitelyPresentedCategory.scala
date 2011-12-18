package net.metaphor.api

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M] = for (s <- objects; t <- objects; g <- generators(s, t)) yield g
}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Categories[O, M, C]

trait FinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategory[O, M, C] { self: C =>
  def relations(source: O, target: O): List[M]
  def relations: List[M] = for (s <- objects; t <- objects; r <- relations(s, t)) yield r

  trait WithTerminalObject extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C => }
  def adjoinTerminalObject(o: O): WithTerminalObject = ???

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    class TerminalExtensions(val terminal: WithTerminalObject) { terminalExtensions =>
      trait FunctorToSet extends terminal.FunctorToSet {
        def terminalSet: Set
        def mapToTerminalSet(o: O): Function

        override def onObjects(o: O): Set = ???
        override def onMorphisms(m: M): Function = ???
      }
      trait NaturalTransformationToSet extends terminal.NaturalTransformationToSet {
        def terminalMap: Function
        
        override def apply(o: O) = {
          if(o == terminal.terminalObject) {
            terminalMap
          } else {
            functorToSet(o).identity
          }
        }
      }

      //      def unchecked(functor: net.metaphor.api.FunctorToSet[O, M, C]): terminalExtensions.FunctorToSet = {
      //        new terminalExtensions.FunctorToSet {
      //          def onObjects(o: O) = functor.onObjects(o)
      //          def onMorphisms(m: M) = functor.onMorphisms(m)
      //        }
      //      }
    }
  }

  case class TerminalExtensionsOfFunctorToSet()

  class FunctorsToSet extends super.FunctorsToSet {
    //    def colimit(functor: self.FunctorToSet, o: O): InitialObject[net.metaphor.api.FunctorToSet[O, M, C], net.metaphor.api.NaturalTransformationToSet[O, M, C]] = {
    //      val extensions = new functor.TerminalExtensions(adjoinTerminalObject(o))
    //      val honestAnswer = colimit(functor)(extensions = extensions)
    //      // now we do some 'widening', just pretending that every net.metaphor.api.FunctorToSet is actually an instance of extensions.FunctorToSet
    //      new InitialObject[net.metaphor.api.FunctorToSet[O, M, C], net.metaphor.api.NaturalTransformationToSet[O, M, C]] {
    //        def initialObject = honestAnswer.initialObject
    //        def morphismTo(o: net.metaphor.api.FunctorToSet[O, M, C]) = honestAnswer.morphismTo(extensions.unchecked(o))
    //      }
    //    }

    def colimit(functor: self.FunctorToSet)(extensions: functor.TerminalExtensions): InitialObject[extensions.FunctorToSet, extensions.NaturalTransformationToSet] = {
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
        m <- self.generators;
        s = self.source(m);
        t = self.target(m);
        x <- functor(s).toIterable
      ) yield List((s, x), (t, functor(m).toFunction(x)))

      lazy val clumps = arrows.foldLeft(initialClumps)(combineClumps _)

      val resultSet = new Set {
        override def toIterable = clumps
      }
      val resultFunctions: (O => Function) = { o: O =>
        new Function {
          override def toFunction = { x => clumps.find(_.contains((o, x))).get }
        }
      }

      new InitialObject[extensions.FunctorToSet, extensions.NaturalTransformationToSet] {
        def initialObject = new extensions.FunctorToSet {
          override def terminalSet = resultSet
          override def mapToTerminalSet(o: O) = resultFunctions(o) 
        }
        def morphismTo(other: extensions.FunctorToSet) = {
          new extensions.NaturalTransformationToSet {
            override def source = initialObject
            override def target = other
            override def terminalMap = ???
          }
        }
      }
    }

  }
}

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategories[O, M, C]

