package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor { translation =>
  override val source: Ontology
  override val target: Ontology
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite

  class SliceFunctor extends super.SliceFunctor {
    override def buildSliceCategory(onRight: Box) = new SliceCategory(onRight)
  }
  class CosliceFunctor extends super.CosliceFunctor {
    override def buildCosliceCategory(onLeft: Box) = new CosliceCategory(onLeft)
  }

  lazy val slice: SliceFunctor = new SliceFunctor
  lazy val coslice: CosliceFunctor = new CosliceFunctor

  trait PullbackLeftAdjoint extends LeftAdjoint { self: Functor =>
    lazy val rightAdjoint = pullback
    lazy val rightUnit = rightAdjoint.leftUnit
    lazy val rightCounit = rightAdjoint.leftCounit
  }
  trait PullbackRightAdjoint extends RightAdjoint { self: Functor =>
    lazy val leftAdjoint = pullback
    lazy val leftUnit = leftAdjoint.rightUnit
    lazy val leftCounit = leftAdjoint.rightCounit
  }

  trait RightPushforward extends CovariantDataFunctor with PullbackRightAdjoint { pushforward =>
    override def onObjects(i: source.O): target.O = (new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = slice(o)
        F.pullback(i).limitSet
      }
      override def onGenerators(g: translation.target.G) = {
        val sg = slice(translation.target.opposite.generatorAsMorphism(translation.target.opposite.reverseGenerator(g)))
        val Fg = sg.functor
        val Ft = sg.target
        val Fs = sg.source

        // First, construct the limit (that is, the terminal cone) on the slice category over the target of g.
        // Ft is the functor from the comma category of objects left of the target of g, back to the source of the the functor.
        val targetLimitTerminalCone = Ft.pullback(i).limit.terminalObject

        // Second, construct the dataset over the slice category over the target of g.
        val sourceData = Fs.pullback(i)

        // Third, we need to build a cone for sourceData 
        val cone: sourceData.Cone = new sourceData.Cone {
          override val initialSet = targetLimitTerminalCone.initialSet
          override def mapFromInitialSet(o: Fs.source.O) = {
            val f = targetLimitTerminalCone.mapFromInitialSet(Fg(o.asInstanceOf[Fg.source.O]).asInstanceOf[Ft.source.O])
            new coneFunction(o) {
              override def toFunction = f.toFunction
            }
          }
        }

        // Now, the source limit provides us with the desired map.
        val sourceLimit = sourceData.limit
        val coneMap = sourceLimit.morphismFrom(cone)

        coneMap.initialMap
      }
    }).memo
    override def onMorphisms(m: source.M): target.M = new translation.target.Datamap {
      override val source = pushforward.onObjects(m.source)
      override val target = pushforward.onObjects(m.target)
      override def apply(o: Box) = ???  // MATH what is the pushforward of a Datamap?
    }
  }

  trait LeftPushforward extends CovariantDataFunctor with PullbackLeftAdjoint { shriek =>
    override def onObjects(i: source.O): target.O = (new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = coslice(o) // Weird, why on earth do we need this intermediate val?
        F.pullback(i).colimitSet
      }
      override def onGenerators(g: translation.target.G) = {
        val sg = coslice(translation.target.generatorAsMorphism(g))
        val Fg = sg.functor
        val Ft = sg.target
        val Fs = sg.source

        // First, construct the colimit (that is, the initial cocone) on the coslice category over the target of g.
        // Ft is the functor from the comma category of objects left of the target of g, back to the source of the the functor.
        val targetColimitInitialCoCone = Ft.pullback(i).colimit.initialObject

        // Second, construct the dataset over the slice category over the target of g.
        val sourceData = Fs.pullback(i)

        // Third, we need to build a cone for sourceData 
        val cocone: sourceData.CoCone = new sourceData.CoCone {
          override val terminalSet = targetColimitInitialCoCone.terminalSet
          override def mapToTerminalSet(o: Fs.source.O) = {
            val f = targetColimitInitialCoCone.mapToTerminalSet(Fg(o.asInstanceOf[Fg.source.O]).asInstanceOf[Ft.source.O])
            new coConeFunction(o) {
              override def toFunction = f.toFunction
            }
          }
        }

        // Now, the source limit provides us with the desired map.
        val sourceColimit = sourceData.colimit
        val coconeMap = sourceColimit.morphismTo(cocone)

        coconeMap.terminalMap
      }

    }).memo
    override def onMorphisms(m: source.M): target.M = new translation.target.Datamap {
      override val source = onObjects(m.source)
      override val target = onObjects(m.target)
      override def apply(o: Box) = ??? // MATH what is the pushforward of a Datamap?
    }
  }

  trait MemoLeftPushforward extends LeftPushforward with MemoFunctor
  trait MemoRightPushforward extends RightPushforward with MemoFunctor

  lazy val leftPushforward: LeftPushforward = new MemoLeftPushforward {}
  lazy val rightPushforward: RightPushforward = new MemoRightPushforward {}

  def __! = new Functor {
    override val source = FunctorsToSet
    override val target = translation.target.functorsToSet
    def onObjects(i: FunctorToSet) = leftPushforward.apply(translation.source.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = leftPushforward.apply(translation.source.internalize(t))
  }
  def __* = new Functor {
    override val source = FunctorsToSet
    override val target = translation.target.functorsToSet
    def onObjects(i: FunctorToSet) = rightPushforward.apply(translation.source.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = rightPushforward.apply(translation.source.internalize(t))
  }

  trait Pullback extends super.Pullback with LeftAdjoint with RightAdjoint {
    /*
     *	F: C --> D
     *
     * 	F^*: D-Sets --> C-Sets, has
     * 		left adjoint F_!: C-Sets --> D-Sets
     * 		right adjoint F_*: C-Sets --> D-Sets
     * 
     * We have a (left) counit F_! F^* --> id_{C-Sets}
     *     and a (left) unit   id_{D-Sets} --> F^* F_!
     *     
     * and also a (right) counit F^* F_* --> id_{D-Sets}
     *      and a (right) unit   id_{C-Sets} --> F_* F^*
     */
    lazy val leftAdjoint = leftPushforward
    lazy val rightAdjoint = rightPushforward

    lazy val leftCounit = new NaturalTransformation { leftCounit =>
      val source =  leftPushforward andThen pullback 
      val target = pullback.target.identityFunctor
      def apply(o: translation.source.F /* e.g. Dataset */ ): translation.source.T /* e.g. Datamap */ = {
        translation.source.internalize(new NaturalTransformationToSet {
          override val source = leftCounit.source(o)
          override val target = leftCounit.target(o)
          override def apply(o: translation.source.O): FFunction = ??? // MATH what is the left counit for pullback?
        })
      }
    }
    lazy val leftUnit = new NaturalTransformation { leftUnit =>
      val source = pullback.source.identityFunctor
      val target = pullback andThen leftPushforward
      def apply(o: sourceCategory.O /* this is just translation.target.FunctorToSet, but the compiler is recalcitrant */): translation.target.T = {
        translation.target.internalize(new NaturalTransformationToSet {
          override val source = leftUnit.source(o)
          override val target = leftUnit.target(o)
          override def apply(o: translation.target.O): FFunction = ??? // MATH what is the left unit for pullback?
        })
      }
    }
    lazy val rightCounit = new NaturalTransformation { rightCounit =>
      val source = pullback andThen rightPushforward
      val target = pullback.source.identityFunctor
      def apply(o: sourceCategory.O /* this is just translation.target.FunctorToSet, but the compiler is recalcitrant */): translation.target.T = {
        translation.target.internalize(new NaturalTransformationToSet {
          override val source = rightCounit.source(o)
          override val target = rightCounit.target(translation.target.internalize(o))
          override def apply(o: translation.target.O): FFunction = ??? // MATH what is the right counit for pullback?
        })
      }
    }
    lazy val rightUnit = new NaturalTransformation { rightUnit =>
      val source = translation.source.AllFunctorsToSet.identityFunctor
      val target = rightPushforward andThen pullback
      def apply(o: sourceCategory.O /* this is just translation.source.FunctorToSet, but the compiler is recalcitrant */): translation.source.T = {
        translation.source.internalize(new NaturalTransformationToSet {
          override val source = rightUnit.source(o)
          override val target = rightUnit.target(o)
          override def apply(o: translation.source.O): FFunction = ??? // MATH what is the right unit for pullback?
        })
      }

    }
  }

  override lazy val pullback = new Pullback {}
}
