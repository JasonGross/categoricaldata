package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor { translation =>
  override val source: Ontology
  override val target: Ontology

  def assertFiniteTarget: Translation with FiniteTarget = ??? //???
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

  trait RightPushforward extends CovariantDataFunctor { pushforward =>
    override def onObjects(i: source.O) = new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = slice(o)
        F.pullback(i).limitSet
      }
      override def onGenerators(g: translation.target.G) = {
        val sg = slice(translation.target.opposite.generatorAsMorphism(translation.target.opposite.reverse(g)))
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
            // actually, o is an ObjectLeftOf
            targetLimitTerminalCone.mapFromInitialSet(???) andThen ??? //???
            ???
          }
        }

        // Now, the source limit provides us with the desired map.
        val sourceLimit = sourceData.limit
        val coneMap = sourceLimit.morphismFrom(cone)

        coneMap.initialMap
      }
    }
    override def onMorphisms(m: translation.source.NaturalTransformationToSet) = new translation.target.Datamap {
      override val source = pushforward.onObjects(m.source)
      override val target = pushforward.onObjects(m.target)
      override def apply(o: Box) = ??? //???
    }
  }
  trait LeftPushforward extends CovariantDataFunctor { shriek =>
    override def onObjects(i: source.O) = new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = coslice(o) // TODO weird, why on earth do we need this intermediate val?
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
            // TODO if this really right?
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
      
    }
    override def onMorphisms(m: translation.source.NaturalTransformationToSet) = new translation.target.Datamap {
      override val source = onObjects(m.source)
      override val target = onObjects(m.target)
      override def apply(o: Box) = ??? //???
    }
  }

  lazy val leftPushforward = new LeftPushforward {}
  lazy val rightPushforward = new RightPushforward {}

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
}
