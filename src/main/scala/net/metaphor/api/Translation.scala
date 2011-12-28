package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor { translation =>
  override val source: Ontology
  override val target: Ontology

  override type SC = SliceCategory
  override type cSC = CosliceCategory

  def assertFiniteTarget: Translation with FiniteTarget = ???
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

  object Pushforward extends CovariantDataFunctor { pushforward =>
    override def onObjects(i: source.O) = new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = slice(o).functor
        F.Pullback(i).limitSet
      }
      override def onGenerators(g: translation.target.G) = ???
    }
    override def onMorphisms(m: translation.source.NaturalTransformationToSet) = new translation.target.Datamap {
      override val source = pushforward.onObjects(m.source)
      override val target = pushforward.onObjects(m.target)
      override def apply(o: Box) = ???
    }
  }
  object Shriek extends CovariantDataFunctor { shriek =>
    override def onObjects(i: source.O) = new translation.target.Dataset {
      override def onObjects(o: Box) = {
        val F = coslice(o).functor // FIXME weird, why on earth do we need this intermediate val?
        F.Pullback(i).colimitSet
      }
      override def onGenerators(m: translation.target.G) = ???
    }
    override def onMorphisms(m: translation.source.NaturalTransformationToSet) = new translation.target.Datamap {
      override val source = onObjects(m.source)
      override val target = onObjects(m.target)
      override def apply(o: Box) = ???
    }
  }

  def __! = new Functor {
    override val source = FunctorsToSet 
    override val target = translation.target.functorsToSet
    def onObjects(i: FunctorToSet) = Shriek.apply(translation.source.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = Shriek.apply(translation.source.internalize(t))
  } 
  def __* = new Functor {
    override val source = FunctorsToSet
    override val target = translation.target.functorsToSet 
    def onObjects(i: FunctorToSet) = Pushforward.apply(translation.source.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = Pushforward.apply(translation.source.internalize(t))
  } 
}
