package net.metaphor.api

trait Translation extends Functor.withFinitelyPresentedSource.withFinitelyPresentedTarget { translation =>
  override val source: Ontology
  override val target: Ontology

  lazy val toJSON = net.metaphor.json.Pack.packTranslation(this)

  override def equals(other: Any): Boolean = {
    other match {
      case other: Translation => {
        if (source != other.source) return false
        if (target != other.target) return false
        for (o <- source.objects) {
          if (this(o) != other(o)) return false
        }
        for (g <- source.allGenerators) {
          if (this.onGenerators(g) != other.onGenerators(g)) return false
        }
        true
      }
      case _ => false
    }
  }

  override def toString = {
    "Translation(\n" +
      "  source = " + source.toString + ",\n" +
      "  target = " + target.toString + ",\n" +
      "  onObjects = " + source.objects.map(o => o -> this(o)).toMap.toString + ",\n" +
      "  onMorphisms = " + source.allGenerators.map(g => g -> this.onGenerators(g)).toMap.toString + ")"
  }

  private class FiniteSliceCategory(onRight: translation.target.O) extends SliceCategory(
    translation.target.asInstanceOf[Ontologies.Finite].maximumWordLength,
    onRight) with CachingGenerators

  private class FiniteCosliceCategory(onLeft: translation.target.O) extends CosliceCategory(
    translation.target.asInstanceOf[Ontologies.Finite].maximumWordLength,
    onLeft) with CachingGenerators

  class SliceFunctor extends super.SliceFunctor {
    import net.tqft.toolkit.functions.Memo
    override val buildSliceCategory: Box => SliceCategory = Memo({ onRight: Box => new FiniteSliceCategory(onRight) })
  }
  class CosliceFunctor extends super.CosliceFunctor {
    import net.tqft.toolkit.functions.Memo
    override val buildCosliceCategory: Box => CosliceCategory = Memo({ onLeft: Box => new FiniteCosliceCategory(onLeft) })
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
          override def functionFromInitialSet(o: Fs.source.O) = {
            val f = targetLimitTerminalCone.functionFromInitialSet(Fg(o.asInstanceOf[Fg.source.O]).asInstanceOf[Ft.source.O])
            new coneFunction(o) {
              override def toFunction = f.toFunction
            }
          }
        }

        // Now, the source limit provides us with the desired map.
        val sourceLimit = sourceData.limit
        val coneMap = sourceLimit.morphismToTerminalObject(cone)

        coneMap.initialFunction
      }
    }).memo
    override def onMorphisms(m: source.M): target.M = new translation.target.Datamap {
      override val source = pushforward.onObjects(m.source)
      override val target = pushforward.onObjects(m.target)
      override def apply(o: Box) = ??? // MATH what is the Rightpushforward of a Datamap?
   /* 
    * Given a functor F: C-->D. 
    * Given datasets I,J: C-->Set
    * Given a natural transformation m:I-->J
    * Want: n:= F_*(m): F_*(I) --> F_*(J).
    * Want: for each object d in D, a function n(d): F_*(I)(d)-->F_*(J)(d)
    * Let pi: (d | F)-->C.
    * Want: a function n(d): lim_{d | F} pi^*(I) --> lim_{d|F} pi^*(J) 
    * Provide: lim_{d | F} pi^*(m).  
    */
      
      
      
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
          override def functionToTerminalSet(o: Fs.source.O) = {
            val f = targetColimitInitialCoCone.functionToTerminalSet(Fg(o.asInstanceOf[Fg.source.O]).asInstanceOf[Ft.source.O])
            new coConeFunction(o) {
              override def toFunction = f.toFunction
            }
          }
        }

        // Now, the source limit provides us with the desired map.
        val sourceColimit = sourceData.colimit
        val coconeMap = sourceColimit.morphismFromInitialObject(cocone)

        coconeMap.terminalFunction
      }

    }).memo
    override def onMorphisms(m: source.M): target.M = new translation.target.Datamap {
      override val source = onObjects(m.source)
      override val target = onObjects(m.target)
      override def apply(o: Box) = ??? // MATH what is the Leftpushforward of a Datamap?
   /* 
    * Given a functor F: C-->D. 
    * Given datasets I,J: C-->Set
    * Given a natural transformation m:I-->J
    * Want: n:= F_!(m): F_!(I) --> F_!(J).
    * Want: for each object d in D, a function n(d): F_!(I)(d)-->F_!(J)(d)
    * Let pi: (F | d)-->C.
    * Want: a function n(d): colim_{F | d} pi^*(I) --> colim_{d|F} pi^*(J) 
    * Provide: colim_{F | d} pi^*(m).  
    */
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
      val source = leftPushforward andThen pullback
      val target = pullback.target.identityFunctor
      def apply(o: translation.source.F /* e.g. Dataset */ ): translation.source.T /* e.g. Datamap */ = {
        translation.source.internalize(new NaturalTransformationToSet {
          override val source = leftCounit.source(o)
          override val target = leftCounit.target(o)
          override def apply(o: translation.source.O): FFunction = ??? // MATH what is the left counit for pullback?
   /* 
    * Given a functor F: C-->D. 
    * Given a dataset L: D-->Set
    * Want: epsilon:= F_!F^*(L) --> L.
    * Want: for each object d in D, a function epsilon(d): F_!F^*(L)(d) --> L(d)
    * Let pi: (F | d)-->C.
    * Want: a function n(d): colim_{F | d} pi^*F^*(L) --> L(d) 
    * Given b:F(c)-->d in (F | d)
    * Want: function epsilon(d): pi^*F^*L(b)-->L(d).
    * Lemma: we have function L(a): L(F(c)) --> L(d).
    * Want: function ep: pi^*F^*L(b)-->L(F(c)); composing with L(a) will give the desired epsilon(d).
    * Compute: pi^*F^*L(b) = L(F(c)), where = means "canonically isomorphic to". 
    * Provide ep:= canonical isomorphism. QED
    */
        })
      }
    }
    lazy val leftUnit = new NaturalTransformation { leftUnit =>
      val source = pullback.source.identityFunctor
      val target = pullback andThen leftPushforward
      def apply(o: sourceCategory.O /* this is just translation.target.FunctorToSet, but the compiler is recalcitrant */ ): translation.target.T = {
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
      def apply(o: sourceCategory.O /* this is just translation.target.FunctorToSet, but the compiler is recalcitrant */ ): translation.target.T = {
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
      def apply(o: sourceCategory.O /* this is just translation.source.FunctorToSet, but the compiler is recalcitrant */ ): translation.source.T = {
        translation.source.internalize(new NaturalTransformationToSet {
          override val source = rightUnit.source(o)
          override val target = rightUnit.target(o)
          override def apply(o: translation.source.O): FFunction = ??? // MATH what is the right unit for pullback?
        })
      }

    }
  }

  override lazy val pullback = new Pullback {}

  def opposite = new Translation {
    override val source = translation.source.opposite
    override val target = translation.target.opposite
    override def onObjects(o: source.O) = translation.onObjects(o)
    override def onGenerators(g: source.G) = ???
  }
}
