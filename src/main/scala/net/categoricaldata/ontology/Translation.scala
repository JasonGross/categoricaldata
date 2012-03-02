package net.categoricaldata.ontology
import net.categoricaldata.category._
import net.categoricaldata.sets._
import net.categoricaldata.dsl.Sentences

trait Translation extends functor.withFinitelyPresentedSource.withFinitelyPresentedTarget { translation =>
  override val source: Ontology
  override val target: Ontology

  def toJSON = net.categoricaldata.server.json.Pack.packTranslation(this)

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
    translation.target.asInstanceOf[Ontology.Finite].maximumWordLength,
    onRight) with LocallyFinitelyGeneratedCategory.CachingGenerators

  private class FiniteCosliceCategory(onLeft: translation.target.O) extends CosliceCategory(
    translation.target.asInstanceOf[Ontology.Finite].maximumWordLength,
    onLeft) with LocallyFinitelyGeneratedCategory.CachingGenerators

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

  trait CovariantDataFunctor extends super.CovariantDataFunctor {
    override val target: translation.target.FunctorsToSet = translation.target.functorsToSet
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

  //Call the translation F:C-->D
  
  trait LeftPushforward extends CovariantDataFunctor with PullbackLeftAdjoint { shriek => //left pushforward of translation is a functor from C-sets to D-sets
    val CSet=source
    val DSet=target
    val F:translation.type = translation //we need to be extra careful here, not just writing val F=translation, because we need to tell the type system exactly what's going on.
    val C:F.source.type=F.source
    val D:F.target.type=F.target
    override def onObjects(i: CSet.O): DSet.O = (new D.Dataset { //i is a dataset on C, we're going to define a dataset on D.
      val D=source
      type DObject=Box
      type DArrow=Arrow
      override def onObjects(o: DObject) = {
        val cs = coslice(o) // Weird, why on earth do we need this intermediate val?
        cs.pullback(i).colimitSet //We have now defined F_!(i)(o)
      }
      override def onGenerators(g: DArrow) = {
        val o = g.source
        val p = g.target
        val sg = coslice(D.generatorAsMorphism(g)) // a commutative triangle (F|o) --> (F|p) --> C
        val Fg = sg.functor  // (F|o) --> (F|p)
        val Fs = sg.source   // (F|o) --> C
        val Ft = sg.target   // (F|p) --> C
        val cosliceo = Fs.source
        val coslicep = Ft.source
        
        // First, construct the colimit (that is, the initial cocone) on (F|p).
        val targetColimitInitialCoCone = Ft.pullback(i).colimit.initialObject // initial object in (F|p)*-Set over Ft.pullback(i) (which is an (F|p)-set).

        // Second, construct the dataset on (F|o).
        val sourceData = Fs.pullback(i)

        // Third, we need to build a cocone for sourceData, by pulling back the cocone on Ft.pullback(i) via Fg.
        val cocone: sourceData.CoCone = new sourceData.CoCone { //the pullback of targetColimitInitialCocone along Fg.
          override val terminalSet = targetColimitInitialCoCone.terminalSet
          override def functionToTerminalSet(Fa2o: Fs.source.O) = {  //Fa2o : Fa --> o, for some a in Ob(C). We cheat, never needing to do anything on morphisms in Fs.source
            val f = targetColimitInitialCoCone.functionToTerminalSet(Fg(Fa2o.asInstanceOf[Fg.source.O]).asInstanceOf[Ft.source.O])
            new coConeFunction(Fa2o) {
              override def toFunction = f.toFunction
            }
          }
        }

        // Now, the source colimit provides us with the desired map.
        val sourceColimit = sourceData.colimit
        val coconeMap = sourceColimit.morphismFromInitialObject(cocone)

        coconeMap.terminalFunction
      }

    }).memo  //memo on a dataset makes sure that we never have to do the same computation twice.
    override def onMorphisms(m: CSet.M): DSet.M = new D.Datamap {
      val i=m.source
      val j=m.target
      override val source = onObjects(i) 
      override val target = onObjects(j)
      override def apply(d: Box) = ??? // MATH what is the Leftpushforward of a Datamap?
      /* 
    * Given a functor F: C-->D. 
    * Given datasets i,j: C-->Set
    * Given a natural transformation m:i-->j
    * Want: n:= F_!(m): F_!(i) --> F_!(j).
    * Want: for each object d in D, a function n(d): F_!(i)(d)-->F_!(j)(d)
    * Let pi=coslice (d)                  // pi: (F | d)-->C.
    * 
    * Want: a function n(d): colim_{F | d} pi^*(i) --> colim_{F | d} pi^*(j) 
    * Provide: colim_{F | d} pi^*(m).  
    */
    }
  }

  trait MemoLeftPushforward extends LeftPushforward with Functor.MemoFunctor
  trait MemoRightPushforward extends RightPushforward with Functor.MemoFunctor

  lazy val leftPushforward: LeftPushforward = new MemoLeftPushforward {}
  lazy val rightPushforward: RightPushforward = new MemoRightPushforward {}

  def __! = new Functor {
    override val source = Datasets
    override val target = translation.target.functorsToSet
    def onObjects(i: Dataset) = target.internalize(leftPushforward.apply(leftPushforward.source.internalize(i)))
    def onMorphisms(t: Datamap) = target.internalize(leftPushforward.apply(leftPushforward.source.internalize(t)))
  }
  def __* = new Functor {
    override val source = Datasets
    override val target = translation.target.functorsToSet
    def onObjects(i: Dataset) = target.internalize(rightPushforward.apply(rightPushforward.source.internalize(i)))
    def onMorphisms(t: Datamap) = target.internalize(rightPushforward.apply(rightPushforward.source.internalize(t)))
  }

  trait ContravariantDataFunctor extends super.ContravariantDataFunctor {
    override val target: translation.source.FunctorsToSet = translation.source.functorsToSet
  }

  trait Pullback extends super.Pullback with ContravariantDataFunctor with LeftAdjoint with RightAdjoint {
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
      override val source = Functor.compose(pullback, leftPushforward)
      override val target = pullback.source.identityFunctor
      override def apply(L: sourceCategory.O /* e.g. translation.source.Dataset */ ): targetCategory.M /* e.g. Datamap */ = {
        targetCategory.internalize(new NaturalTransformationToSet {
          override val source = leftCounit.source(L)
          override val target = leftCounit.target(L)
          override def apply(d: translation.source.O): FFunction = {
            val sourceSet = ???
            val targetSet = ???
            val map = ???
            FFunction(sourceSet, targetSet, map) // MATH what is the left counit for pullback?
          }
          /* 
    * Given a functor F: C-->D. 
    * Given a dataset L: D-->Set
    * Want: epsilon: F_!F^*(L) --> L.
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
      override val source = leftPushforward.source.identityFunctor
      override val target = Functor.compose(leftPushforward, pullback)
      override def apply(L: sourceCategory.O /* this is just translation.target.Dataset, but the compiler is recalcitrant */ ): targetCategory.M = {
        targetCategory.internalize(new NaturalTransformationToSet {
          override val source = leftUnit.source(L)
          override val target = leftUnit.target(L)
          override def apply(d: translation.target.O): FFunction = ??? // MATH what is the left unit for pullback?
        })
      }
    }
    lazy val rightCounit = new NaturalTransformation { rightCounit =>
      override val source = Functor.compose(rightPushforward, pullback)
      override val target = rightPushforward.source.identityFunctor
      override def apply(L: sourceCategory.O /* this is just translation.target.Dataset, but the compiler is recalcitrant */ ): targetCategory.M = {
        targetCategory.internalize(new NaturalTransformationToSet {
          override val source = rightCounit.source(L)
          override val target = rightCounit.target(L)
          override def apply(d: translation.target.O): FFunction = ??? // MATH what is the right counit for pullback?
        })
      }
    }
    lazy val rightUnit = new NaturalTransformation { rightUnit =>
      override val source = pullback.source.identityFunctor
      override val target = Functor.compose(pullback, rightPushforward)
      override def apply(L: sourceCategory.O /* this is just translation.source.FunctorToSet, but the compiler is recalcitrant */ ): targetCategory.M = {
        targetCategory.internalize(new NaturalTransformationToSet {
          override val source = rightUnit.source(L)
          override val target = rightUnit.target(L)
          override def apply(d: translation.source.O): FFunction = ??? // MATH what is the right unit for pullback?
        })
      }

    }
  }

  override lazy val pullback = new Pullback {}

  lazy val ^* = new Functor {
    override val source = FunctorsToSet
    override val target = translation.source.functorsToSet
    def onObjects(i: FunctorToSet) = target.internalize(pullback.apply(pullback.source.internalize(i)))
    def onMorphisms(t: NaturalTransformationToSet) = target.internalize(pullback.apply(pullback.source.internalize(t)))
  }

  def opposite = new Translation {
    override val source = translation.source.opposite
    override val target = translation.target.opposite
    override def onObjects(o: source.O) = translation.onObjects(o)
    override def onGenerators(g: source.G) = ???
  }

  def asPartialDataset: target.PartialDataset = new target.PartialDataset {
    override val source = translation.source
    override def onObjects(o: Box) = translation.onObjects(o)
    override def onGenerators(g: Arrow) = translation.onGenerators(g)
  }

}

object Translation {
  class ConcreteTranslation(override val source: Ontology, override val target: Ontology, onObjects: String => String, onMorphisms: Sentences.StringArrow => Sentences.StringPath, _json: Option[String] = None) extends Translation {
    private val objectMap: Map[Box, Box] = (for (s <- source.objects) yield {
      val t = target.objects.find(_.name == onObjects(s.name)).get
      s -> t
    }).toMap
    private val morphismMap: Map[Arrow, target.M] = (for (
      a <- source.allGenerators
    ) yield {
      val morphisms = for (Sentences.StringArrow(ts, to, tp) <- onMorphisms(Sentences.StringArrow(a.source.name, a.target.name, a.name)).arrows) yield {
        target.generatorAsMorphism(target.allGenerators.find(a => a.source.name == ts && a.name == tp && a.target.name == to).get)
      }
      a -> target.compose(objectMap(source.generatorSource(a)), morphisms)
    }).toMap

    verifyRelations

    override def onObjects(o: Box) = objectMap(o)
    // And again, replacing source.G with the apparently equivalent Arrow causes an AbstractMethodError
    override def onGenerators(a: source.G) = morphismMap(a)

    override def toJSON = super.toJSON.copy(json = _json)

  }

  def apply(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: Sentences.StringArrow => Sentences.StringPath, json: Option[String] = None): Translation = {
    // construct a new translation object
    new ConcreteTranslation(source, target, onObjects, onMorphisms, json)
  }

}
