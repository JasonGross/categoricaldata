package net.categoricaldata.category

trait Functor {
  val source: Category
  val target: Category

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

trait ParametrizedFunctor[SC <: Category, TC <: Category] extends Functor {
  override val source: SC
  override val target: TC
}

object Functor {
    def compose(f: Functor, g: Functor): ParametrizedFunctor[f.source.type, g.target.type] = new ParametrizedFunctor[f.source.type, g.target.type] {
      require(f.target == g.source)
      override val source: f.source.type = f.source
      override val target: g.target.type = g.target
      override def onObjects(o: f.source.O): g.target.O = g(f(o).asInstanceOf[g.source.O])
      override def onMorphisms(m: f.source.M): g.target.M = g(f(m).asInstanceOf[g.source.M])
    }  
  
  trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource {
    override val source: FinitelyGeneratedCategory
  }
  trait withFinitelyGeneratedTarget extends withLocallyFinitelyGeneratedTarget {
    override val target: FinitelyGeneratedCategory
  }
  trait withFinitelyPresentedSource extends withFinitelyGeneratedSource { functor =>
    override val source: FinitelyPresentedCategory
    def verifyRelations = {
      for (relation <- source.allRelations) {
        require(functor.onMorphisms(source.pathAsMorphism(relation._1)) == functor.onMorphisms(source.pathAsMorphism(relation._2)))
      }
    }
  }
  trait withFinitelyPresentedTarget extends withFinitelyGeneratedTarget {
    override val target: FinitelyPresentedCategory
  }
  trait withLocallyFinitelyGeneratedSource extends withSmallSource { functor =>
    override val source: LocallyFinitelyGeneratedCategory
    def onGenerators(g: source.G): target.M
    override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }
  }
  trait withLocallyFinitelyGeneratedTarget extends withSmallTarget {
    override val target: LocallyFinitelyGeneratedCategory
  }
  trait withSmallSource extends Functor {
    override val source: SmallCategory
  }
  trait withSmallTarget extends Functor {
    override val target: SmallCategory
  }

}

trait MemoFunctor extends Functor {
  import net.tqft.toolkit.functions.Memo
  val memoOnObjects = Memo(super.onObjects _)
  val memoOnMorphisms = Memo(super.onMorphisms _)
  abstract override def onObjects(o: source.O) = memoOnObjects(o)
  abstract override def onMorphisms(o: source.M) = memoOnMorphisms(o)
}
