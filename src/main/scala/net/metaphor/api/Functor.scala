package net.metaphor.api

trait HeteroFunctor[C1 <: Category[C1], C2 <: Category[C2]] {
  val source: C1
  val target: C2

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

trait SmallHeteroFunctor[C1 <: SmallCategory[C1], C2 <: SmallCategory[C2]] extends HeteroFunctor[C1, C2] { functor =>
  trait ContravariantDataFunctor extends HeteroFunctor[target.CSets, source.CSets] {
    val source = functor.target.functorsToSet
    val target = functor.source.functorsToSet

    def apply(i: C2#F) = super.apply(functor.target.liftFunctorToSet(i))
    def apply(m: C2#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.target.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[C2, functor.target.F]]))
    }
  }
  trait CovariantDataFunctor extends HeteroFunctor[source.CSets, target.CSets] {
    val source = functor.source.functorsToSet
    val target = functor.target.functorsToSet

    def apply(i: C1#F) = super.apply(functor.source.liftFunctorToSet(i))
    def apply(m: C1#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.source.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[C1, functor.source.F]]))
    }
  }

  trait Pullback extends ContravariantDataFunctor {
    def onObjects(i: functor.target.F) = functor.source.liftFunctorToSet(new functor.source.FunctorToSet {
      def onObjects(o: functor.source.O) = i(functor.apply(o))
      def onMorphisms(m: functor.source.M) = i(functor.apply(m))
    })
    def onMorphisms(m: functor.target.T) = functor.source.liftNaturalTransformationToSet(new functor.source.NaturalTransformationToSet[functor.source.F] {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: functor.source.O) = m(functor.apply(o).asInstanceOf[m.sourceCategory.O])
    })
  }

  def pullback: Pullback = new Pullback {}

  def ^* = pullback

}

trait Functor[C <: Category[C]] extends HeteroFunctor[C, C] { functor => }

trait SmallFunctor[C <: SmallCategory[C]] extends Functor[C] with SmallHeteroFunctor[C, C] { smallFunctor =>

  type SC <: SliceCategory
  type cSC <: CosliceCategory

  abstract class SliceCategory(onRight: smallFunctor.target.O) extends LocallyFinitelyGeneratedCategory[SC] { sliceCategory: SC =>
    override type O = ObjectLeftOf
    override type M = ObjectLeftOfMap

    case class ObjectLeftOf(left: smallFunctor.source.O, path: smallFunctor.target.M) {
      require(smallFunctor.target.source(path) == smallFunctor.apply(left))
      require(smallFunctor.target.target(path) == onRight)
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, path: smallFunctor.source.M) {
      require(smallFunctor.source.source(path) == source.left)
      require(smallFunctor.source.target(path) == target.left)
      require(smallFunctor.target.compose(smallFunctor.apply(path), target.path) == source.path)
    }

    def identity(o: ObjectLeftOf) = ObjectLeftOfMap(o, o, smallFunctor.source.identity(o.left))
    def source(m: ObjectLeftOfMap) = m.source
    def target(m: ObjectLeftOfMap) = m.target
    def compose(m1: ObjectLeftOfMap, m2: ObjectLeftOfMap) = ObjectLeftOfMap(m1.source, m2.target, smallFunctor.source.compose(m1.path, m2.path))

    def opposite = ??? // new SliceCategory(onRight) with Opposite

    val functorsToSet = ???
    val adjoinInitialObject = ???
    val adjoinTerminalObject = ???
    //      type CSets = sliceCategory.FunctorsToSet

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[SC]): F = ???
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[SC, F]): T = ???
  }

  abstract class CommaFunctor extends HeteroFunctor[C, target.CategoriesOver[SC]] {
    override val target = smallFunctor.target.categoriesOver[SC]
  }

  abstract class SliceFunctor extends CommaFunctor { sliceFunctor =>
    override val source: smallFunctor.target.type = smallFunctor.target
    override def onObjects(s: source.O): target.O = ??? // FIXME // new SliceCategoryOver(s)
    override def onMorphisms(m: smallFunctor.target.M): target.M = ??? //new SliceFunctorOver(m)

    class SliceCategoryOver(onRight: smallFunctor.target.O) extends smallFunctor.target.CategoriesOver[SC] {
      val functor: smallFunctor.target.FunctorTo[SC] = new smallFunctor.target.FunctorTo[SC] {
        override val source = category
        override def onObjects(o: source.O) = ???
        override def onMorphisms(m: source.M) = ???
      }
      val category: SC = buildSliceCategory(onRight)
    }
    class SliceFunctorOver(m: smallFunctor.target.M) { sliceFunctorOver: smallFunctor.target.FunctorOver[SC] =>
      def source = onObjects(smallFunctor.target.source(m))
      def target = onObjects(smallFunctor.target.target(m))
      def functor = ???
    }

    def buildSliceCategory(onRight: smallFunctor.target.O): SC

  }

  abstract class CosliceCategory(onLeft: smallFunctor.target.O) extends LocallyFinitelyGeneratedCategory[cSC] { cosliceCategory: cSC =>
    override type O = ObjectRightOf
    override type M = ObjectRightOfMap

    case class ObjectRightOf(right: smallFunctor.source.O, path: smallFunctor.target.M) {
      require(smallFunctor.target.source(path) == onLeft)
      require(smallFunctor.target.target(path) == smallFunctor.apply(right))
    }
    case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, path: smallFunctor.source.M) {
      require(smallFunctor.source.source(path) == source.right)
      require(smallFunctor.source.target(path) == target.right)
      require(smallFunctor.target.compose(source.path, smallFunctor.apply(path)) == target.path)
    }

    def identity(o: ObjectRightOf) = ObjectRightOfMap(o, o, smallFunctor.source.identity(o.right))
    def source(m: ObjectRightOfMap) = m.source
    def target(m: ObjectRightOfMap) = m.target
    def compose(m1: ObjectRightOfMap, m2: ObjectRightOfMap) = ObjectRightOfMap(m1.source, m2.target, smallFunctor.source.compose(m1.path, m2.path))

    def opposite = ??? // new CosliceCategory(onLeft) with Opposite

    val functorsToSet = ???
    val adjoinInitialObject = ???
    val adjoinTerminalObject = ???
    //        type CSets = FunctorsToSet

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[cSC]): F = ???
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[cSC, F]): T = ???
  }

  abstract class CosliceFunctor extends CommaFunctor {
    type cSC <: CosliceCategory

  }
}

trait FinitelyGeneratedFunctor[C <: FinitelyGeneratedCategory[C]] extends SmallFunctor[C] { functor =>
  override type SC <: SliceCategory
  override type cSC <: CosliceCategory

  class SliceCategory(onRight: functor.target.O) extends super.SliceCategory(onRight) with FinitelyGeneratedCategory[SC] { sliceCategory: SC =>
    override type O = super.O
    override type M = super.M

    override type CSets = FunctorsToSet

    def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (0 to k).toList;
        left <- functor.source.objectsAtLevel(l);
        path <- functor.target.wordsOfLength(k - l)(functor.apply(left), onRight)
      ) yield ObjectLeftOf(left, path)
    }
    val minimumLevel: Int = 0
    val maximumLevel: Int = ??? // functor.source.maximumLevel + functor.target.maximumWordLength
    def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      for (g <- functor.source.generators(source.left, target.left); if functor.target.compose(functor.apply(g), target.path) == source.path) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }
  }

  class CosliceCategory(onLeft: functor.target.O) extends super.CosliceCategory(onLeft) with FinitelyGeneratedCategory[cSC] { cosliceCategory: cSC =>
    override type O = super.O
    override type M = super.M

    override type CSets = FunctorsToSet

    def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (functor.source.minimumLevel to k).toList;
        right <- functor.source.objectsAtLevel(l);
        path <- functor.target.wordsOfLength(k - l)(onLeft, functor.apply(right))
      ) yield ObjectRightOf(right, path)
    }
    val minimumLevel: Int = 0
    val maximumLevel: Int = ??? // functor.source.maximumLevel + functor.target.maximumWordLength

    def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      for (g <- functor.source.generators(source.right, target.right); if functor.target.compose(source.path, functor.apply(g)) == target.path) yield {
        ObjectRightOfMap(source, target, g)
      }
    }
  }
}

object Functor {
  class IdentityFunctor[C <: Category[C]](val category: C) extends Functor[C] {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor[C <: Category[C]](val functor1: Functor[C], val functor2: Functor[C]) extends Functor[C] {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])

  }
}
