package net.metaphor.api

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

    lazy val adjoinInitialObject = ???
    lazy val adjoinTerminalObject = ???

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

    lazy val adjoinInitialObject = ???
    lazy val adjoinTerminalObject = ???
  }

  abstract class SliceFunctor extends HeteroFunctor[C, target.CategoriesOver[SC]] { sliceFunctor =>
    override val source: smallFunctor.target.type = smallFunctor.target
    override val target = smallFunctor.target.categoriesOver[SC]
    override def onObjects(s: source.O): target.O = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): target.M = new SliceFunctorOver(m)

    class SliceCategoryOver(onRight: smallFunctor.target.O) extends smallFunctor.target.CategoryOver[SC] {
      override val functor: smallFunctor.target.FunctorTo[SC] = new smallFunctor.target.FunctorTo[SC] {
        override val source = buildSliceCategory(onRight)
        override def onObjects(o: source.O) = ???
        override def onMorphisms(m: source.M) = ???
      }
    }
    class SliceFunctorOver(m: smallFunctor.target.M) extends smallFunctor.target.FunctorOver[SC] {
      override def source = onObjects(smallFunctor.target.source(m))
      override def target = onObjects(smallFunctor.target.target(m))
      override def functor = ???
    }

    def buildSliceCategory(onRight: smallFunctor.target.O): SC

  }

  abstract class CosliceFunctor extends HeteroFunctor[C, target.CategoriesOver[cSC]] { cosliceFunctor =>
    override val source: smallFunctor.target.type = smallFunctor.target
    override val target = smallFunctor.target.categoriesOver[cSC]
    override def onObjects(s: source.O): target.O = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): target.M = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onLeft: smallFunctor.target.O) extends smallFunctor.target.CategoryOver[cSC] {
      override val functor: smallFunctor.target.FunctorTo[cSC] = new smallFunctor.target.FunctorTo[cSC] {
        override val source = buildCosliceCategory(onLeft)
        override def onObjects(o: source.O) = ???
        override def onMorphisms(m: source.M) = ???
      }
    }
    class CosliceFunctorOver(m: smallFunctor.target.M) extends smallFunctor.target.FunctorOver[cSC] {
      override def source = onObjects(smallFunctor.target.source(m))
      override def target = onObjects(smallFunctor.target.target(m))
      override def functor = ???
    }

    def buildCosliceCategory(onLeft: smallFunctor.target.O): cSC
  }
}