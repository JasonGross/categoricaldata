package net.categoricaldata.category.functor.withLocallyFinitelyGeneratedSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedSource { functor =>
  /**
   * This is a little confusing. SliceCategory is abstract, but always has to be a FinitelyGeneratedCategory.
   * Lower down the hierarchy, the remaining methods can be filled in, either as a truncated SliceCategory, or the honest thing if that's really finite generated.
   */
  abstract class SliceCategory(onLeft: functor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategory.StandardFunctorsToSet { sliceCategory =>
    override type O = ObjectRightOf
    override type G = ObjectRightOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectRightOf(right: functor.source.O, morphism: functor.target.M) {
      override def toString = ("(" + morphism.toString + " = F(" + right.toString + "))").replace('"', ''') // this replacement is a hack, so double quotes never show up in JSON
      // we need to provide our own equals method, to ignore the outer class
      override def equals(other: Any) = {
        other match {
          case other: SliceCategory#ObjectRightOf => {
            right == other.right && morphism == other.morphism
          }
          case _ => false
        }
      }
      override def hashCode = (right, morphism).hashCode
      require(functor.target.target(morphism) == functor.apply(right))
      require(functor.target.source(morphism) == onLeft)
    }
    case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, generator: functor.source.G) {
      // we need to provide our own equals method, to ignore the outer class
      override def equals(other: Any) = {
        other match {
          case other: SliceCategory#ObjectRightOfMap => {
            source == other.source && target == other.target && generator == other.generator
          }
          case _ => false
        }
      }
      override def hashCode = (source, target, generator).hashCode
      require(functor.source.generatorSource(generator) == source.right)
      require(functor.source.generatorTarget(generator) == target.right)
      require(functor.target.compose(source.morphism, functor.onGenerators(generator)) == target.morphism)
    }

    override val minimumLevel: Int = functor.source.minimumLevel

    override def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      import functor.source.generatorAsMorphism
      for (g <- functor.source.generators(source.right, target.right); if functor.target.compose(source.morphism, functor.apply(g)) == target.morphism) yield {
        ObjectRightOfMap(source, target, g)
      }
    }

    override def pathEquality(p1: Path, p2: Path) = {
      val x1 = Path(p1.source.right, p1.target.right, p1.morphisms.map(_.generator))
      val x2 = Path(p2.source.right, p2.target.right, p2.morphisms.map(_.generator))
      functor.source.pathEquality(x1, x2)
    }
  }

  abstract class CosliceCategory(onRight: functor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategory.StandardFunctorsToSet { cosliceCategory =>
    override type O = ObjectLeftOf
    override type G = ObjectLeftOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectLeftOf(left: functor.source.O, morphism: functor.target.M) {
      override def toString = ("(F(" + left.toString + ") = " + morphism.toString + ")").replace('"', ''') // this replacement is a hack, so double quotes never show up in JSON
      // we need to provide our own equals method, to ignore the outer class
      override def equals(other: Any) = {
        other match {
          case other: CosliceCategory#ObjectLeftOf => {
            left == other.left && morphism == other.morphism
          }
          case _ => false
        }
      }
      override def hashCode = (left, morphism).hashCode
      require(functor.target.source(morphism) == functor.apply(left))
      require(functor.target.target(morphism) == onRight)
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, generator: functor.source.G) {
      // we need to provide our own equals method, to ignore the outer class
      override def equals(other: Any) = {
        other match {
          case other: CosliceCategory#ObjectLeftOfMap => {
            source == other.source && target == other.target && generator == other.generator
          }
          case _ => false
        }
      }
      override def hashCode = (source, target, generator).hashCode
      require(functor.source.generatorSource(generator) == source.left)
      require(functor.source.generatorTarget(generator) == target.left)
      require(functor.target.compose(functor.onGenerators(generator), target.morphism) == source.morphism)
    }

    override val minimumLevel: Int = functor.source.minimumLevel

    override def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      import functor.source.generatorAsMorphism
      for (g <- functor.source.generators(source.left, target.left); if functor.target.compose(functor.apply(g), target.morphism) == source.morphism) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }

    override def pathEquality(p1: Path, p2: Path) = {
      val x1 = Path(p1.source.left, p1.target.left, p1.morphisms.map(_.generator))
      val x2 = Path(p2.source.left, p2.target.left, p2.morphisms.map(_.generator))
      functor.source.pathEquality(x1, x2)
    }
  }

  trait ContravariantDataFunctor extends super.ContravariantDataFunctor {
    override val target: functor.source.FunctorsToSet = functor.source.functorsToSet
  }
  trait Pullback extends super.Pullback with ContravariantDataFunctor
  
}
