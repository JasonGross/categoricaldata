package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory[C <: FinitelyPresentedCategory[C]] extends FinitelyGeneratedCategory[C] { self: C =>
  def relations(source: O, target: O): List[M]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[M] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  // FIXME implement toString, hashcode, equals
  override def toString: String = ???
  override def hashCode: Int = ???
  override def equals(other: Any): Boolean = ???

//  trait Opposite extends super.Opposite { opposite: C =>
//    override def relations(source: O, target: O) = self.relations(target, source)
//  }

  trait WithTerminalObject[D <: FinitelyPresentedCategory[D]] extends super.WithTerminalObject[D] { terminal: D =>
    def relations(source: O, target: O) = {
      if (target == terminalObject) {
        ???
      } else {
        self.relations(source, target)
      }
    }
  }
  trait WithInitialObject[D <: FinitelyPresentedCategory[D]] extends super.WithInitialObject[D] { initial: D =>
    def relations(source: O, target: O) = {
      if (source == initialObject) {
        ???
      } else {
        self.relations(source, target)
      }
    }
  }

}

trait FinitelyPresentedCategories[C <: FinitelyPresentedCategory[C]] extends FinitelyGeneratedCategories[C] { FPCAT =>
}

