package net.metaphor.api.typed
import net.metaphor.api.Category
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.collections.NonStrictIterable

trait Set[A] {
  def toIterable: Iterable[A]
  def identity: Function[A, A] = IdentityFunction(this)
  
  override def equals(other: Any) = {
    other match {
      case other: Set[_] => {
        toIterable == other.toIterable
      }
      case _ => false
    }
  }
  
  /**
   * The default implementation calls sizeIfFinite, which may be expensive.
   */
  
  def finite: Boolean = sizeIfFinite.nonEmpty
  def sizeIfFinite: Option[Int]
  def size: Int = sizeIfFinite.getOrElse(???)
}
trait Function[A, B] { function =>
  def source: Set[A]
  def target: Set[B]
  def toFunction[X <: A, Y >: B]: X => Y
  def andThen[C](other: Function[B, C]) = new Function[A, C] {
    def source = function.source
    def target = other.target
    def toFunction[X <: A, Y >: C] = function.toFunction andThen other.toFunction
  }
  
  override def equals(other: Any): Boolean = {
    other match {
      case other: Function[_ , _] => {
        if(source != other.source) return false
        if(target != other.target) return false
        for(x <- source.toIterable) { if(toFunction(x) != other.asInstanceOf[Function[A, B]].toFunction(x)) return false }
        return true
      }
    }
  }
}
case class IdentityFunction[A](set: Set[A]) extends Function[A, A] {
  override def source = set
  override def target = set
  override def toFunction[X <: A, Y >: A] = { a: X => a }
}

object Function {
  def apply[A, B](source: Set[A], target: Set[B], function: A => B) = {
    val source_ = source
    val target_ = target
    new Function[A, B] {
    override val source = source_
    override val target = target_
    override def toFunction[X <: A, Y >: B] = function
  }
}
}

trait TypedCategory[C <: TypedCategory[C]] extends Category[C] { typedCategory: C =>
  type OO[A]
  type MM[A, B]
  type O = OO[_]
  type M = MM[_, _]
  def typedIdentity[A](o: OO[A]): MM[A, A]
  def typedSource[A, B](m: MM[A, B]): OO[A]
  def typedTarget[A, B](m: MM[A, B]): OO[B]
  def typedCompose[A, B,C](m1: MM[A, B], m2: MM[B,C]): MM[A,C]
  override def identity(o: O): M = typedIdentity(o)
  override def source(m: M): O = typedSource(m)
  override def target(m: M): O = typedTarget(m)
  override def compose(m1: M, m2: M): M = typedCompose(m1.asInstanceOf[MM[Any, Any]], m2.asInstanceOf[MM[Any, Any]]) // Fake it, but Scala is faking it anyway.
}

trait Sets extends TypedCategory[Sets] {
  type OO[A] = Set[A]
  type MM[A, B] = Function[A ,B]

  override def typedIdentity[A](set: Set[A]) = set.identity
  override def typedSource[A, B](m: Function[A, B]) = m.source
  override def typedTarget[A, B](m: Function[A, B]) = m.target
  override def typedCompose[A, B,C](m1: Function[A, B], m2: Function[B, C]) = m1 andThen m2
  def bijections[A](set1: Set[A], set2: Set[A]) = {
    (set1.sizeIfFinite, set2.sizeIfFinite) match {
      case (Some(k), Some(l)) if k == l => {
        val set2List = set2.toIterable.toList
        for(p <- Permutations.of(k)) yield {
          val m = (for((x, i) <- set2.toIterable zip p) yield {
            x -> set2List(i)
          }).toMap
          Function(set1, set2, m)
        }
      }
      case _ => NonStrictIterable()
    }
  }
}

object Sets extends Sets
