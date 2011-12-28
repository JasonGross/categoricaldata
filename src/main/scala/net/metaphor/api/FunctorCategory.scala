//package net.metaphor.api
//
//abstract class FunctorCategory(source: Category, target: Category) extends Category { self =>
//  override type M = HeteroNaturalTransformation
//  override type O = HeteroFunctor
//  override def identity(o: O) = new NaturalTransformation.IdentityHeteroNaturalTransformation(o)
//  override def source(m: M) = {
//    m.source
//  }
//  override def target(m: M) = {
//    m.target
//  }
//  override def compose(m1: M, m2: M) = new HeteroNaturalTransformation {
//    val source = m1.source
//    val target = m2.target
//
//    def apply(o: sourceCategory.O) = target.target.compose(
//        m1(o.asInstanceOf[m1.sourceCategory.O]).asInstanceOf[this.target.target.M],
//        m2(o.asInstanceOf[m2.sourceCategory.O]).asInstanceOf[this.target.target.M]).asInstanceOf[targetCategory.M]
//  }
//
//}
