package net.categoricaldata.universalalgebra
import net.categoricaldata.category._

trait Diagrams { category: Category => 
  trait Diagram {  
	trait Cone {
	  def coneObject: O
	  def coneMap(o: O): M
	}
	case class ConeMap(source: Cone, target: Cone, mapBetweenConeObjects: M)
  
	trait Cones extends Category {
	  override type O = Cone
	  override type M = ConeMap
	  
	  override def source(m: ConeMap) = m.source
	  override def target(m: ConeMap) = m.target
	  override def identity(o: Cone) = ConeMap(o, o, category.identity(o.coneObject))
	  override def compose(m1: ConeMap, m2: ConeMap) = ConeMap(m1.source, m2.target, category.compose(m1.mapBetweenConeObjects, m2.mapBetweenConeObjects))
	}   
  }
}

trait Limits extends Diagrams { self: Category =>
	def limit(d: Diagram): d.Cones with InitialObject
	def limitCone(d: Diagram) = limit(d).initialObject
	def limitObject(d: Diagram) = limitCone(d).coneObject
}

object Limits {
  trait usingProductsAndEqualizers extends Limits { self: Category with Products with Equalizers =>
    
  }
}