package net.metaphor.api2

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import net.tqft.toolkit.Extractors.Int

@RunWith(classOf[JUnitRunner])
class FinitelyPresentedCategoriesTest extends FlatSpec with ShouldMatchers {
  
  def Ord(n: Int) = new FinitelyPresentedCategories.FinitelyPresentedCategory {
    val boxes = for(i <- (0 to n).toList) yield Box(i.toString)
    def arrows(s: Box, t: Box) = (s.label, t.label) match {
      case (Int(i), Int(j)) if j == i + 1 => List(Arrow(s, t, ""))
          case _ => Nil
    }
  }
  
  import CategoriesOfSets.Sets
  
  val o = Ord(1)
  val foo = new o.FunctorsToSet.Functor {
   override def onObjects(b: SO) = Set(b.label)
   override def onMorphisms[X <: SO, Y <: SO](m: SM[X, Y]) = {
     Sets.Function(onObjects(m.source), onObjects(m.target), { i: String => i } )
   }
  }
  
  "identity0" should "not explode" in {
    FinitelyPresentedCategories.identity0(Ord(1))
  }
}

