package net.categoricaldata.universalalgebra
import net.categoricaldata.category._

trait Limits {

}

object Limits {
  trait usingProductsAndEqualizers extends Limits { self: Category with Products with Equalizers =>
    
  }
}