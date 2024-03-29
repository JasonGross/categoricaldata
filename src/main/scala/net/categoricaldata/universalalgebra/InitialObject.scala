package net.categoricaldata.universalalgebra
import net.categoricaldata.category._

trait TerminalObject { category: Category => 
  def terminalObject: category.O
  def morphismToTerminalObject(o: category.O): category.M
}
trait InitialObject { category: Category => 
  def initialObject: category.O
  def morphismFromInitialObject(o: category.O): category.M
}
