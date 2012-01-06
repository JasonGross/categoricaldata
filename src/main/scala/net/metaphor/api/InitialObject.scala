package net.metaphor.api

trait TerminalObject { category: Category => 
  def terminalObject: category.O
  def morphismToTerminalObject(o: category.O): category.M
}
trait InitialObject { category: Category => 
  def initialObject: category.O
  def morphismFromInitialObject(o: category.O): category.M
}