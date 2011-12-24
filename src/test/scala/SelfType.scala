trait Example {
  type O
  def apply(o: O)
  def f(o: O) = this.apply(o)
}

