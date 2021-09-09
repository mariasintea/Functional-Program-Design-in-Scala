package generators

trait Generator[+T] {
  self => // an alias for "this"
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)
    // a call like f(generate) would refer to this.generate and would create a recursive call
    // using self we refer the outer class's method; similar to Generator.this.generate
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate).generate
  }
}