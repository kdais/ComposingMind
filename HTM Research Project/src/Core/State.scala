package Core

class State[S, A](val run : State.Transition[S, A]) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = new State(s => {
    val (a, s1) = run(s)
    f(a).run(s1())
  })
}

object State {
  type LazyState[S] = () => S
  
  type Transition[S, A] = S => (A, LazyState[S])
  
  def unit[S, A](a: A): State[S, A] =
    new State(s => (a, lazyState(s)))

  def lazyState[S](s: => S): LazyState[S] = () => s
}