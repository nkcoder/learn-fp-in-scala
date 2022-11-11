package fp.chp6

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) => (A, S)

    def apply[S, A](f: S => (A, S)): State[S, A] = f
