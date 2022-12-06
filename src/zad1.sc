def lfrom(n:Int):Stream[Int] = {
  n #:: lfrom(n + 1)
}

// Zad 1
def lrepeatExponential[T](n: Int, s: Stream[T]): Stream[T] = {
  def lrepeatExponential_rec(i: Int, x: T, remaining: () => Stream[T]): Stream[T] = {
    if (i > 1) {
      x #:: lrepeatExponential_rec(i - 1, x, remaining)
    } else {
      x #:: remaining()
    }
  }

  if (s.isEmpty) {
    Stream.empty
  } else {
    val x #:: xs = s
    lrepeatExponential_rec(n, x, () => lrepeatExponential(n+1, xs))
  }
}

println(lrepeatExponential(1, lfrom(1)).take(15).toList)