import fp/option
import fp/list
import fp/either
import sugar

proc noop*() = discard
proc noop*(a: auto) = discard

proc bitap*[T](xs: Option[T], errFn: () -> void, succFn: T -> void): Option[T] =
  if (xs.isDefined):
    succFn(xs.get)
  else:
    errFn()
  xs

proc tap*[T](x: Option[T], f: T -> void): Option[T] =
  f(x)
  x

proc log*[T](x: any): void =
  discard echo(x)

proc tap*[E,A](xs: Either[E,A], f: A -> void): Either[E, A] =
  f(xs)
  xs

proc tap*[T](xs: List[T], f: T -> void): List[T] =
  f(xs)
  xs

proc orElse*[T](x: Option[T], noneX: T): T =
  if (x.isSome): return x.get
  else: noneX
