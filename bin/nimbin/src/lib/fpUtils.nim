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
  f(x.get)
  x

proc log*[T](x: any): void =
  discard echo(x)

proc tap*[T](xs: List[T], f: T -> void): List[T] =
  f(xs)
  xs

proc orElse*[T](x: Option[T], noneX: T): T =
  if (x.isSome): return x.get
  else: noneX

proc tap*[E,A,B](e: Either[E,A], f: A -> B): Either[E,A] =
  if e.isRight: discard f(e.get)
  e

proc log*[E,A](e: Either[E,A]): Either[E,A] =
  echo $e
  e

## find gets overwritten by system and there is no way to shadow it...
proc findX*[T](xs: List[T], p: T -> bool): Option[T] =
  ## Finds the first element that satisfies the predicate `p`
  if xs.isEmpty:
    T.none
  else:
    if p(xs.head): xs.head.some else: xs.tail.find(p)
