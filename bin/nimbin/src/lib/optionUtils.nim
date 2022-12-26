import strutils
import fp/option
import sugar

proc mapWhen*[T,U](o: Option[T], p: Option[T] -> bool, f: T -> U): Option[U] =
  ## Maps nil object to none
  if o.isDefined and p(o) == true:
    o.map((x: T) => f(x))
  else:
    o

proc log*[T](o: Option[T]): Option[T] =
  if o.isDefined:
    echo(o.get())
  o
