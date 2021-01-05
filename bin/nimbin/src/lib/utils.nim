import macros
import sequtils
import fp/option
import fp/list
import sugar
import osproc
import strutils

{.experimental.}

proc shellCommand*(c: string): seq[string] =
  execProcess(c).strip(chars = {'\n'}).splitLines()

func grep*(sa: seq[string], s:string) : seq[string] = sa.filter(proc(l:string) : bool = s in l)
func getColumn*(s: string, n: int) : string = s.strip().splitWhiteSpace()[n]

template findIt*(coll, cond): untyped =
  var res: typeof(coll.items, typeOfIter)
  for it {.inject.} in coll:
    if not cond: continue
    res = it
    break
  res

proc optionIndex*[T](xs: openArray[T], i: int): Option[T] =
  if (xs.len > i): return some(xs[i])

proc last*[T](s: openArray[T], predicate: proc(el: T): bool): Option[T] =
    ## Return the last element of openArray s that match the predicate encapsulated as Option[T].
    ## If no one element match it the function returns none(T)
    var lastValue: Option[T] = none(T)
    for el in s:
        if predicate(el):
            lastValue = some(el)
    return lastValue

macro `|>`*(lhs, rhs: untyped): untyped =
  case rhs.kind:
  of nnkIdent: # single-parameter functions
    result = newCall(rhs, lhs)
  else:
    result = rhs
    result.insert(1, lhs)
