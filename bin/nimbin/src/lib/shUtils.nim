import osproc
import strutils
import osproc
import fp/either
import sugar

proc sh*(cmd: string, opts = {poStdErrToStdOut}): Either[string, string] =
  let (res, exitCode) = execCmdEx(cmd, opts)
  if exitCode == 0:
    return res
        .strip
        .right(string)
  return res
    .strip
    .left(string)
