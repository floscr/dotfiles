import strformat

proc polyBarPrettyPrint(text: string, color: string, prefix: string): string =
  &"%{{F{color}}}{prefix} {text}%{{F-}}"
