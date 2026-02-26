final: previous: {
  fava = previous.fava.overridePythonAttrs (old: {
    patches = builtins.filter
      (p: !(builtins.isString (builtins.toString p) && builtins.match ".*fix-have-excel-replacement.*" (builtins.toString p) != null))
      (old.patches or []);
  });
}
