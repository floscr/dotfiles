---
name: cli
description: Build CLI tools using Babashka and babashka.cli. Scaffolds new CLIs as standalone bins or multi-command tools following project conventions.
---

# Build CLI Skill

Build Babashka CLI tools that follow this project's conventions. There are two patterns depending on complexity.

## Pattern 1: Simple Bin (single-purpose tool)

For tools with one job (like `browser`, `image_to_text`). Lives in `src/bins/` with a shell wrapper in `bin/`.

### Structure

```
src/bins/<name>.clj       # Implementation with -main
bin/<name>                 # Shell wrapper
```

### Shell wrapper (`bin/<name>`)

```bash
#!/usr/bin/env bash
set -euo pipefail

exec bb --config "$HOME/Code/My/bb.edn" -m bins.<name> "$@"
```

Make executable: `chmod +x bin/<name>`

### Implementation (`src/bins/<name>.clj`)

```clojure
(ns bins.<name>
  (:require [babashka.process :refer [shell]]))

(defn -main [& args]
  ;; implementation
  )
```

## Pattern 2: Multi-command CLI (like scdl)

For tools with subcommands, options, and help text. Gets its own namespace tree under `src/<name>/`.

### Structure

```
src/<name>/
  core.clj                 # Entry point, dispatch
  table.clj                # Help generation, dispatch (copy from template)
  commands/
    <command>.clj           # One file per command group
bin/<name>                  # Shell wrapper
```

### Shell wrapper (`bin/<name>`)

```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

exec bb --config "$PROJECT_ROOT/bb.edn" -m <name>.core "$@"
```

### Entry point (`src/<name>/core.clj`)

```clojure
(ns <name>.core
  (:require [babashka.cli :as cli]
            [<name>.table :as table]
            [<name>.commands.<cmd> :refer [<cmd>-cmd]]))

(def cli-config
  {:name "<name>"
   :description "What this tool does"
   :usage "<name> [options] <args>"
   :commands [{:name "<cmd>"
               :description "Description"}
              {:name "help"
               :description "Show help"}]
   :options [{:flags ["-f" "--flag"]
              :description "A boolean flag"}
             {:flags ["-o" "--option"]
              :description "An option with value"}]})

(def cli-spec
  {:flag {:alias :f :desc "A boolean flag"}
   :option {:alias :o :desc "An option"}
   :help {:alias :h :desc "Show help"}})

(defn help-cmd [_]
  (table/print-help cli-config))

(def command-table
  {"help" help-cmd
   :default <cmd>-cmd})

(defn -main [& args]
  (let [parsed (cli/parse-args args {:spec cli-spec})
        opts (:opts parsed)
        rest-args (:args parsed)]
    (cond
      (:help opts) (help-cmd nil)
      (empty? rest-args) (do (help-cmd nil) (System/exit 1))
      :else (table/dispatch command-table rest-args opts))))
```

### Help & dispatch (`src/<name>/table.clj`)

Copy from `ressources/templates/cli-template/src/cli/table.clj` or `src/scdl/table.clj`.

### Command file (`src/<name>/commands/<cmd>.clj`)

```clojure
(ns <name>.commands.<cmd>
  (:require [babashka.process :refer [shell]]))

(defn <cmd>-cmd
  "Description of what this command does."
  [{:keys [opts rest-args]}]
  (let [arg (first rest-args)]
    (when (nil? arg)
      (println "Error: argument required")
      (System/exit 1))
    ;; implementation
    ))
```

## Alternative: babashka.cli dispatch table pattern

For CLIs that need nested subcommands (like the main `my` CLI), use the dispatch table pattern from `ressources/templates/cli-template/`:

```clojure
(def table
  [{:cmds ["subcmd"]
    :fn subcmd-fn
    :desc "Description"
    :args->opts [:arg1]           ; positional args mapped to opt keys
    :coerce {:port :long
             :verbose :boolean}
    :alias {:p :port :v :verbose}
    :examples ["mycli subcmd arg"]}])
```

Type coercion options: `:long`, `:double`, `:boolean`, `:keyword`, `:symbol`, `:edn`, `[]` (vector), `#{}` (set).

## Available dependencies (from bb.edn)

These are already available - no need to add them:
- `babashka.cli` - CLI argument parsing
- `babashka.process` - Shell/subprocess (`shell`, `process`)
- `babashka.fs` - Filesystem operations
- `cheshire.core` - JSON parsing
- `clojure.string` - String utilities
- `clojure.java.io` - IO

## Conventions

- Command functions use `-cmd` suffix: `download-cmd`, `hello-cmd`
- Include `(comment ...)` block at bottom for REPL testing
- Use `System/exit 1` for error exits
- Use `babashka.process/shell` for subprocesses (inherits stdio)
- Helper functions use `defn-` (private)
- Expand `~` paths with `(str (System/getProperty "user.home") (subs path 1))`

## Checklist

1. Decide pattern: simple bin or multi-command CLI
2. Create source files following the pattern above
3. Create `bin/<name>` shell wrapper
4. `chmod +x bin/<name>`
5. Test: `bb --config bb.edn -m <namespace> <args>`
