Ten-hundred is a graphical explanation editor.

## Building
Compile with Leiningen: `lein cljsbuild (auto | once)` and `lein scss (auto | once)`. To recompile the grammar, `pegjs -e THParser pegjs/grammar.pegjs lib/grammar-parser.js`.
