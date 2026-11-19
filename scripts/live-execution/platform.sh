# Platform differences for run.sh and chain.sh: the extension file
# suffix (`snlink --extension` writes <module>.bundle on macOS, <module>.so
# elsewhere), file sizes, and the count of a binary's exported symbols.
if [ "$(uname -s)" = Darwin ]; then
  SO=bundle
  exported_symbols() { nm -gU "$1" | wc -l | tr -d ' '; }
else
  SO=so
  exported_symbols() { nm -D --defined-only "$1" | wc -l | tr -d ' '; }
fi
size_of() { wc -c < "$1" | tr -d ' '; }
