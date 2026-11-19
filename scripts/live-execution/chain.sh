#!/bin/bash
# Chained extensions, linked the way the REPL links its lines.
#
# The host (chain/Host.scala) knows none of the lines and is linked with
# everything open and callable (`--open '*' --callable '*' --stub-unlinkable`,
# as the REPL's evaluator is). Each line is compiled against the earlier ones,
# linked as an extension against the host's manifest and the earlier lines'
# deltas (`--chain`), and writes its own (`--delta`). Line 2 subclasses line
# 1's class and implements its trait; line 3 subclasses line 2's class and
# calls `super`; all of them use line 1's module state, which must not be
# computed again; type tests and pattern matches cross the lines.
#
# usage: chain.sh [debug|release-fast]; ENV_SH as for run.sh
set -e
source "${ENV_SH:?set ENV_SH to the environment file}"
HERE=$(cd "$(dirname "$0")" && pwd)
source "$HERE/platform.sh"
MODE=${1:-debug}
W=$WORK/chain-$MODE
rm -rf $W; mkdir -p $W

snc_compile $W/nir-host "" $HERE/chain/Host.scala
$SNLINK --main Host --mode $MODE --multithreading true --no-optimize \
  --open '*' --callable '*' --stub-unlinkable \
  --callable '-scala.scalanative.runtime.Intrinsics*' --callable '-scala.scalanative.unsafe.CFuncPtr*' \
  --base-dir $W/host $W/nir-host $LCP > $W/host.log 2>&1 || {
  tail -40 $W/host.log; exit 1; }
grep "Live execution" $W/host.log || true
echo "host: $(size_of $W/host/out) bytes, manifest $(size_of $W/host/out.snm) bytes"

cp=$W/nir-host
chain=()
bundles=()
for n in 1 2 3; do
  snc_compile $W/line$n "$cp" $HERE/chain/Line$n.scala 2> $W/compile$n.log
  cp=$cp:$W/line$n
  $SNLINK --extension --host $W/host/out.snm --ext $W/line$n "${chain[@]}" \
    --delta $W/delta$n.snm \
    --base-dir $W/link$n --module line$n --no-optimize --compile-option -O0 \
    $(echo $cp | tr ':' ' ') $LCP > $W/link$n.log 2>&1 || {
    tail -40 $W/link$n.log; exit 1; }
  echo "line $n: $(grep -E 'Linking \(|Compiling and linking|Total' $W/link$n.log | sed 's/\[info\] //' | tr '\n' ' ')"
  chain+=(--chain $W/delta$n.snm)
  bundles+=($W/link$n/line$n.$SO "rs\$runner\$$n\$")
done
# A line that never finishes loading must not hang the caller
$W/host/out "${bundles[@]}" & pid=$!
( sleep 120; kill $pid 2>/dev/null && echo "BLOCKED: stopped after 120 s" ) & watch=$!
status=0
wait $pid || status=$?
kill $watch 2>/dev/null || true
exit $status
