#!/bin/bash
# Micro benchmark of virtual calls on open types: the same program linked
# closed and as a host with its hierarchies open (release-fast).
#
# usage: bench.sh [runs]; ENV_SH as for run.sh
set -e
source "${ENV_SH:?set ENV_SH to the environment file}"
HERE=$(cd "$(dirname "$0")" && pwd)
RUNS=${1:-5}
W=$WORK/bench
rm -rf $W; mkdir -p $W
snc_compile $W/nir "" $HERE/bench/Bench.scala
LINK=(--main bench.Bench --mode release-fast --multithreading false)
$SNLINK "${LINK[@]}" --base-dir $W/closed $W/nir $LCP > $W/closed.log 2>&1 || {
  tail -30 $W/closed.log; exit 1; }
$SNLINK "${LINK[@]}" --open bench.Shape --open bench.Mono --open bench.Poly --open bench.Op \
  --open java.lang.Object --base-dir $W/open $W/nir $LCP > $W/open.log 2>&1 || {
  tail -30 $W/open.log; exit 1; }
for i in 1 2 3; do
  echo "== closed, round $i"; $W/closed/out $RUNS
  echo "== open, round $i"; $W/open/out $RUNS
done
