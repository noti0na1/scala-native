#!/bin/bash
# An extension linked separately against a host's manifest and loaded into
# the running host.
#
# The host knows only the API (api/), its own implementation of the open
# types (HostPlugin) and, for the marker test, a copy of the extension in
# package `hostcopy` whose strings say HOST COPY. The extension (ext/) is
# compiled against the host's NIR and linked with `snlink --extension`
# against the host's manifest; its classes are unknown to the host. Output
# naming the host copy, or run(1) = 1009, means host code ran in the
# extension's place.
#
# usage: run.sh <debug|release-fast> <multithreading true|false> <threads> <open|closed>
#   open: the host has a second implementation of the open types and the copy
#   closed: it has neither, so the phantoms are the only other targets
#
# ENV_SH names a file that sets: SNC_COMPILE (a function: snc_compile <out>
# <classpath> <sources...>), SNLINK (command), LCP (link class path entries),
# WORK (output directory).
set -e
source "${ENV_SH:?set ENV_SH to the environment file}"
HERE=$(cd "$(dirname "$0")" && pwd)
source "$HERE/platform.sh"
MODE=${1:-debug}; MT=${2:-false}; THREADS=${3:-1}; WORLD=${4:-open}
W=$WORK/run-$MODE-mt$MT-$WORLD
rm -rf $W; mkdir -p $W/src/host $W/src/hostcopy

if [ $WORLD = open ]; then
  cp $HERE/host/Host.scala $W/src/host/Host.scala
  sed -e 's/^package ext$/package hostcopy/' -e 's/ORIGIN/HOST COPY/g' \
    $HERE/ext/Ext.scala > $W/src/hostcopy/Ext.scala
  HOSTSRC="$HERE/api/Api.scala $W/src/host/Host.scala $W/src/hostcopy/Ext.scala"
else
  grep -v '// OPEN' $HERE/host/Host.scala > $W/src/host/Host.scala
  HOSTSRC="$HERE/api/Api.scala $W/src/host/Host.scala"
fi
sed 's/ORIGIN/extension/g' $HERE/ext/Ext.scala > $W/src/Ext.scala

snc_compile $W/nir-host "" $HOSTSRC
snc_compile $W/nir-ext $W/nir-host $W/src/Ext.scala

OPEN=(--open 'api.*' --open java.lang.Object --open 'scala.Function*' --open 'scala.runtime.java8.*' --open 'scala.runtime.function.*' --open java.io.Serializable --open scala.Product --open scala.Equals)
CALLABLE=(--callable 'api.*' --callable 'scala.Array$' --callable scala.runtime.IntRef)
$SNLINK --main Host --mode $MODE --multithreading $MT "${OPEN[@]}" "${CALLABLE[@]}" \
  --base-dir $W/host $W/nir-host $LCP > $W/host.log 2>&1 || {
  tail -40 $W/host.log; exit 1; }
grep "Live execution" $W/host.log || true
echo "host: $(size_of $W/host/out) bytes, $(exported_symbols $W/host/out) exported symbols, manifest $(size_of $W/host/out.snm) bytes"

$SNLINK --extension --host $W/host/out.snm --ext $W/nir-ext --module ext \
  --base-dir $W/ext $W/nir-host $LCP > $W/ext.log 2>&1 || {
  tail -40 $W/ext.log; exit 1; }
echo "extension: $(size_of $W/ext/ext.$SO) bytes, $(cat $W/ext/native/generated/*.ll | wc -l | tr -d ' ') lines of IR"
cp $W/ext/ext.$SO $W/ext/ext-copy.$SO

run() {
  "$@" & local pid=$!
  ( sleep 120; kill $pid 2>/dev/null && echo "BLOCKED: stopped after 120 s" ) & local watch=$!
  wait $pid || echo "exit status $?"
  kill $watch 2>/dev/null || true
}
echo "== default GC"
run $W/host/out $W/ext/ext.$SO $THREADS $W/ext/ext-copy.$SO
echo "== GC_INITIAL_HEAP_SIZE=1m GC_NPROCS=1"
GC_INITIAL_HEAP_SIZE=1m GC_NPROCS=1 run $W/host/out $W/ext/ext.$SO $THREADS
