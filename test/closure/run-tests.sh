#!/bin/bash
# usage: run-tests.sh <env.sh of the EusLisp build> <eus command: eus|irteus> <label>
# Runs every closure test in its own process, interpreted and compiled.
ENVSH=$1; EUSCMD=${2:-eus}; LABEL=${3:-run}
. $ENVSH
export TESTDIR=$(cd $(dirname $0); pwd)
WORK=$TESTDIR/work-$LABEL; rm -rf $WORK; mkdir -p $WORK
cp $TESTDIR/closure-defs.l $WORK/
# CLOSURE_MODE=nil compiles and runs with *closure-mode* = nil (original closures)
if [ "$CLOSURE_MODE" = "nil" ]; then SETMODE='(setq *closure-mode* nil)'; else SETMODE=''; fi
export CLOSURE_MODE
( cd $WORK && echo "(progn $SETMODE (compile-file \"closure-defs.l\") (exit 0))" | $EUSCMD > compile.log 2>&1 )
export TESTDIR=$WORK
NAMES=$(sed -n "/setq \*closure-tests\*/,\$p" $WORK/closure-defs.l | grep -o "(test-[a-z0-9-]*" | tr -d "(")
for MODE in interp comp; do
  export MODE
  for T in $NAMES; do
    OUT=$(TEST=$T timeout 30 $EUSCMD $TESTDIR/../run-one.l < /dev/null 2>&1)
    RC=$?
    LINE=$(echo "$OUT" | grep -E '^(PASS|FAIL) ' | tail -1)
    if [ -z "$LINE" ]; then
      ERR=$(echo "$OUT" | grep -iE 'segmentation|error|fatal' | head -1 | tr -d '\r' | cut -c1-90)
      LINE="CRASH $T rc=$RC ${ERR}"
    fi
    echo "$MODE $LINE"
  done
done | tee $WORK/results.txt
echo "summary $LABEL: $(grep -c ' PASS ' $WORK/results.txt) pass / $(wc -l < $WORK/results.txt) total"
