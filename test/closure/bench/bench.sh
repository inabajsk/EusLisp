#!/bin/bash
# bench.sh <env.sh> <label>
. $1; L=$2; D=$(cd $(dirname $0); pwd); W=$D/work-$L; rm -rf $W; mkdir -p $W
cp $D/bench-defs.l $W/; (cd $W; echo '(progn (compile-file "bench-defs.l") (exit 0))' | eus > compile.log 2>&1)
export BDIR=$W
for m in interp comp; do MODE=$m eus $D/bench-run.l < /dev/null 2>&1 | grep BENCH; done | sed "s/^BENCH/$L/"
