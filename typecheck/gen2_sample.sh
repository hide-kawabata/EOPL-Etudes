export GEN="./gen2"
echo ${GEN} sample/fib.eopl
${GEN} < sample/fib.eopl > sample/fib.asm
echo ${GEN} sample/prim.eopl
${GEN} < sample/prim.eopl > sample/prim.asm
echo ${GEN} sample/proc.eopl
${GEN} < sample/proc.eopl > sample/proc.asm
echo ${GEN} sample/sum.eopl
${GEN} < sample/sum.eopl > sample/sum.asm
echo ${GEN} sample/sum_cont.eopl
${GEN} < sample/sum_cont.eopl > sample/sum_cont.asm
