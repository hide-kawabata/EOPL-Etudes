export GEN="./gen2"
echo ${GEN} test/add.eopl
${GEN} < test/add.eopl > test/add.asm
echo test/add2.eopl
${GEN} < test/add2.eopl > test/add2.asm
echo test/even.eopl
${GEN} < test/even.eopl > test/even.asm
echo test/if.eopl
${GEN} < test/if.eopl > test/if.asm
echo test/let.eopl
${GEN} < test/let.eopl > test/let.asm
echo test/let2.eopl
${GEN} < test/let2.eopl > test/let2.asm
echo test/proc.eopl
${GEN} < test/proc.eopl > test/proc.asm
echo test/proc1.eopl
${GEN} < test/proc1.eopl > test/proc1.asm
echo test/proc2.eopl
${GEN} < test/proc2.eopl > test/proc2.asm
echo test/proc3.eopl
${GEN} < test/proc3.eopl > test/proc3.asm
echo test/proc4.eopl
${GEN} < test/proc4.eopl > test/proc4.asm
echo test/proc5.eopl
${GEN} < test/proc5.eopl > test/proc5.asm
echo test/val.eopl
${GEN} < test/val.eopl > test/val.asm
echo test/tail.eopl
${GEN} < test/tail.eopl > test/tail.asm
