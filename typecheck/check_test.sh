export DIR=test
sh gen2_${DIR}.sh
sh vm_${DIR}.sh | grep OUT
sh rep_${DIR}.sh | grep :
