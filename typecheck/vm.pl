#! /usr/bin/perl

$stack_ptr = "R30";
$frame_ptr = "R31";


$count = 0;
while (<>) {
    next if /^\*/;
#    print $_;

    chop;
    s/!.*//;
    s/[: \t,]+/@/g;
    @tmp = split(/@/);

    $label[$count] = $tmp[0];
    $op[$count] = $tmp[1];
    $opr1[$count] = $tmp[2];
    $opr2[$count] = $tmp[3];
    # printf "label:[%s] op:[%s] opr1:[%s] opr2:[%s]\n",
    # 	$label[$count],
    # 	$op[$count],
    # 	$opr1[$count],
    # 	$opr2[$count];
    $count += 1;
}


sub findpc {
    ($arg) = @_;
    print "(findpc) arg=$arg\n";
    for ($i = 0; $i < $count; $i++) {
	if ($label[$i] =~ /$arg/) {
	    return $i;
	}
    }
    return -1;
}



$pc = 0;
$sp = 0;
$fp = 0;

while ($op[$pc] !~ /HLT/) {
#    print $op[$pc], "\n";

    if ($op[$pc] =~ /LD/) {
	if ($opr2[$pc] =~ /^\[R(.+)\+(.+)\]$/) { # [R0+-3]
	    $reg{$opr1[$pc]} = $mem{$reg{"R$1"} + $2};
	    print "LD ", $opr1[$pc], " [R$1+$2] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^\[R(.+)\]$/) { # [R0]
	    $reg{$opr1[$pc]} = $mem{$reg{"R$1"}};
	    print "LD ", $opr1[$pc], " [R$1] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^R([0-9]*)$/) { # R0
	    $reg{$opr1[$pc]} = $reg{$opr2[$pc]};
	    print "LD $opr1[$pc], R$1 -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^([0-9]+)$/) { # 32
	    $reg{$opr1[$pc]} = $1;
	    print "LD ", $opr1[$pc], " $1 -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /(.+)/) { # label
	    $reg{$opr1[$pc]} = &findpc($1);
	    print "LD $opr1[$pc], $1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} else {
	    print "LD ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /ST/) {
	if ($opr2[$pc] =~ /^\[R(.+)\+(.+)\]$/) { # [R0+-3]
	    $mem{$reg{"R$1"} + $2} = $reg{$opr1[$pc]};
	    print "ST ", $opr1[$pc], " [R$1+$2] => [",
		$reg{"R$1"} + $2, "] = ", 
		$mem{$reg{"R$1"} + $2}, "\n";
	} elsif ($opr2[$pc] =~ /^\[R([^+]+)\]$/) { # [R0]
	    $mem{$reg{"R$1"}} = $reg{$opr1[$pc]};
	    print "ST ", $opr1[$pc], " [R$1] -> [",
		$reg{"R$1"}, "] = ", 
		$mem{$reg{"R$1"}}, "\n";

	# } elsif ($opr2[$pc] =~ /^R([0-9]*)$/) { # R0
	#     $reg{$opr2[$pc]} = $reg{$opr1[$pc]};
	#     print "ST $opr1[$pc], R$1 -> $opr1[$pc] = ", 
	# 	$reg{$opr1[$pc]}, "\n";
	# } elsif ($opr2[$pc] =~ /^([0-9]+)$/) { # 32
	#     $reg{$opr1[$pc]} = $1;
	#     print "ST ", $opr1[$pc], " $1 -> $opr1[$pc] = ", 
	# 	$reg{$opr1[$pc]}, "\n";
	# } elsif ($opr2[$pc] =~ /(.+)/) { # label
	#     $reg{$opr1[$pc]} = &findpc($1);
	#     print "ST $opr1[$pc], $1 -> $opr1[$pc] = ",
	# 	$reg{$opr1[$pc]}, "\n";

	} else {
	    print "ST ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /ADD/) {

	if ($opr2[$pc] =~ /^\[R(.+)\+(.+)\]$/) { # [R0+-3]
	    $reg{$opr1[$pc]} += $mem{$reg{"R$1"} + $2};
	    print "ADD ", $opr1[$pc], " [R$1+$2] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^\[R(.+)\]$/) { # [R0]
	    $reg{$opr1[$pc]} += $mem{$reg{"R$1"}};
	    print "ADD ", $opr1[$pc], " [R$1]\n";
	} elsif ($opr2[$pc] =~ /^R([0-9]*)$/) { # R0
	    $reg{$opr1[$pc]} += $reg{$opr2[$pc]};
	    print "ADD ", $opr1[$pc], " R$1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^([0-9]+)$/) { # 32
	    $reg{$opr1[$pc]} += $1;
	    print "ADD ", $opr1[$pc], " $1 -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /(.+)/) { # label
	    print "ADD ---", $opr1[$pc], " $1\n";
	} else {
	    print "ADD ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /SUB/) {

	if ($opr2[$pc] =~ /^\[R(.+)\+(.+)\]$/) { # [R0+-3]
	    $reg{$opr1[$pc]} -= $mem{$reg{"R$1"} + $2};
	    print "SUB ", $opr1[$pc], " [R$1+$2] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^\[R(.+)\]$/) { # [R0]
	    $reg{$opr1[$pc]} -= $mem{$reg{"R$1"}};
	    print "SUB ", $opr1[$pc], " [R$1]\n";
	} elsif ($opr2[$pc] =~ /^R([0-9]*)$/) { # R0
	    $reg{$opr1[$pc]} -= $reg{$opr2[$pc]};
	    print "SUB ", $opr1[$pc], " R$1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /^([0-9]+)$/) { # 32
	    $reg{$opr1[$pc]} -= $1;
	    print "SUB ", $opr1[$pc], " $1 -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ /(.+)/) { # label
	    print "SUB ---", $opr1[$pc], " $1\n";
	} else {
	    print "SUB ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /CMP/) {

	if ($opr2[$pc] =~ /^\[R(.+)\+(.+)\]$/) { # [R0+-3]
	    $arg2 = $mem{$reg{"R$1"} + $2};
	} elsif ($opr2[$pc] =~ /^\[R(.+)\]$/) { # [R0]
	    $arg2 = $mem{$reg{"R$1"}};
	} elsif ($opr2[$pc] =~ /^R([0-9]*)$/) { # R0
	    $arg2 = $reg{$opr2[$pc]};
	} elsif ($opr2[$pc] =~ /^([0-9]+)$/) { # 32
	    $arg2 = $1;
	} elsif ($opr2[$pc] =~ /(.+)/) { # label
	    $arg2 = "---";
	    print "CMP ---", $opr1[$pc], " $1\n";
	} else {
	    $arg2 = "---";
	    print "CMP ??? <", $opr2[$pc], ">\n";
	}
	if ($arg2 !~ /---/) {
	    if ($reg{$opr1[$pc]} > $arg2) {
		$zf = 0;
		$nf = 0;
	    } elsif ($reg{$opr1[$pc]} < $arg2) {
		$zf = 0;
		$nf = 1;
	    } elsif ($reg{$opr1[$pc]} == $arg2) {
		$zf = 1;
		$nf = 0;
	    }
	}
	print "CMP $opr1[$pc] $opr2[$pc] -> zf=$zf nf=$nf\n";
	$pc++;

    } elsif ($op[$pc] =~ /BA/) {

	$nextpc = &findpc($opr1[$pc]);
	print "BA -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BZ/) {

	if ($zf > 0) {
	    $nextpc = &findpc($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BLT/) {

	if ($nf > 0) {
	    $nextpc = &findpc($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BGT/) {

	if ($nf == 0 && $zf == 0) {
	    $nextpc = &findpc($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /PUSH/) {

	$reg{$stack_ptr} -= 1;
#	$reg{"$stack_ptr"} -= 1;
#	$reg{"R30"} -= 1;
	$mem{$reg{$stack_ptr}} = $reg{$opr1[$pc]};
#	$mem{$reg{"$stack_ptr"}} = $reg{$opr1[$pc]};
#	$mem{$reg{"R30"}} = $reg{$opr1[$pc]};
	print "PUSH : $opr1[$pc] = ", $reg{$opr1[$pc]}, 
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";
#	    " $stack_ptr = ", $reg{"$stack_ptr"}, "\n";
#	    " $stack_ptr = ", $reg{"R30"}, "\n";
	$pc += 1;

    } elsif ($op[$pc] =~ /POP/) {

	$reg{$opr1[$pc]} = $mem{$reg{$stack_ptr}};
#	$reg{$opr1[$pc]} = $mem{$reg{"$stack_ptr"}};
#	$reg{$opr1[$pc]} = $mem{$reg{"R30"}};
	$reg{$stack_ptr} += 1;
#	$reg{"$stack_ptr"} += 1;
#	$reg{"R30"} += 1;
	print "POP -> $opr1[$pc] = ", $reg{$opr1[$pc]},
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";
#	    " $stack_ptr = ", $reg{"$stack_ptr"}, "\n";
#	    " R30 = ", $reg{"R30"}, "\n";
	$pc += 1;

    } elsif ($op[$pc] =~ /CALLR/) {

	# push pc+1
	$reg{$stack_ptr} -= 1;
#	$reg{"$stack_ptr"} -= 1;
#	$reg{"R30"} -= 1;
	$mem{$reg{$stack_ptr}} = $pc+1;
#	$mem{$reg{"$stack_ptr"}} = $pc+1;
#	$mem{$reg{"R30"}} = $pc+1;
	print "CALLR $opr1[$pc] -> RA = ", $pc+1,
	    " Addr = ", $reg{"$stack_ptr"},
#	    " Addr = ", $reg{"R30"},
	    " PC = $reg{$opr1[$pc]}\n";
	$pc = $reg{$opr1[$pc]};

    } elsif ($op[$pc] =~ /RET/) {

	# pop pc+1
	$pc = $mem{$reg{$stack_ptr}};
#	$pc = $mem{$reg{"R30"}};
	$reg{$stack_ptr} += 1;
#	$reg{"$stack_ptr"} += 1;
#	$reg{"R30"} += 1;
	print "RET -> RA = ", $pc,
	    " Addr = ", $reg{$stack_ptr},
#	    " Addr = ", $reg{"$stack_ptr"},
#	    " Addr = ", $reg{"$stack_ptr"},
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";
#	    " $stack_ptr = ", $reg{"$stack_ptr"}, "\n";

    } elsif ($op[$pc] =~ /OUT/) {

	print "OUT $opr1[$pc] ($opr1[$pc] = ", 
	    $reg{$opr1[$pc]}, ")\n";
	$pc++;

    } elsif ($op[$pc] =~ /END/) {
	last;
    } elsif ($label[$pc] =~ /.+/) { # label
	print $label[$pc], ":\n";
	$pc++;
    } else {
	print "(???) <", $op[$pc], ">\n";
	$pc++;
    }       
}
