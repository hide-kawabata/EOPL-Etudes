#! /usr/bin/perl

# KUE-CHIP2-S Simulator
# Copyright (C) 2018 Hideyuki Kawabata

$stack_ptr = "R30";
$frame_ptr = "R31";

$count = 0;

while (<>) {
    next if /^\*/;
    chop;
    s/!.*//;
    s/[: \t,]+/@/g;
    @tmp = split(/@/);

    $label[$count] = $tmp[0];
    $op[$count] = $tmp[1];
    $opr1[$count] = $tmp[2];
    $opr2[$count] = $tmp[3];

    $count += 1;
}


$pc = 0;

while ($op[$pc] !~ /HLT/) {

    $pat1 = qr/^\[(R[0-9]*|ACC|IX)\+(-?[0-9][0-9A-Fa-f]*[Hh]?)\]$/; # [R0+-3]
    $pat2 = qr/^\[(R[0-9]*|ACC|IX)\]$/; # [R0]
    $pat3 = qr/^(R[0-9]*|ACC|IX)$/; # R0
    $pat4 = qr/^(-?[0-9][0-9A-Fa-f]*[Hh]?)$/; # 32
    $pat5 = qr/^([A-Za-z][A-Za-z0-9]*)$/; # label

    if ($op[$pc] =~ /LD/) {
	if ($opr2[$pc] =~ $pat1) { # [R0+-3]
	    $dis = &num2decimal($2);
	    $reg{$opr1[$pc]} = $mem{$reg{"$1"} + $dis};
	    print "LD ", $opr1[$pc], " [$1+$dis] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat2) { # [R0]
	    $reg{$opr1[$pc]} = $mem{$reg{"$1"}};
	    print "LD ", $opr1[$pc], " [$1] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat3) { # R0
	    $reg{$opr1[$pc]} = $reg{$opr2[$pc]};
	    print "LD $opr1[$pc], $1 -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat4) { # 32
	    $dis = &num2decimal($1);
	    $reg{$opr1[$pc]} = $dis;
	    print "LD ", $opr1[$pc], " $dis -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat5) { # label
	    $reg{$opr1[$pc]} = &findaddr($1);
	    print "LD $opr1[$pc], $1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} else {
	    print "LD ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /ST/) {
	if ($opr2[$pc] =~ $pat1) { # [R0+-3]
	    $dis = &num2decimal($2);
	    $mem{$reg{"$1"} + $dis} = $reg{$opr1[$pc]};
	    print "ST ", $opr1[$pc], " [$1+$dis] => [",
		$reg{"$1"} + $dis, "] = ", 
		$mem{$reg{"$1"} + $dis}, "\n";
	} elsif ($opr2[$pc] =~ $pat2) { # [R0]
	    $mem{$reg{"$1"}} = $reg{$opr1[$pc]};
	    print "ST ", $opr1[$pc], " [$1] -> [",
		$reg{"$1"}, "] = ", 
		$mem{$reg{"$1"}}, "\n";
	} else {
	    print "ST ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /ADD/) {

	if ($opr2[$pc] =~ $pat1) { # [R0+-3]
	    $dis = &num2decimal($2);
	    $reg{$opr1[$pc]} += $mem{$reg{"$1"} + $dis};
	    print "ADD ", $opr1[$pc], " [$1+$dis] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat2) { # [R0]
	    $reg{$opr1[$pc]} += $mem{$reg{"$1"}};
	    print "ADD ", $opr1[$pc], " [$1]\n";
	} elsif ($opr2[$pc] =~ $pat3) { # R0
	    $reg{$opr1[$pc]} += $reg{$opr2[$pc]};
	    print "ADD ", $opr1[$pc], " $1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat4) { # 32
	    $dis = &num2decimal($1);
	    $reg{$opr1[$pc]} += $dis;
	    print "ADD ", $opr1[$pc], " $dis -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat5) { # label
	    $dis = &findaddr($1);
	    $reg{$opr1[$pc]} += $dis;
	    print "ADD ", $opr1[$pc], " $dis -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} else {
	    print "ADD ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /SUB/) {

	if ($opr2[$pc] =~ $pat1) { # [R0+-3]
	    $dis = &num2decimal($2);
	    $reg{$opr1[$pc]} -= $mem{$reg{"$1"} + $dis};
	    print "SUB ", $opr1[$pc], " [$1+$dis] -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat2) { # [R0]
	    $reg{$opr1[$pc]} -= $mem{$reg{"$1"}};
	    print "SUB ", $opr1[$pc], " [$1]\n";
	} elsif ($opr2[$pc] =~ $pat3) { # R0
	    $reg{$opr1[$pc]} -= $reg{$opr2[$pc]};
	    print "SUB ", $opr1[$pc], " $1 -> $opr1[$pc] = ",
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat4) { # 32
	    $dis = &num2decimal($1);
	    $reg{$opr1[$pc]} -= $dis;
	    print "SUB ", $opr1[$pc], " $dis -> $opr1[$pc] = ", 
		$reg{$opr1[$pc]}, "\n";
	} elsif ($opr2[$pc] =~ $pat5) { # label
	    print "SUB ---", $opr1[$pc], " $1\n";
	} else {
	    print "SUB ??? <", $opr2[$pc], ">\n";
	}
	$pc++;

    } elsif ($op[$pc] =~ /CMP/) {

	if ($opr2[$pc] =~ $pat1) { # [R0+-3]
	    $arg2 = $mem{$reg{"$1"} + &num2decimal($2)};
	} elsif ($opr2[$pc] =~ $pat2) { # [R0]
	    $arg2 = $mem{$reg{"$1"}};
	} elsif ($opr2[$pc] =~ $pat3) { # R0
	    $arg2 = $reg{$opr2[$pc]};
	} elsif ($opr2[$pc] =~ $pat4) { # 32
	    $arg2 = &num2decimal($1);
	} elsif ($opr2[$pc] =~ $pat5) { # label
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

	$nextpc = &findaddr($opr1[$pc]);
	print "BA -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BZ/) {

	if ($zf > 0) {
	    $nextpc = &findaddr($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BLT/) {

	if ($nf > 0) {
	    $nextpc = &findaddr($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /BGT/) {

	if ($nf == 0 && $zf == 0) {
	    $nextpc = &findaddr($opr1[$pc]);
	} else {
	    $nextpc = $pc+1;
	}
	print "BZ -> nextpc = $nextpc\n";
	$pc = $nextpc;

    } elsif ($op[$pc] =~ /PUSH/) {

	$reg{$stack_ptr} -= 1;
	$mem{$reg{$stack_ptr}} = $reg{$opr1[$pc]};
	print "PUSH : $opr1[$pc] = ", $reg{$opr1[$pc]}, 
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";
	$pc += 1;

    } elsif ($op[$pc] =~ /POP/) {

	$reg{$opr1[$pc]} = $mem{$reg{$stack_ptr}};
	$reg{$stack_ptr} += 1;
	print "POP -> $opr1[$pc] = ", $reg{$opr1[$pc]},
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";
	$pc += 1;

    } elsif ($op[$pc] =~ /CALLR/) {

	# push pc+1
	$reg{$stack_ptr} -= 1;
	$mem{$reg{$stack_ptr}} = $pc+1;
	print "CALLR $opr1[$pc] -> RA = ", $pc+1,
	    " Addr = ", $reg{"$stack_ptr"},
	    " PC = $reg{$opr1[$pc]}\n";
	$pc = $reg{$opr1[$pc]};

    } elsif ($op[$pc] =~ /RET/) {

	# pop pc+1
	$pc = $mem{$reg{$stack_ptr}};
	$reg{$stack_ptr} += 1;
	print "RET -> RA = ", $pc,
	    " Addr = ", $reg{$stack_ptr},
	    " $stack_ptr = ", $reg{$stack_ptr}, "\n";

    } elsif ($op[$pc] =~ /OUT/) {

	print "OUT $opr1[$pc] ($opr1[$pc] = ", 
	    $reg{$opr1[$pc]}, ")\n";
	$pc++;

    } elsif ($op[$pc] =~ /END/) {
	last;

    } elsif ($op[$pc] =~ /EQU/) {
	$pc++;

    } elsif ($label[$pc] =~ /.+/) { # label
	print $label[$pc], ":\n";
	$pc++;
    } else {
	print "(???) <", $op[$pc], ">\n";
	$pc++;
    }       
}

sub findaddr {
    my ($arg) = @_;
    my ($i, $r);
    print "(findaddr) arg=$arg\n";
    for ($i = 0; $i < $count; $i++) {
	if ($label[$i] =~ /$arg/) {
	    if ($op[$i] =~ /EQU/) {
		if ($opr1[$i] =~ /^[A-Za-z][A-Za-z0-9]+$/) { # label
		    $r = &findaddr($opr1[$i]);
		} else { # (must be a) number
		    $r = &num2decimal($opr1[$i]);
		}
		return $r;
	    } else {
		return $i; # addr
	    }
	}
    }
    return -1;
}

sub num2decimal {
    my ($arg) = @_;
    my $r;
    if ($arg =~ /^-([0-9A-Fa-f]+)[Hh]$/) {
	$r = -hex($1);
    } elsif ($arg =~ /^([0-9A-Fa-f]+)[Hh]$/) {
	$r = hex($1);
    } else {
	$r = $arg;
    }
    print "(num2decimal) arg=$arg -> $r\n";
    return $r;
}
