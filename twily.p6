# + int int -> int
# lambda (list-of-N sym) (a -> ?) -> fn-of-N
# let ((sym a) (sym b) (sym c) ...) d) -> d
# map (a -> b) (list a) -> (list b)

class Type {
    # list, atom?
    # [base type]
    # count (for lists-of-N)
    # return type (if base type = fn)
}

# +
# list
# ["int"] or ["int", "int"]
# 2 (or -1)
# "int"

# lambda
# list
# [Type { list, "sym", 0, nil }, fn]
# -1 (i.e just look at base type list)
# 

# let
# list
# [Type { list, Type { list, ["sym", "atom"], -1, nil }, 0 }, any]

class Typespec {
    has $.argtypes;
    has $.rettype;
}

class Atom {
    has $.val;
}

class Thing {
    has $.val is rw;
    has $.type is rw;
}

class Li {
    has Thing $.car is rw;
    has Li $.cdr is rw;
    has Bool $.has_cdr is rw;
}

sub compile() {
    my $x = 42;
    say $x;
}

sub recursify(@things) {
    my Li $li = Li.new();
    
    my $first = @things[0];
    #say $first.val;
    
    given $first.WHAT {
	when (Array) {
	    $li.car = Thing.new(val => recursify($first));
	}
	default {
	    $li.car = Thing.new(val => $first.val);
	}
    }

    my @g = @things[1..*];
    say "g elems = ", @g.elems;
    if @g.elems {
	$li.cdr = recursify(@g);
	$li.has_cdr = True;
    } else {
	$li.cdr = Nil;
	$li.has_cdr = False;
    }

    say $li;
    return $li;
}

sub parse2(Str $s, Int $pos, Int $depth, @things) {
    my Int $c;
    my Str $word = "";
    my Int $int_depth = $depth;
    my Bool $car_filled = False;
    loop ($c = $pos+1; $c <= $s.chars;) {
	given $s.split("")[$c] {
	    when $_ ~~ /\s/ {
		my Int $skip = 0;
		while $s.split("")[$c + ($skip++)] ~~ /\s/ {
		    say "whitespace";
		}
		$c += $skip-1;
		if $word.chars {
		    say "word is ", $word;
		    @things.push(Thing.new(val => Atom.new(val => $word)));
		}
		$word = "";
	    }
	    when ";" {
		my Int $skip = 0;
		while my $f = $c + ($skip++) and $s.split("")[$f] ~~ /\n/ {
		    say "comment", $s.split("")[$f];
		    if $f == $s.chars {
			return @things, $c;
		    }
		}
		# we just consumed the newline so skip one more
		$c += $skip;
	    }
	    when "(" {
		my @g = parse2($s, $c, $depth + 1, []);
		@things.push(@g[0]);
		$c = @g[1]+1;
		say "c is ", $c;
	    }
	    when ")" {
		#$word = $word ~ $_;
		if $word.chars {
		    @things.push(Thing.new(val => Atom.new(val => $word)));
		}
		if $int_depth == 0 {
		    die "ERROR (parse) - closing paren when not expected in $s at pos $c";
		}
		$int_depth -= 1;
		return @things, $c;
	    }
	    default {
		$word = $word ~ $_;
		if $c == $s.chars {
		    say "word is ", $word;
		    @things.push(Thing.new(val => Atom.new(val => $word)));
		}
		$c++;
	    }
	}
    }
    if $int_depth != 0 {
	say "depth is ", $int_depth;
	die "ERROR (parse) - $int_depth closing parens expected in $s at pos $c";
    }

    my @h = [];
    my Int $index = 0;
    while $index != @things.elems  {
	if @things[$index].WHAT === (Array) {
    	    @h.append(Thing.new(val => recursify(@things[$index])));
	} else {
    	    @h.append(@things[$index]);
	}
	$index++;
    }
    
    return @h, $c;
}

sub type-check-thing(Thing $th) {
    if $th.type.WHAT === (Any) {
	given $th.val.WHAT {
	    say "th.val is ", $th.val;
	    say "th.val.what is", $th.val.WHAT;

	    when (Li) {
		given type-check-thing($th.val.car) {
		    # expecting either a lambda or function name
		    when $_.type eq "builtin" {
			given $_.val.val {
			    when "+" {
				my $firstarg = $th.val.cdr;
				say "H", $firstarg;
				while $firstarg.has_cdr {
				    if type-check-thing($firstarg.car).type eq "int" {
					$firstarg =  $firstarg.cdr;
				    } else {
					die "ERROR (type-check) failed on ", $firstarg.car.val.val;
				    }
				}
				$th.type = "int";
			    }

			    when "lambda" {
				
			    }
			}
		    }
		}
	    }

	    when (Atom) {
		given $th.val.val {
		    when $_ ~~ /^\d+$/ {
			$th.type = "int";
		    }
		    when $_ ~~ /^\w+/ {
			$th.type = "sym";
		    }
		    when "+" {
			$th.type = "builtin";
		    }
		    when $_ ~~ /^\d+\.\d?/ {
			$th.type = "deci";
		    }
		    default {
			die "ERROR (type-check): couldn't infer type of ", $th.val, " (", $th.val.val, ")";
		    }
		}
	    }
	}
    }

    #say $th.val.type;
    return $th;
}

sub top-type-check(Thing @ths) {
    
}

sub eval-thing(Thing $th) {
    
}

sub top-eval(Thing @ths) {

}

my @q = parse2("(+ (+ 2 2) 1 (+ 1 2))", 0, 0, []);
say @q;
say @q[0][1];
say type-check-thing(@q[0][0]);

# Li.new(car => Thing.new(val => Atom.new(val => "+", type => Any)),
#        cdr => Li.new(
# 	   car => Thing.new(val => Atom.new(val => "1", type => Any)),
# 	   cdr => Li.new(
# 	       car => Thing.new(val => Atom.new(val => "2", type => Any)),
# 	       cdr => Li,
# 	       type => Any),
# 		     type => Any),
#        type => Any)
