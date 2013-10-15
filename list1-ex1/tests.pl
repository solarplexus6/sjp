%%
%% Rafal Lukaszewski 221064
%% IMP language tests
%% 

%% include konieczne by miec dostep do wszystkich klauzul modulu - dla testow
%% rzuca warningami i bledami, ale testy dzialaja
:- include('imp-parser').
:- include('imp-interp').

:- begin_tests(parser).

test(atomic_numi) :- phrase(atomic(numi(1)), [tokNumber(1)]).
test(atomic_vari) :- phrase(atomic(vari(b)), [tokVar(b)]).

test(summand_single) :- phrase(summand(numi(1)), [tokNumber(1)]).
test(summand_op) :- phrase(summand(numi(1)*numi(2)), [tokNumber(1), tokTimes, tokNumber(2)]).

test(disjunct_and) :- 
    phrase( disjunct(numi(1)=numi(2) && <=(vari(b), vari(c))), 
            [tokNumber(1), tokEq, tokNumber(2), tokAnd, tokVar(b), tokLeq, tokVar(c)]).
test(skip) :- 
    phrase(program(skip),[tokSkip]).

test(program_if) :- phrase(program(if(numi(10)=numi(12), skip, skip)),
                           [tokIf, tokNumber(10), tokEq, tokNumber(12),tokThen, tokSkip, tokElse, tokSkip, tokEnd]).

test(program_assign) :- phrase(program((assign(vari(a), numi(1))) ),
                               [tokVar(a), tokAssign, tokNumber(1)]).
test(program_assign_mult) :- phrase(program(assign(vari(b) , vari(b) '*' vari(a))),
                                    [tokVar(b) , tokAssign, tokVar(b), tokTimes, tokVar(a)]).
test(program_assign_minus) :- phrase(program( assign(vari(b) ,vari(b) '-' vari(a))),
                                    [tokVar(b) , tokAssign, tokVar(b), tokMinus, tokVar(a)]).

test(program_while) :- phrase(program(while( <=( vari(a) ,numi(10)), assign(vari(a) , vari(a) '+' numi(1))) ), 
                              [tokWhile, tokVar(a), tokLeq, tokNumber(10), tokDo, tokVar(a), tokAssign, tokVar(a), tokPlus, tokNumber(1), tokEnd]).

test(program_colon) :- phrase(program(skip ; skip), [tokSkip, tokColon, tokSkip]).
test(program_multi_colon) :- phrase(program( assign(vari(x), numi(1)) ; assign(vari(x), numi(1)) ; assign(vari(x), numi(1) '+' numi(2))), 
                                   [tokVar(x), tokAssign, tokNumber(1), tokColon, tokVar(x), tokAssign, tokNumber(1), tokColon, tokVar(x), tokAssign, tokNumber(1), tokPlus, tokNumber(2)]).

test(program_complex_while) :- phrase(program(while( vari(b) '=' numi(2), assign(vari(a), vari(a) '+' numi(1)) ; assign(vari(b), numi(1)) ; assign( vari(a), vari(a) '*' numi(2))) ), 
                                      [tokWhile, tokVar(b), tokEq, tokNumber(2), tokDo, 
                                          tokVar(a), tokAssign, tokVar(a), tokPlus, tokNumber(1), tokColon, 
                                          tokVar(b), tokAssign, tokNumber(1), tokColon,
                                          tokVar(a), tokAssign, tokVar(a), tokTimes, tokNumber(2), 
                                        tokEnd]).

test(program_factorial) :- phrase(program(assign(vari(a), numi(10)) ; assign(vari(b), numi(1)) ; while(<=( numi(1), vari(a)), assign(vari(b), vari(b) '*' vari(a)) ; assign(vari(a), vari(a) '-' numi(1))) ),
                                  [tokVar(a), tokAssign, tokNumber(10), tokColon, 
                                   tokVar(b), tokAssign, tokNumber(1), tokColon, 
                                   tokWhile, tokNumber(1), tokLeq, tokVar(a), tokDo, 
                                            tokVar(b), tokAssign, tokVar(b), tokTimes, tokVar(a), tokColon, 
                                            tokVar(a), tokAssign, tokVar(a), tokMinus, tokNumber(1), 
                                    tokEnd]).

%% test(if) :-
%%     phrase(program(if()))

test(lexer_and) :- phrase(lexer([tokNumber(1), tokEq, tokNumber(2), tokAnd, tokVar(b), tokLeq, tokVar(c)]),
                        "1 = 2 && b <= c").
test(lexer_factorial) :- phrase(lexer([tokVar(a), tokAssign, tokNumber(10), tokColon, 
                                       tokVar(b), tokAssign, tokNumber(1), tokColon, 
                                       tokWhile, tokNumber(1), tokLeq, tokVar(a), tokDo, 
                                          tokVar(b), tokAssign, tokVar(b), tokTimes, tokVar(a), tokColon, 
                                          tokVar(a), tokAssign, tokVar(a), tokMinus, tokNumber(1), 
                                        tokEnd]),
                           "a:= 10; b := 1; while 1 <= a do b := b * a; a:= a - 1 end").

test(parser_if) :- parse("if 2 <= 3 then a:= 25 else skip end", if(numi(2)<=numi(3), assign(vari(a), numi(25)), skip)).
test(parser_if_and) :- parse("if 2 <= 3 && 10 = 10 then skip else skip end", if(numi(2)<=numi(3) && numi(10) = numi(10), skip, skip)).
test(parser_true) :- parse("if true then skip else skip end", if(bool(t), skip, skip)).
test(parser_false) :- parse("if false then skip else skip end", if(bool(f), skip, skip)).
test(parser_not) :- parse("if not true then skip else skip end", if(not(bool(t)), skip, skip)).
test(parser_not) :- parse("if not 2 <= 1 then skip else skip end", if(not(numi(2)<=numi(1)),skip, skip)).

test(parser_factorial) :- parse("a:= 10; b := 1; while 1 <= a do b := b * a; a:= a - 1 end", 
                                assign(vari(a), numi(10)) ; assign(vari(b), numi(1)) ; while(<=( numi(1), vari(a)), assign(vari(b), vari(b) '*' vari(a)) ; assign(vari(a), vari(a) '-' numi(1))) ).

:- end_tests(parser).

:- begin_tests(intepreter).

test(reduce_arith) :- reduce(numi(5) '+' numi(4), [], numi(9)).

test(eval_bool_and_false) :- eval(bool(t) '&&' bool(f), [], bool(f)).
test(eval_bool_and_true) :- eval(bool(t) '&&' bool(t), [], bool(t)).
test(eval_bool_or_false) :- eval(bool(f) '||' bool(f), [], bool(f)).
test(eval_bool_or_true1) :- eval(bool(t) '||' bool(f), [], bool(t)).
test(eval_bool_or_true2) :- eval(bool(f) '||' bool(t), [], bool(t)).
test(eval_bool_or_true3) :- eval(bool(t) '||' bool(t), [], bool(t)).

test(interp_assign) :- interp("a:= 10", [(vari(a), numi(10))]).
test(interp_assign_eval) :- interp("a:= 10 + 15", [(vari(a), numi(25))]).
test(interp_assign_vari) :- interp("a := 20; b := a * 2", [(vari(a), numi(20)), (vari(b), numi(40))]).

test(interp_if) :- interp("if 2 <= 3 then a:= 25 else skip end", [(vari(a), numi(25))]).
test(interp_if_and_true) :- interp("if 2 <= 3 && 10 = 10 then a:= 25 else skip end", [(vari(a), numi(25))]).
test(interp_if_and_false) :- interp("if 4 <= 4 - 1 && 10 = 10 then skip else a := 25 end", [(vari(a), numi(25))]).
test(interp_if_or_true) :- interp("if 2 <= 3 || 9 = 10 then a:= 25 else skip end", [(vari(a), numi(25))]).
test(interp_if_or_false) :- interp("if 4 <= 3 || 10 = 10 && 2 = 3 then skip else a := 25 end", [(vari(a), numi(25))]).
test(interp_if_bool_true) :- interp("if true then a:= 25 else skip end", [(vari(a), numi(25))]).
test(interp_if_bool_false) :- interp("if false then skip else a := 25 end", [(vari(a), numi(25))]).
test(interp_if_bool_complex_true) :- interp("if true || false && true then a:= 25 else skip end", [(vari(a), numi(25))]).
test(interp_if_bool_complex_false) :- interp("if true && false then skip else a := 25 end", [(vari(a), numi(25))]).
test(interp_if_not_true) :- interp("if not true then skip else a := 1 end", [(vari(a), numi(1))]).
test(interp_if_not_rel) :- interp("if not 2 <= 1 then a:= 1 else skip end", [(vari(a), numi(1))]).
test(interp_if_not_complex) :- interp("if not 2 <= 1 && not 2 = 1 || not 3 = 4 then a:= 1 else skip end", [(vari(a), numi(1))]).

test(interp_colon) :- interp("a := 12; skip; b := 2 * 3; skip", [(vari(a), numi(12)), (vari(b), numi(6))]).

test(interp_while) :- interp("a := 20; while a <= 100 do a:= a * 2 end", [(vari(a), numi(160))]).
test(interp_while_complex) :- interp("a := 20; while a <= 100 do a:= a * 2; b := 0 - a end", [(vari(a), numi(160)), (vari(b), numi(-160))]).

test(interp_factorial) :- interp("a:= 10; fac := 1; while not a <= 0 do fac := fac * a; a := a - 1 end", [(vari(a), numi(0)), (vari(fac), numi(3628800))]).
test(interp_gcd) :- interp("a := 1071; b := 462; while not a = b do if not a <= b then a := a - b else b := b - a end end", [(vari(a), numi(21)), (vari(b), numi(21))]).

%% test unknown_identifier
test(iterp_unknown_id, [throws(unknown_identifier(_))]) :- interp("a := b", []).

:- end_tests(intepreter).