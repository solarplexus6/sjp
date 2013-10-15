%%
%% Rafal Lukaszewski 221064
%% IMP language interpreter
%% 

:- module('imp-interp', [interp/1, interp/2, run/1]).
:- use_module('imp-parser').

%% aktualizacja stanu pamieci
update([], X, V, [(X,V)]).
update([(X,_)|T], X, V, [(X,V)|T]) :- !.
update([H|T], X, V, [H|S]) :-
    update(T, X, V, S).

:- op(820, xfy, '&&').
:- op(840, xfy, '||').
:- op(700, xfy, '<=').
:- op(700, xfy, '=').

%% Atomic commands
reduce( skip, Env, Env).

reduce( assign(vari(V), Aexp), Env, NewEnv) :-
        reduce(Aexp, Env, AexpVal), !,
        update(Env, vari(V), AexpVal, NewEnv).

%% Sequencing
reduce( Expr1 ';' Expr2, Env, NewEnv) :-
        reduce( Expr1, Env, Env1),!,
        reduce( Expr2, Env1, NewEnv).

%% Conditionals

reduce( if( If, Then,Else), Env, Result) :-
        reduce(If, Env, IfRes),!,
        ( IfRes = bool(t), reduce(Then, Env, Result)
        ; IfRes = bool(f), reduce(Else, Env, Result)).

%% While-loops

reduce( while(While, Do), Env, NewEnv)  :-
        reduce(While, Env, WhileRes), !,
        ( WhileRes = bool(t), 
            reduce(Do, Env, TransEnv), reduce(while(While, Do), TransEnv, NewEnv)
        ; WhileRes = bool(f), NewEnv = Env
        ).

reduce( vari(V), Env, Val):-
         (memberchk( (vari(V), Val), Env),! ; throw(unknown_identifier(V))).

reduce( Expr, Env, Result ) :-
      Expr =.. [Op,Expr1,Expr2],
      memberchk(Op, ['+', '-', '*', '||', '&&', '<=','=']),!,
      reduce( Expr1, Env, Res1),!,
      reduce( Expr2, Env, Res2),!,
      NewExpr =.. [Op, Res1,Res2],
      eval(NewExpr,Env, Result), !.

%% not
reduce(not(Bexp), Env, bool(Result)) :-
       reduce(Bexp, Env, bool(BexpRes)),
       ( BexpRes = t, Result = f
       ; BexpRes = f, Result = t).

reduce(numi(V), _ , numi(V)) :- !.
reduce(bool(t), _, bool(t)) :- !.
reduce(bool(f), _, bool(f)) :- !.

%% Arithmetic
eval( Aexp, _, numi(Result) ):-
      Aexp =.. [Op,numi(A1),numi(A2)],
      ( Op = '+', Result is A1+A2
      ; Op = '-', Result is A1-A2
      ; Op = '*', Result is A1*A2
      ), !.

%% relational
eval( Bexp, _, bool(Result) ):-
      Bexp =.. [Op,numi(A1),numi(A2)],
      ( Op = '<=', (A1 =< A2 -> Result = t ; Result = f)
      ; Op = '=', (A1 =:= A2 -> Result = t; Result = f)), !.

%% boolean
eval( Bexp, _, bool(Result) ):-
      Bexp =.. [Op,bool(B1),bool(B2)],
      ( Op = '&&', ((B1 = t, B2 = t) -> Result = t; Result = f)
      ; Op = '||', ((B1 = t; B2 = t) -> Result = t; Result = f)), !.

error( unknown_identifier(V)) :-
        string_to_atom([V],Char), writef("\nParse: unknown identifier %w.\n",[Char]).

interp( CodeList ):-        
        interp(CodeList, Result),
        write(Result).

interp( CodeList, Result):-
        parse(CodeList, Tree),
        reduce(Tree, [] , Result), !.

run(Prog):-
        catch( interp( Prog ), Error, error(Error)).

runFile(File):-
       read_file_to_codes(File,CodeList,[]),
       run(CodeList).        