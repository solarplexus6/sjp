%%
%% Rafal Lukaszewski 221064
%% IMP language parser
%% 

:- module(imp_parser, [parse/2]).

% Lexer
lexer(Tokens) -->
   white_space,
   (  (  ";",       !, { Token = tokColon }      
      ;  "+",       !, { Token = tokPlus }    
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }            
      ;  "<=",      !, { Token = tokLeq }
      ;  "=",       !, { Token = tokEq }
      ;  "||",      !, { Token = tokOr }
      ;  "&&",      !, { Token = tokAnd }
      ; ":=",       !, { Token  = tokAssign}
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [              
                                     (if, tokIf),
                                     (then, tokThen),
                                     (else, tokElse),
                                     (while, tokWhile),
                                     (do, tokDo),
                                     (skip, tokSkip),
                                     (true, tokTrue),
                                     (false, tokFalse),
                                     (end, tokEnd),
                                     (not, tokNot)]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [Un],
            { Token = tokUnknown, throw((unrecognized_token, Un)) }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].

program(Astree) -->
   stms(Astree).

stms(Astree) -->
   (  
      stm(Stm), [tokColon], !, stms(Rest),
         { Astree = (Stm ';' Rest) }
    ; stm(Astree)
   ).

stm(Astree) -->
   (  [tokVar(Id)], [tokAssign], !, arith_expr(Aexpr),
                { Astree = assign(vari(Id), Aexpr) }
    ; [tokIf], !, simple(If), [tokThen], stms(Then), [tokElse], stms(Else), [tokEnd],
                { Astree = if(If,Then,Else) }    
    ; [tokWhile], !, simple(While), [tokDo], stms(Do), [tokEnd],
                { Astree = while(While,Do) }
    ; [tokSkip], !,
                { Astree = skip }    
   ).

simple(Expr) -->
   disjunct(Dis), simple(Dis, Expr).
         
simple(Acc, Bool) -->
   [tokOr], !, disjunct(Disjunct),
      { Acc1 =.. ['||', Acc, Disjunct] }, simple(Acc1, Bool).
simple(Acc, Acc) -->
   [].

disjunct(Disjunct) -->
   conjunct(Conjunct), disjunct(Conjunct, Disjunct).

disjunct(Acc, Disjunct) -->
   [tokAnd], !, conjunct(Conjunct),
      { Acc1 =.. ['&&', Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) -->
   [].

conjunct(Con) -->
  ( no_rel(Expr), conjunct(Expr, Con), !
  ; [tokNot], !, conjunct(Con1),
      { Con = not(Con1)}
  ; [tokTrue], !,
      { Con = bool(t)}
  ; [tokFalse], !,
      { Con = bool(f)}
  ).

conjunct(Acc, Con) -->
  ( rel_op(Op), !, no_rel(Expr),
      { Acc1 =.. [Op, Acc, Expr] }, conjunct(Acc1, Con)
  ).
conjunct(Acc, Acc) -->
   [].

no_rel(Expr) -->
   arith_expr(Expr1), no_rel(Expr1,Expr).
no_rel(Acc,Acc) -->
   [].

arith_expr(Expr) -->
   summand(Summand), arith_expr(Summand, Expr).

arith_expr(Acc, Expr) -->
   add_op(Op), !, summand(Summand),
      { Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

summand(Expr) -->
   atomic(E), summand(E, Expr).

summand(Acc, Expr) -->
   mult_op(Op), !, atomic(E),
      { Acc1 =.. [Op, Acc, E] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

atomic(Atom) -->
    ( [tokVar(Var)], !,
                { Atom = vari(Var)}
    ; [tokNumber(N)], !,
                { Atom = numi(N) }
    ).

rel_op('<=') --> [tokLeq],!.
rel_op('=')  --> [tokEq].

add_op('+') --> [tokPlus], !.
add_op('-') --> [tokMinus].

mult_op('*') -->   [tokTimes].

parse(CodeList,SyntaxTree):-
   phrase(lexer(TokList), CodeList),
   phrase(program(SyntaxTree), TokList),!.