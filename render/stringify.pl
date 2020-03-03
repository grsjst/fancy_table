:- module(stringify,
	  [ stringify_term/2			% +Term, -String
	  ]).

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

:- debug(stringify).


/** <module> Stringify

	Utility module common between the fancy_table and xlsx renderer
*/


%%	stringify_term(+Term:term, -String:string)
% 	dcg to produce a string from a term
stringify_term(Term, String) :-
   debug(stringify,"do atomize: ~p",[Term]),
   phrase(stringify_term(Term),Codes,[]),
   string_codes(String,Codes).

stringify_term(TextList) -->
	{is_list(TextList),forall(member(Str,TextList),is_text(Str)),!,atomics_to_string(TextList,',',TextStr)},
	TextStr.

stringify_term([]) --> !,"".
stringify_term([Term]) --> !,stringify_term(Term).
stringify_term([Term|Terms]) --> !,stringify_term(Term),",",stringify_term(Terms).  % [a,b,c] -> "a,b,c"

stringify_term(Date) --> 
	{Date = date(_,_,_),!,format_time(string(DateStr),"%d %b %Y",Date)},
	DateStr.

stringify_term(DateTime) --> 
	{DateTime = date(_,_,_,_,_,_,_,_,_),!,format_time(string(DateStr),"%FT%T%z",DateTime)}, % ISO8601
	DateStr.

stringify_term(Compound) --> 
	{compound(Compound),!,Compound =.. [Functor|Args],atom_string(Functor,FunctorStr)}, % a(x,y,z) -> "a - x,y,z"
	FunctorStr, " - ",stringify_term(Args).

stringify_term(Atomic) -->
	{atomic(Atomic),!,format(string(AtomicStr),"~w",[Atomic])},
	AtomicStr.

is_text(String) :- string(String),!.
is_text(Atom) :- atom(Atom),!.
