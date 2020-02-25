:- module(swish_render_fancy_table,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).
:- use_module(library(csv)).
:- use_module(swish(lib/render)).
:- use_module(swish(lib/render/table),[term_rendering/5 as table_term_rendering]).
:- use_module(library(debug)).

:- debug(fancy_table).
:- debug(fancy_table,"Fancy table renderer loaded",[]).

:- register_renderer(fancy_table, "Fancy options for rendering tables").
:- use_module(swish(lib/storage)).


% :- use_gitty_file(test_fancy_term). % this comes from SWISH


/** <module> SWISH fancy table renderer
Includes fancy options for rendering tables using the standard table renderer

Options include:
	- multuple key sorting
	- special formatting for specific values
	- download as xls 

*/


%%	term_rendering(+Rows:list, +Vars:list, +Options:list)//
%
%	Renders Rows as a fancy_table.
% 	Supported formats
%   Rows = [[2,2,1],[1,2,3]]. 
%   Rows = [[population(1),name("aaa"),country("fr")]] 
%   Rows = [[population-1,name - "aaa",country - "fr"]]
%   Rows = [[population=1,name="aaa",country="fr"]]
%   Rows = [a(2,2,1),a(1,2,3)]
%   Rows = [captial{populaton:1,name:"aa",country:"fr"}]
%   NRows = [capital(population-1,name-"aaa",country-"fr"),]
term_rendering(Term, _Vars, Options) -->
	{   
		debug(fancy_table, "try fancy_table with header ~p",[Term]),
		normalise_rows(Term,Rows),
		debug(fancy_table, "normalised ~w",[Rows]),
		match_header(Options,Rows,Header),!,
		debug(fancy_table, "use fancy table - header:~w, options:~w",[Header,Options]),
		forall(member(fancy_file(GittyFile),Options),use_gitty_file(GittyFile)),
		Header =.. [_|SortKeys],
		sort_rows(Rows,SortKeys,SortedRows),
		debug(fancy_table,"sorted rows:~p",[SortedRows]),
		qualify_rows(SortedRows,QRows)
	},
	fancy_table_rendering(Header,QRows,Options).

term_rendering(Term, _Vars, Options) -->
	{   
		debug(fancy_table, "try fancy_table without header",[]),
		normalise_rows(Term,Rows),
		qualify_rows(Rows,QRows),
		debug(fancy_table,"~p, ~w ",[Rows,QRows])
	},
	fancy_table_rendering(QRows,Options).

qualify_rows([],[]).
qualify_rows([Row|Rows],[QRow|QRows]) :-
	Row =.. [Tag|Pairs],
	debug(fancy_table,"oairs:~w",[Pairs]),
	findall(QValue,(member(K-V,Pairs),QValue =.. [K,V]),QValues),
	QRow =..[Tag|QValues],
	qualify_rows(Rows,QRows).

normalise_rows([],[]).
normalise_rows([Row|Rows],[NRow|NRows]) :-
	normalise_row(Row,NRow),
	normalise_rows(Rows,NRows).

normalise_row(Dict,NRow) :-
	is_dict(Dict),!,
	dict_pairs(Dict,Tag,Pairs),
	(var(Tag) -> NTag = row ; NTag = Tag), 
	NRow =.. [NTag|Pairs].

normalise_row(Row,NRow) :-
	qpairs(Row,NRow).

qpairs(Row,NRow) :-
	\+ is_list(Row),
	compound(Row),!,
	Row =.. [T|Pairs],
	pairs(Pairs,NPairs),
	NRow =.. [T|NPairs].

qpairs(Pairs,row(NPairs)) :-
	is_list(Pairs),
	pairs(Pairs,NPairs). 

pairs([],[]).
pairs([Pair|Pairs],[NPair|NPairs]) :-
	pair(Pair,NPair),
	pairs(Pairs,NPairs).

pair(K-V,K-V) :- !. 
pair(K=V,K-V) :- !. 
pair(K:V,K-V) :- !. 
pair(Pair,K-V) :-
	compound(Pair),
	Pair =.. [K,V],!.
pair(V,value-V). 

%! match_header(+Options,+Rows,-Header) is nondet
match_header(Options,Rows,Header) :-
	common_keys(Rows,CommonKeys),
	member(header(Header),Options),
	header_keys(Header,Tag,HeaderKeys),
	subset(HeaderKeys,CommonKeys),
	forall(member(Row,Rows), Row =.. [Tag|_]),!.

match_header(Options,Rows,Header) :-
	member(header(Header),Options),
	header_keys(Header,Tag,HeaderKeys),
	length(HeaderKeys,N),
	forall(member(Row,Rows), (Row =.. [Tag|Values],length(Values,N))),!.



common_keys([],[]).
common_keys([Row],Keys) :-
	Row =.. [_|Pairs],
	pairs_keys(Pairs,Keys).
common_keys([Row|Rows],CommonKeys) :-
	Row =.. [_|Pairs],
	pairs_keys(Pairs,Keys),
	common_keys(Rows,CKeys),
	intersection(Keys,CKeys,CommonKeys).

header_keys(Header,Tag,HeaderKeys) :-
	Header =.. [Tag|SortKeys],
	findall(HKey,(member(SortKey,SortKeys),SortKey =.. [HKey,_]),HeaderKeys).


% sort_rows([row(2,1,3),row(1,1,1),row(1,2,2)],[a(@=<),b(@>=),c(_)],Sorted).
sort_rows(Rows,SortKeys,Sorted) :-
	sort_rows(Rows,0,SortKeys,Sorted).

sort_rows(Rows,N,SortKeys,Rows) :- length(SortKeys,N),!.
sort_rows(Rows,I,SortKeys,Sorted) :-
	nth1(I,SortKeys,SortKey),
	SortKey =.. [_Key,Order],
	nonvar(Order),!,
	J is I + 1,
	sort_rows(Rows,J,SortKeys,TmpSorted),
	sort(I,Order,TmpSorted,Sorted).

sort_rows(Rows,I,SortKeys,Sorted) :-
	J is I + 1,
	sort_rows(Rows,J,SortKeys,Sorted).	


fancy_table_rendering(Header,Rows,Options) -->
	{
		header_keys(Header,Tag,Keys),
		NHeader =.. [Tag|Keys]
		% debug(fancy_table, "Tag: ~w, Keys:~w, Rows:~w, Options:~w, Header:~w",[Tag, Keys, Rows, Options,Header])
	},
	html(div([class(['export-dom']), style('display:inline-block'),
		   'data-render'('Fancy Table with Header')
		 ],
		 [  
		 	\table_term_rendering(Rows,_,[header(NHeader)|Options]),
		 	\fancy_table_footer(Rows)		 	
		 ])).

fancy_table_rendering(Rows,Options) -->
	{
		% debug(fancy_table, "Tag: ~w, Keys:~w, Rows:~w, Options:~w, Header:~w",[Tag, Keys, Rows, Options,Header])
	},
	html(div([class(['export-dom']), style('display:inline-block'),
		   'data-render'('Fancy Table without Header')
		 ],
		 [  
		 	\table_term_rendering(Rows,_,Options),
		 	\fancy_table_footer(Rows)		 	
		 ])).


fancy_table_footer([]) --> html(p("0 records")).
fancy_table_footer(Rows) -->
	{
		length(Rows,N),
		format(string(FooterMesg),"~w records",[N]),
		to_csv(Rows,CSV,[]),
		Rows = [R|_], R =.. Tag, 
		format(string(DataURI),"data:~w,~w",["text/csv",CSV]),
		format(string(FName),"~w.csv",[Tag])

	},
	html([p([FooterMesg," ",a([href(DataURI),download(FName)],"(export)")])]).

to_csv(Rows,CSV, Options) :-
	setup_call_cleanup(
		new_memory_file(Handle),
		(setup_call_cleanup(
			open_memory_file(Handle, write, Out),
			forall(member(Row,Rows), (atomize_row(Row,ARow),csv_write_stream(Out, [ARow], Options))),
			close(Out)),
		memory_file_to_string(Handle, CSV)),
		free_memory_file(Handle)).
	

% atomize rows so they can be exported to CSV
atomize_row(Row,ARow) :-
	Row =.. [Tag|Values],
	findall(AValue,
		(member(Value,Values), (atomic(Value) -> AValue = Value ; format(atom(AValue),"~w",[Value]))),
		AValues),
	ARow =.. [Tag|AValues].


%%	fancy_term(Term, +Options)
%
%	Specific renders for values in the table
%	
%	supported;
% 	we renders dates as eg "16 Aug 2018"
%	wiki_id include a clcikable link 
%  	human friendly format for dates

:- multifile swish_render_fancy_table:fancy_term/5.

user_or_default_fancy_term(ColName,Cell,Options) -->  {debug(fancy_table,"col:~w, cell:~w, ops: ~w",[ColName,Cell,Options])},fancy_term(ColName,Cell,Options).
user_or_default_fancy_term(ColName,Cell,Options) --> def_fancy_term(ColName,Cell,Options).

def_fancy_term(_,Date, Options) -->
	{   
		Date = date(_,_,_),!,
		format_time(string(DateStr),"%d %b %Y",Date)
	},
	html(td(\term(DateStr, Options))).

% use label for IRIs if defined
def_fancy_term(_,IRI, Options) -->
	{ 
		atom(IRI),
		rdf_iri(IRI),
		rdfs_label(IRI,Label), 
		debug(fancy_table,"iri: ~w, label:~w", [IRI,Label])
	},
	html(td(\term(Label, Options))).

% include a link for uri
def_fancy_term(_,URI, _Options) -->
	{ 
	atom(URI),uri_is_global(URI) 
	},
	html(td(a(href(URI),URI))).

def_fancy_term(ColName,value(Term), Options) --> def_fancy_term(ColName,Term, Options).

% default
def_fancy_term(ColName,Term, Options) -->
	{ 
		debug(fancy_table,"use default for ColName: ~w, Term:~w", [ColName,Term])
	},
	html(td(\term(Term, Options))).


% create a hook in swish_render_table to render specific terms
:- abolish(swish_render_table:row/4).
swish_render_table:row(Row, Options) --> {debug(fancy_table,"using fancy_row: ~w",[Row])},fancy_row(Row,Options).


fancy_row([],_) --> [].
fancy_row([QCell|Cells], Options) -->
	{
		debug(fancy_table,"fancy_row: ~w", [QCell]),
		QCell =.. [ColName,Cell]
	},
	user_or_default_fancy_term(ColName,Cell,Options),
	fancy_row(Cells, Options).

