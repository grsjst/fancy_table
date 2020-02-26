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
term_rendering(Dicts, _Vars, Options) -->
	{
			% test for all dicts
			to_pairs_list(Dicts,Tag,PairsList), % Rows = list of pairs
			match_header(Options,Tag,PairsList,FancyHeader),!,
			debug(fancy_table, "use fancy table - header:~w, options:~w",[FancyHeader,Options]),
			header_keys(FancyHeader,Tag,SortKeys,Keys),
			order_pairs_list(PairsList,Keys,OrderedPairsList),
			sort_pairs_list(OrderedPairsList,SortKeys,SortedPairsList),
			delete(Options,header(_),NOptions)
	},
	fancy_table_rendering(FancyHeader,SortedPairsList,NOptions). 

to_pairs_list([],_,[]).
to_pairs_list([Dict|Dicts],Tag, [Pairs|PairsList]) :-
	dict_pairs(Dict,Tag,Pairs),
	to_pairs_list(Dicts,Tag,PairsList).

%! match_header(+Options,+Tag,+PairsList,-Header) is nondet
match_header(Options,Tag,PairsList,Header) :-
	common_keys(PairsList,CommonKeys),
	member(header(Header),Options),
	header_keys(Header,Tag,_,HeaderKeys),
	subset(HeaderKeys,CommonKeys).

% common_keys([a-3,c-1,b-2],[b-2,a-2,c-2,d-0]],[a,b,c])
common_keys([],[]) :- !.
common_keys([Pairs],Keys) :- pairs_keys(Pairs,Keys).
common_keys([Pairs|PairsList],CommonKeys) :-
	pairs_keys(Pairs,Keys),
	common_keys(PairsList,CKeys),
	intersection(Keys,CKeys,CommonKeys).

header_keys(Header,Tag,SortKeys,HeaderKeys) :-
	Header =.. [Tag|SortKeys],
	findall(HKey,(member(SortKey,SortKeys),SortKey =.. [HKey,_]),HeaderKeys).

% order_pairs_list([[a-3,c-1,b-2],[b-2,a-2,c-2]],[a,b,c],Ordered)
order_pairs_list([],_,[]) .
order_pairs_list([Pairs|PairsList],Keys,[OrderedPairs|OrderedPairsList]) :-
	order_pairs(Pairs,Keys,OrderedPairs),
	order_pairs_list(PairsList,Keys,OrderedPairsList).

order_pairs(Pairs,[],Pairs) :- !.
order_pairs(Pairs,[K|Ks],[K-V|OrderedPairs]) :-
	select(K-V,Pairs,RPairs),!,
	order_pairs(RPairs,Ks,OrderedPairs).


% sort_pairs_list([[a-3,b-2,c-1],[a-2,b-2,c-2]],[a(@>=),b(@=<)],Sorted)
sort_pl(PairsList,[],PairsList) :- !.
sort_pl(PairsList,[SortKey|SortKeys],SortedPairsList) :-
	SortKey =.. [Key,Order],
	nonvar(Order),!,
	nth1(1,Pairs,PairsList), 
	nth1(I,Pairs,Key-_),forall(member(Pairs,PairsList),nth1(I,Pairs,Key-_)), % to ensure we sort Key 
	sort_pl(PairsList,SortKeys,PartialSortedPairsList),
	sort([I,2],Order,PartialSortedPairsList,SortedPairsList).

sort_pl(PairsList,[_|SortKeys],SortedPairsList) :-
	sort_pl(PairsList,SortKeys,SortedPairsList).


% sort_pairs_list([[a-3,b-2,c-1],[a-2,b-2,c-2]],[a(@>=),b(@=<)],Sorted)
sort_pairs_list(PairsList,SortKeys,PairsList) :-
	sort_pairs_list(PairsList,0,SortKeys,PairsList).

sort_pairs_list(PairsList,N,SortKeys,PairsList) :- length(SortKeys,N),!.
sort_pairs_list(PairsList,I,SortKeys,SortedPairsList) :-
	nth1(I,SortKeys,SortKey),
	SortKey =.. [_Key,Order],
	nonvar(Order),!,
	J is I + 1,
	sort_pairs_list(PairsList,J,SortKeys,PartialSortedPairsList),
	sort([I,2],Order,PartialSortedPairsList,SortedPairsList).
	
sort_pairs_list(PairsList,I,SortKeys,SortedPairsList) :-
	J is I + 1,
	sort_pairs_list(PairsList,J,SortKeys,SortedPairsList).

fancy_table_rendering(Header,Rows,Options) -->
	{
		header_keys(Header,Tag,_,Keys),
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
fancy_row([ColName-Cell|Cells], Options) -->
	{
		debug(fancy_table,"fancy_row: colname: ~w, value:~w", [ColName,Cell])
	},
	user_or_default_fancy_term(ColName,Cell,Options),
	fancy_row(Cells, Options).

