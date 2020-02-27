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
			to_rows(Dicts,Tag,Rows), % Rows = list of pairs -> to rows ()
			match_header(Options,Tag,Rows,FancyHeader),!,
			debug(fancy_table, "use fancy table - header:~w, options:~w",[FancyHeader,Options]),
			header_keys(FancyHeader,Tag,SortKeys,Keys),
			reorder_values(Rows,Keys,OrderedRows),
			sort_rows(OrderedRows,SortKeys,SortedRows),
			delete(Options,header(_),NOptions)
	},
	fancy_table_rendering(FancyHeader,SortedRows,NOptions). 

to_rows([],_,[]).
to_rows([Dict|Dicts],Tag,[Row|Rows]) :-
	is_dict(Dict,Tag),
	dict_pairs(Dict,Tag,Pairs),
	Row =.. [Tag,Pairs],
	to_rows(Dicts,Tag,Rows).

%! match_header(+Options,+Tag,+Rows,-Header) is nondet
match_header(Options,Tag,Rows,Header) :-
	common_keys(Rows,CommonKeys),
	member(header(Header),Options),
	header_keys(Header,Tag,_,HeaderKeys),
	subset(HeaderKeys,CommonKeys).

% common_keys([row(a-3,c-1,b-2],row(b-2,a-2,c-2,d-0),row(a,b,c)])
common_keys([],[]) :- !.
common_keys([Row],Keys) :- Row =.. [_|Pairs],pairs_keys(Pairs,Keys).
common_keys([Row|Rows],CommonKeys) :-
	Row =.. [_|Pairs],
	pairs_keys(Pairs,Keys),
	common_keys(Rows,CKeys),
	intersection(Keys,CKeys,CommonKeys).

header_keys(Header,Tag,SortKeys,HeaderKeys) :-
	Header =.. [Tag|SortKeys],
	findall(HKey,(member(SortKey,SortKeys),SortKey =.. [HKey,_]),HeaderKeys).

% reorder_values([row(a-3,c-1,b-2),row(b-2,a-2,c-2),row(a,b,c)],Ordered)
reorder_values([],_,[]).
reorder_values([Row|Rows],Keys,[OrderedRow|OrderedRows]) :-
	Row =.. [_|Pairs],
	order_pairs(Pairs,Keys,OrderedPairs),
	OrderedRow =.. [_|OrderedPairs],
	reorder_values(Rows,Keys,OrderedRows).

order_pairs(Pairs,[],Pairs) :- !.
order_pairs(Pairs,[K|Ks],[K-V|OrderedPairs]) :-
	select(K-V,Pairs,RPairs),!,
	order_pairs(RPairs,Ks,OrderedPairs).

get_keys(Rows,Keys) :-
	get_keys(Rows,1,Keys).
get_keys(Rows,I,[]) :- forall(member(Row,Rows),(functor(Row,_,N), I > N)).
get_keys(Rows,I,[Key|Keys]) :-
	Rows = [FirstRow|_],
	arg(I,FirstRow,Key-_),
	forall(member(Row,Rows),arg(I,Row,Key-_)),
	J is I + 1,
	get_keys(Rows,J,Keys).


% sort_rows([row(a-3,b-2,c-1),row(a-2,b-2,c-2),row(a-2,b-1,c-2)],[a(@>=),b(@=<)],Sorted)
sort_rows(Rows,SortKeys,SortedRows) :-
	get_keys(Rows,Keys),
	sort_rows(Rows,Keys,SortKeys,SortedRows).

sort_rows(Rows,_,[],Rows) :- !.
sort_rows(Rows,Keys,[SortKey|SortKeys],SortedRows) :-
	SortKey =.. [Key,Order],
	nonvar(Order),!,
	nth1(I,Keys,Key),
	sort_rows(Rows,Keys,SortKeys,PartialSortedRows),
	debug(fancy_table,"i: ~w, key:~w, keys:~w, order:~w, rows:~w",[I,Key,Keys,Order,Rows]),
	sort([I,2],Order,PartialSortedRows,SortedRows).

sort_rows(Rows,Keys,[_|SortKeys],SortedRows) :-
	sort_rows(Rows,Keys,SortKeys,SortedRows).

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

