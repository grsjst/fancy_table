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


/** <module> SWISH fancy table renderer
Includes more fancy options for rendering tables of dicts than the standard table renderer

Options include:

sorting, download csv
*/

/*
table_spec([
       		_{tag:capital, keys:[name, country],sort_keys:[country], order:ascending}
           ]),
     export(csv),
     count(true)
*/


%%	term_rendering(+Dicts:list, +Vars:list, +Options:list)//
%
%	Renders  Dicts as a fancy_table.

term_rendering([], _Vars, _Options) -->
	!,
	html(div([ style('display:inline-block'),
		   'data-render'('Fancy Table')
		 ],
		 [  
		 	\fancy_table_footer([])
		 ])).

term_rendering(Dicts, Vars, Options) -->
	{   
		% debug(fancy_table, "Dicts: ~w, Vars:~w, Options:~w",[Dicts,Vars,Options]),
		is_list_of_dicts(Dicts,Tag,DictKeys),
		debug(fancy_table, "Tag: ~w, AllKeys:~w, Options:~w",[Tag, DictKeys,Options]),

		% select the renderer table_spec to use		
		select(table_spec(Specs),Options, NOptions),
		member(Spec,Specs),
		Spec =.. [Tag,SortKeys],
		findall(Key,(member(TmpKey,SortKeys),(TmpKey =.. [Key,_] -> true ; Key = TmpKey)),Keys),
		subset(Keys,DictKeys),!,
		
		sort_dicts(Dicts,SortKeys,SortedDicts),
		dicts_to_rows(SortedDicts,Keys,Rows)
	},
	fancy_table_rendering(Tag,Keys,Rows,Vars,NOptions).

term_rendering(Dicts, Vars, Options) -->
	{
		is_list_of_dicts(Dicts,Tag,Keys),
		dicts_to_rows(Dicts,Keys,Rows)
	},
	fancy_table_rendering(Tag,Keys,Rows,Vars,Options).

fancy_table_rendering(Tag,Keys,Rows,Vars,Options) -->
	{
		debug(fancy_table, "Tag: ~w, Keys:~w, Rows:~w",[Tag, Keys, Rows]),
		Header =.. [Tag|Keys]
	},
	html(div([class(['export-dom']), style('display:inline-block'),
		   'data-render'('Fancy Table')
		 ],
		 [  
		 	\table_term_rendering(Rows,Vars,[keys(Keys),header(Header)|Options]),
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
		format(string(FName),"~w.csv",Tag)

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
% 	Term is of the form key(value) (eg 'wiki_id("Projet H2020 341"))'') 
%	
%	supported;
% 	we renders dates as eg "16 Aug 2018"
%	wiki_id include a clcikable link 

% human readible format for dates
fancy_term(Term, Options) -->
	{   
		%debug(fancy_table, "fancy_term: Term: ~w",[Term]),
		Term =.. [_,Date],
		Date = date(_,_,_),!,
		format_time(string(DateStr),"%d %b %Y",Date)
	},
	html(td(\term(DateStr, Options))).

% include a link for wiki_id
fancy_term(wiki_id(WikiId), _Options) -->
	{   
		format(atom(URL),"https://wiki.inria.fr/dpe/~w",[WikiId])
	},
	html(td(a(href(URL),WikiId))).

% include a link for uri
fancy_term(uri(URI), _Options) -->
	{   
	},
	html(td(a(href(URI),URI))).

% use label for IRIs if defined
fancy_term(property(IRI), Options) -->
	{ 
		rdf_global_id(IRI,URI),
		debug(fancy_table,"iri: ~w, uri:~w", [IRI,URI]),
		rdfs_label(URI,Label)  
	},
	html(td(\term(Label, Options))).

%%	sort_dicts(+In:list, +Keys:list, +Sorted:list)//
%
%	Sorts a list of dicts on multiple keys
%   keys are of the format:  key(=<), or key
%
%   eg sort_dicts([_{a:2,b:1,c:3},_{a:1,b:1,c:1},_{a:1,b:2,c:2}],[a(=<),b(>=),c],Sorted).  

sort_dicts(In,[],In) :- !.
sort_dicts(In,[TmpKey|Keys],Sorted) :-
	sort_dicts(In,Keys,TmpSorted),
	(TmpKey =.. [Key,Order] -> sort(Key,Order,TmpSorted,Sorted) ; Sorted = TmpSorted).

dicts_to_rows([],_,[]).
dicts_to_rows([Dict|Dicts],Headers,[Row|Rows]) :-
    dict_to_row(Dict,Headers,Row),
    dicts_to_rows(Dicts,Headers,Rows).

dict_to_row(Dict,Headers,Row) :-
    var(Headers),!,
    dict_pairs(Dict,Tag,Pairs),
    pairs_keys_values(Pairs, Headers, Values),
    Row =.. [Tag|Values].

dict_to_row(Dict,Headers,Row) :-
    \+var(Headers),!,
    dict_pairs(Dict,Tag,Pairs),
    pairs_keys_values(Pairs, Keys, Values),
    findall(Value,(member(Key,Headers),nth1(I,Keys,Key),nth1(I,Values,Value)),NValues),
    Row =.. [Tag|NValues].

is_list_of_dicts([],_,_).
is_list_of_dicts([Dict|Dicts],Tag,Keys) :-
	is_dict(Dict,Tag),
	dict_keys(Dict, Keys),
	is_list_of_dicts(Dicts,Tag,Keys).


% create a hook in swish_render_table to render specific terms
:- abolish(swish_render_table:row/4).
swish_render_table:row([], _) --> [].
swish_render_table:row([H|T], Options) -->
	{
		member(keys(Keys),Options),
		length(Keys,N),
		length(T,M),
		I is N - M,
		nth1(I,Keys,Key),
		FancyTerm =.. [Key,H]
		%debug(fancy_table,"fancy_term: ~w", [FancyTerm])
	},
	fancy_term(FancyTerm,Options),
	swish_render_table:row(T, Options).
swish_render_table:row([H|T], Options) -->
	html(td(\term(H, Options))),
	swish_render_table:row(T, Options).
swish_render_table:row([H|T], Options) -->
	html(td(\term(H, []))),
	swish_render_table:row(T, Options).
