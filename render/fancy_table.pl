:- module(swish_render_fancy_table,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(csv)).
:- use_module(swish(lib/render)).
:- use_module(swish(lib/render/table),[term_rendering/5 as table_term_rendering]).
:- use_module(library(debug)).

:- debug(fancy_table).
:- debug(fancy_table,"Fancy table renderer loaded",[]).

:- register_renderer(fancy_table, "Fancy options for rendering tables").
:- use_module(swish(lib/storage)).


% :- use_gitty_file(test_fancy_term). % this comes from SWISH


/** <module> SWISH fancy table renderer for Dicts

	Includes fancy options for rendering tables that include:
	- multiple key sorting
	- special formatting for specific cell or column values
	- download as xls 
	
	key sorting is specified through a header specification that is part of the render options.
	It takes the format: header(Tag,[KeyA(Order),KeyB(Order),..]).

	A number of specialised cell renders are provided (e.g. renndering dates, url, rdf resources). Additional
	cell renders may be provide through the fancy_file(+File) parameter in the redner options. File refers
	to a SWISH filename. This file inludes DCG rules of the format fancy_term(ColumnName,Term,Options)
		ColumnName refers to the Key, Term is the term that is to be rendered, Options are the Renderer Options

	e.g. fancy_term(profit,important(Value),Options) -->
			html(td(b(\term(Term, Options)))).

	is a directive to render the Value bold if it is marked important, and is presented under the profit column 

	The renderer is based on the table renderer that is part of the core SWISH distribution
*/


%%	term_rendering(+Rows:list, +Vars:list, +Options:list)//
%
%	Renders Rows as a fancy_table.
% 	Options:
% 	- fancy_term_file(GittyFile) GittyFile specfies user defined rendering for particular cells
%	- export(Format,FName)  FName is the filename suggested for export
term_rendering(Dicts, _Vars, Options) -->
	{
		% debug(fancy_table, "try fancy table: ~p",[Dicts]),
		to_rows(Dicts,Tag,Rows), % Rows = list of compounds -> to rows ()
		match_header(Options,Rows,FancyHeader),!,
		debug(fancy_table, "use fancy table - header:~w, options:~w",[FancyHeader,Options]),
		header_keys(FancyHeader,Tag,SortKeys,Keys),
		reorder_values(Rows,Keys,OrderedRows),
		sort_rows(OrderedRows,SortKeys,SortedRows),
		delete(Options,header(_),NOptions)
	},
	fancy_table_rendering(FancyHeader,SortedRows,NOptions). 

%!	to_rows(+Dicts:list(dict),-Tag:atom, -Rows:list(term)) is det.
%	converts dicts to corresponding compound representation
% 		Tag is the dict tag, which is represented as functor of the compound
%			in case the Tag isn't an atom, "row" is used
%		Key:Value are represented as pairs Key-Value
%
% 	e.g. to_rows([_{x:1,y:1,z:2},_{x:2,y:3,z:1}],T,Rs).
to_rows([],Tag,[]) :- (var(Tag) -> Tag = row ; true),!.
to_rows([Dict|Dicts],Tag,[Row|Rows]) :-
	is_dict(Dict,Tag),
	to_rows(Dicts,Tag,Rows),
	dict_pairs(Dict,Tag,Pairs),
	Row =.. [Tag|Pairs].


%! 	match_header(+Options,+Rows,-Header) is nondet.
%	from the Options select a header that matches the given Tag and the layout of the Rows 
match_header(Options,Rows,Header) :-
	common_keys(Rows,CommonTag,CommonKeys),
	member(header(Header),Options),
	header_keys(Header,CommonTag,_,HeaderKeys),
	subset(HeaderKeys,CommonKeys).

%!	common_keys(+Rows:list(term),-Tag:atom,-Keys:list(atom)) is det.
%  	the list of keys that are present in all rows as wel as the tag that is
% 	common to a Rows.
% 
% 	e.g. common_keys([row(a-3,c-1,b-2],row(b-2,a-2,c-2,d-0),row(a,b,c)],Tag,Keys)
common_keys([],[]) :- !.
common_keys([Row],CommonTag,Keys) :- Row =.. [CommonTag|Pairs],pairs_keys(Pairs,Keys).
common_keys([Row|Rows],CommonTag,CommonKeys) :-
	Row =.. [CommonTag|Pairs],
	pairs_keys(Pairs,Keys),
	common_keys(Rows,CommonTag,CKeys),
	intersection(Keys,CKeys,CommonKeys).

header_keys(Header,Tag,SortKeys,HeaderKeys) :-
	Header =.. [Tag|SortKeys],
	findall(HKey,(member(SortKey,SortKeys),SortKey =.. [HKey,_]),HeaderKeys).


%!	reorder_value(+Rows:list(term),+Keys:list(atom),-OrderedRows:list(term)) is det.
%	reposition the Key-Value pairs in Rows according to the scheme provide by Keys
%
% 	e.g. reorder_values([row(a-3,c-1,b-2),row(b-2,a-2,c-2),row(a-1,b-2,c-3)],[a,b,c],Ordered)
reorder_values([],_,[]).
reorder_values([Row|Rows],Keys,[OrderedRow|OrderedRows]) :-
	Row =.. [Tag|Pairs],
	order_pairs(Pairs,Keys,OrderedPairs),
	OrderedRow =.. [Tag|OrderedPairs],
	reorder_values(Rows,Keys,OrderedRows).

order_pairs(Pairs,[],Pairs) :- !.
order_pairs(Pairs,[K|Ks],[K-V|OrderedPairs]) :-
	select(K-V,Pairs,RPairs),!,
	order_pairs(RPairs,Ks,OrderedPairs).

%!	sort_rows(+Rows:list(term),+SortKeys:list(term),-SortedRows:list(term)) is det.
%	Sort rows on multiple columns provided through SortKeys.
%		a Sortkey is of the format Key(Order), 
% 	e.g. sort_rows([row(a-3,b-2,c-1),row(a-2,b-2,c-2),row(a-2,b-1,c-2)],[a(@>=),b(@=<)],Sorted)
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

get_keys(Rows,Keys) :-
	get_keys(Rows,1,Keys).
get_keys(Rows,I,[]) :- forall(member(Row,Rows),(functor(Row,_,N), I > N)).
get_keys(Rows,I,[Key|Keys]) :-
	Rows = [FirstRow|_],
	arg(I,FirstRow,Key-_),
	forall(member(Row,Rows),arg(I,Row,Key-_)),
	J is I + 1,
	get_keys(Rows,J,Keys).

fancy_table_rendering(Header,Rows,Options) -->
	{
		header_keys(Header,Tag,_,Keys),
		NHeader =.. [Tag|Keys],
		(memberchk(fancy_terms_file(GittyFile),Options) -> 
			(
				debug(fancy_table,"load gittyfile: ~w",[GittyFile]),
				use_gitty_file(GittyFile)
			) ; true)
		% debug(fancy_table, "Tag: ~w, Keys:~w, Rows:~w, Options:~w, Header:~w",[Tag, Keys, Rows, Options,Header])
	},
	html(div([class(['export-dom']), style('display:inline-block'),
		   'data-render'('Fancy Table with Header')
		 ],
		 [  
		 	\table_term_rendering(Rows,_,[header(NHeader)|Options]),
		 	\fancy_table_footer(Rows,Options)		 	
		 ])).
 
fancy_table_footer([],_) --> html(p("0 records")).
fancy_table_footer(Rows,Options) -->
	{
		length(Rows,N),
		format(string(FooterMesg),"~w records",[N]),
		(memberchk(export(FName,WBOptions),Options) -> 
			(
				debug(fancy_table,"export fname: ~w, options:~w,",[FName,WBOptions])
			) ; 
			(
				WBOptions = _{bookType:'xlsx'},
				FName = "export.xlsx"
			)),
		to_xlsx(Rows,WB,WBOptions)
	},
	html([
		p([FooterMesg," ",a([href("")],"(export)"),
		\js_script({|javascript(WB,FName,WBOptions)||
(function() {
  if ( $.ajaxScript ) {
  	console.log("from fancy_table.pl");
  	var a = $.ajaxScript.parent().find("a")[0];
	requirejs.config({
    	urlArgs: "ts="+new Date().getTime(),  /* prevent caching during development */
	    paths: {
	    	xlsx: '../node_modules/xlsx/dist/xlsx.full.min'
	    }
	 });

	require(["jquery","xlsx"], function($,XLSX) {
		var wb = WB;
		var wbOptions = WBOptions;
		var fname = FName;
		$(a).click(function(e){
			e.preventDefault();
			XLSX.writeFile(wb, fname, wbOptions);
			});
	});  }
})();
		  |})
		])]).

to_xlsx(Rows,CSF_WB,_Options) :-
	Rows = [Row|_], functor(Row,_,NColumns),
	length(Rows,NRows),
	to_a1(1,1,TopLeft),
	to_a1(NColumns,NRows,BottomRight),
	format(atom(Range),"~w:~w",[TopLeft,BottomRight]),
	to_csf_sheet(Rows,Sheet),
	NSheet = Sheet.put('!ref',Range),
	CSF_WB = _{
		'Sheets' : _{
			'Sheet1' : NSheet
		},
		'SheetNames' : ['Sheet1']  
	}.

to_csf_sheet(Rows,Sheet) :-
	to_csf_sheet(Rows,1,Sheet).

to_csf_sheet([],_,_{}).
to_csf_sheet([Row|Rows],R,Sheet) :-
	Row =.. [_|Pairs],
	findall(Cell,(nth1(C,Pairs,_-Value),to_csf_cell(C,R,Value,Cell)),Cells),
	merge_dicts(Cells,SheetRow),
	debug(fancy_table,"row: ~w, cells: ~w",[Row,SheetRow]),
	NR is R + 1,
	to_csf_sheet(Rows,NR,SheetRows),
	Sheet = SheetRows.put(SheetRow).

merge_dicts([],_{}).
merge_dicts([Dict|Dicts],MergedDict) :-
	merge_dicts(Dicts,MergedDict0),
	MergedDict = MergedDict0.put(Dict).

to_csf_cell(C,R,Value,CSFCell) :-
	to_a1(C,R,A1),
	to_cell(Value,Cell),
	CSFCell = _{}.put(A1,Cell).

to_cell(Date,_{v:Value,t:d, z:'dd/mm/yyyy'}) :- Date = date(_,_,_),format_time(atom(Value),"%FT%T%z",Date),!. % ISO8601
to_cell(DateTime,_{v:Value,t:d, z:'dd/mm/yyyy h:mm'}) :- DateTime = date(_,_,_,_,_,_,_,_,_),format_time(atom(Value),"%FT%T%z",DateTime),!. % ISO8601	
to_cell(Boolean,_{v:Boolean,t:b}) :- memberchk(Boolean,[true,false]),!.
to_cell(Number,_{v:Number,t:n}) :- number(Number),!.
to_cell(String,_{v:String,t:s}) :- string(String),!.
to_cell(Atom,_{v:Value,t:s}) :- atom(Atom),atom_string(Atom,Value),!.
to_cell(Term,Cell) :- Term =.. [_,Value],!, to_cell(Value,Cell). % tag(100) -> 100
to_cell(Term,_{v:Value,t:s}) :- term_string(Term,Value),!,debug(fancy_table,"unknown type: ~p",[Term]).

to_a1(C,R,A1) :-
    to_base(26,C,L),
    As = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'],
    findall(A0,(member(N,L),nth1(N,As,A0)),Txt),
    text_to_string(Txt,Alpha),
    format(atom(A1),"~w~w",[Alpha,R]).

to_base(Base,N,[R]) :-
    divmod(N, Base, 0, R),!.

to_base(Base,N,L) :-
    divmod(N, Base, Q, R),
    to_base(Base,Q,Rs),
    append(Rs,[R],L).


%%	fancy_term(Term, +Options)
%
%	Specific renders for values in the table
%	
%	supported;
% 	we renders dates as eg "16 Aug 2018"
%	rdf resources are presented with their label
%	url include a clcikable link 

:- multifile swish_render_fancy_table:fancy_term/5.

user_or_default_fancy_term(ColName,Cell,Options) -->  {debug(fancy_table,"col:~w, cell:~w, ops: ~w",[ColName,Cell,Options])},fancy_term(ColName,Cell,Options).
user_or_default_fancy_term(ColName,Cell,Options) --> def_fancy_term(ColName,Cell,Options).

def_fancy_term(_,Date, _Options) -->
	{   
		Date = date(_,_,_),!,
		debug(fancy_table,"date: ~w",[Date]),
		format_time(string(DateStr),"%d %b %Y",Date)
	},
	html(td(\term(DateStr,[]))).

% use label for IRIs if defined
def_fancy_term(_,IRI, _Options) -->
	{ 
		atom(IRI),
		rdf_iri(IRI),
		rdfs_label(IRI,Label), 
		debug(fancy_table,"iri: ~w, label:~w", [IRI,Label])
	},
	html(td(\term(Label,[]))).

% include a link for uri
def_fancy_term(_,URI, _Options) -->
	{ 
	atom(URI),uri_is_global(URI) 
	},
	html(td(a(href(URI),URI))).

def_fancy_term(ColName,value(Term), Options) --> def_fancy_term(ColName,Term, Options).

% default
def_fancy_term(ColName,Term, _Options) -->
	{ 
		debug(fancy_table,"use default for ColName: ~w, Term:~w", [ColName,Term])
	},
	html(td(\term(Term,[]))).


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


