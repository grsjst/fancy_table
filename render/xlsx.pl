:- module(swish_render_xlsx,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).
:- use_module(library(debug)).

:- debug(xlsx).
:- debug(xlsx,"XLSX renderer loaded",[]).

:- register_renderer(xlsx, "XLSX for exporting tables").


/** <module> Export to XLSX (and other formats)

	Options for exporting tables that include:
	- format (see https://sheetjs.gitbooks.io/docs/#supported-output-formats)
	- filenane

*/


%%	term_rendering(+Rows:list, +Vars:list, +Options:list)//
%
%	Generates a link that export Rows to XLSX
% 	Options:
% 	- format
%	- filename
term_rendering(Rows, _Vars, Options) -->
	{
		% debug(xlsx, "try xlsx: ~p, options: ~p",[Rows, Options]),
		WBOptions = _{bookType:'xlsx',cellDates:true},
		(memberchk(export(FName0,UserWBOptions),Options) -> 
			(
				file_name_extension(Base,Ext,FName0),
				( Ext == '' -> (get_dict(bookType,UserWBOptions,NExt) -> true ; NExt = 'xlsx') ; NExt = Ext),
				file_name_extension(Base,NExt,FName),
				debug(xlsx,"export fname: ~w, options:~w,",[FName,UserWBOptions])
			) ; 
			(
				UserWBOptions = _{},
				FName = "export.xlsx"
			)),
		WBOptions = WBOptions.put(UserWBOptions),
		to_xlsx(Rows,WB,WBOptions)
	},
	html([
		a([href("")],"(export)"),
		\js_script({|javascript(WB,FName,WBOptions)||
(function() {
  if ( $.ajaxScript ) {
  	console.log("from xlsx.pl");
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
		]).

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
	findall(Cell,(nth1(C,Pairs,Value0),normalize_value(Value0,Value),to_csf_cell(C,R,Value,Cell)),Cells),
	merge_dicts(Cells,SheetRow),
	NR is R + 1,
	to_csf_sheet(Rows,NR,SheetRows),
	Sheet = SheetRows.put(SheetRow).

% remove the key from K-V for export
normalize_value(_-V,V) :- !.
normalize_value(V,V).

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
to_cell(Term,_{v:Value,t:s}) :- term_string(Term,Value),!,debug(xlsx,"unknown type: ~p",[Term]).

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
