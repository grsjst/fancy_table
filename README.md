# FancyTable for SWISH (fancy_table)

FancyTable is a SWISH plugin (https://github.com/SWI-Prolog/SWISH) to generate HTML tables, with some fancy features, including:
- sorting of columns
- user defined fancy rendering of specific data types, values a columns.
- export to spreadsheet format (e.g xslx, ods, csv).

Under the hood, it uses: 
- The SWISH table render plugin (see https://swish.swi-prolog.org/example/render_table.swinb)
- The XLSX render (included in this GIT repository), which is used to to export the data using SheetJS (see https://github.com/SheetJS/sheetjs) 

## Installation

These instructions assume:
- You have installed SWISH (further referred to as `$SWISH_DIR`)
- A \*nix like OS (but it may work for others)

Download the plugin to your preferred location (further referred to as `$PLUGIN_FANCYTABLE`)

```bash
git clone https://github.com/grsjst/fancy_table.git
```

Download the SheetJS libraries and plugins to ./js/node_modules (using `yarn` it will directly download in the correct dir)

```bash
cd $PLUGIN_FANCYTABLE
yarn add xlsx 
```

Edit `$PLUGIN_FANCYTABLE/fancy_table.pl` and edit the following line to set `$PLUGIN_FANCYTABLE` 

```swipl
% $PLUGIN_FANCYTABLE should be set to the root of PLUGIN_SVGDOTJS
user:file_search_path(plugin_svgdotjs, $PLUGIN_FANCYTABLE)
```

Create a symbolic link (or copy) from `$PLUGIN_FANCYTABLE/fancy_table.pl` to `$SWISH_DIR/config-available`
```bash
cd $SWISH_DIR/config-enabled
ln -s $PLUGIN_FANCYTABLE/fancy_table.pl .
```

Enable the plugin in SWISH by making the plugin available in `config-enabled` (create if it doesn't exist)

```bash
cd $SWISH_DIR/config-enabled
ln -s ../config-available/fancy_table.pl .
```

Then relaunch SWISH (or type `make` in swipl console)

## Usage

In your SWISH programme add the directive: 

```swipl
:- use_rendering(fancy_table).   % 
```

As an example, run the following query to render a rectangle:

```swipl
X = [_{a:1,b:date(2020,1,1)}].
```

The PLUGIN_SVGDOTJS renderer defines the following options:
- `header/1` - it contains a compound of the form row(id(>=),label(\_)), which denotes the table has two columns (id, label) and is sorted
	on the first
- `export/2` - contains the filename for export, and options passed to XSLX (see https://sheetjs.gitbooks.io/docs/#writing-options)
- `fancy_term/1` refers to a SWISH file that specified rules to render terms. The rules are of the form:

```swipl
fancy_term(profit,important(Value), _Options) -->
	html(td,b((\term(Value,[])))).
```

## Examples

@todo

## Files

```
fancy_table.pl - loads the svgdotjs renderer in SWISH`
render/fancy_table.pl - the FancyTable renderer
render/xlsx.pl - the XLSC renderer
```

