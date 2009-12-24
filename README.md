Mustache for Erlang
===================

An Erlang port of [Mustache for Ruby][1]. Mustache is a framework-agnostic
templating system that enforces separation of view logic from the template
file. Indeed, it is not even possible to embed logic in the template. This
allows templates to be reused across language boundaries and for other
language independent uses.


Usage
-----

Quick example via `erl` REPL (ensure mustache.erl is in your code path):

    1> Ctx = dict:from_list([{planet, "World!"}]).
    {dict,1,16,16,8,80,48,...}

    2> mustache:render("Hello {{planet}}", Ctx).
    "Hello World!"

A real-world example consists of two files: the view and the template. The
view (logic) file is an Erlang module (simple.erl):

    -module(simple).
    -compile(export_all).

    name() ->
      "Tom".

    value() ->
      10000.

    taxed_value() ->
      value() - (value() * 0.4).

    in_ca() ->
      true.

In the logic file we define functions that will be called by the template.
Some functions reference others, some return values, some return only
booleans.

The template file (simple.mustache):

    Hello {{name}}
    You have just won ${{value}}!
    {{#in_ca}}
    Well, ${{ taxed_value }}, after taxes.
    {{/in_ca}}

Notice that the template references the functions in the view module. The
return values from the view dictate how the template will be rendered. To get
the HTML output, make sure the `simple` module is in your code path and call
the following code:

    mustache:render(simple)

Which tells Mustache to use the `simple` view and to look for a template named
`simple.mustache` in the same directory as the `simple.beam` bytecode file. If
all goes well, it returns the rendered HTML:

    Hello Tom
    You have just won $10000!
    Well, $6000.00, after taxes.


Tag Types
---------

Tags are indicated by the double mustaches. `{{name}}` is a tag. Let's talk
about the different types of tags.

### Variables

The most basic tag is the variable. A `{{name}}` tag in a basic template will
try to call the `name` function on your view. By default a variable "miss"
returns an empty string.

All variables are HTML escaped by default. If you want to return unescaped
HTML, use the triple mustache: `{{{name}}}`.

### Boolean Sections

A section begins with a pound and ends with a slash. That is,
`{{#person}}` begins a "person" section while `{{/person}}` ends it.

If the `person` method exists and calling it returns `false`, the HTML
between the pound and slash will not be displayed.

If the `person` method exists and calling it returns `true`, the HTML
between the pound and slash will be rendered and displayed.

### List Sections

List sections are syntactically identical to boolean sections in that they
begin with a pound and end with a slash. The difference, however, is in the
view: if the function called returns a list, the section is repeated as the
list is iterated over.

Each item in the enumerable is expected to be a dict that will then become the
context of the corresponding iteration. In this way we can construct loops.

For example, imagine this template:

    {{#repo}}
      <b>{{name}}</b>
    {{/repo}}

And this view code:

    def repo
      [dict:from_list([{name, Name}]) || Name <- ["Tom", "Chris", "PJ"]]
    end

When rendered, our view will contain a list of each of the names in the source
list.

### Comments

Comments begin with a bang and are ignored. The following template:

    <h1>Today{{! ignore me }}.</h1>

Will render as follows:

    <h1>Today.</h1>


Meta
----

* Code: `git clone git://github.com/mojombo/mustache.erl.git`

[1]: http://github.com/defunkt/mustache.git