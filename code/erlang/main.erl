-module(main).

-import(parser, [xml_parser/1]).

-import(interpreter, [interp/1]).

-export([parse/1,
         interpret/1,
         parse_and_interpret/1]).

parse(File) ->
  Term = parser:xml_parser(File),
  Term.

interpret(Term) ->
  Model = interpreter:interp(Term),
  Model.

parse_and_interpret(File) ->
  Term  = parse(File),
  Model = interpret(Term),
  Model.  