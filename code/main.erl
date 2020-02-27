-module(main).

-import(parser, [file_parser/1,
				 string_parser/1]).

-import(interpreter, [interp/1]).

-export([fparse/1,
         sparse/1,
         interpret/1,
         fparse_and_interpret/1,
         sparse_and_interpret/1]).

fparse(File) ->
  Term = parser:file_parser(File),
  Term.

sparse(File) ->
  Term = parser:string_parser(File),
  Term.

interpret(Term) ->
  Model = interpreter:interp(Term),
  Model.

fparse_and_interpret(File) ->
  Term  = fparse(File),
  Model = interpret(Term),
  Model.

sparse_and_interpret(File) ->
  Term  = sparse(File),
  Model = interpret(Term),
  Model.