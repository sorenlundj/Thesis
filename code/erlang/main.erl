-module(main).

-import(parser, [xml_parser/1]).

-import(interpreter, [interp/1]).

-import(mmods, [start/1, 
                add_relation/2,
                add_dependency/3,
                add_info/2,
                remove_info/2,
                transfer_info/3,
                request_info/4,
                get_state/1,
                get_type/1,
                get_relations/1,
                get_info/1]).

-export([parse/1,
         interpret/1,
         parse_and_interpret/1,
         start/1, 
         add_relation/2,
         add_dependency/3,
         add_info/2,
         remove_info/2,
         transfer_info/3,
         request_info/4,
         get_state/1,
         get_type/1,
         get_relations/1,
         get_info/1]).

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

start(Type) ->
  mmods:start(Type).

add_relation(From, To) ->
  mmods:add_relation(From, To).

add_dependency(From, To, Dependency) ->
  mmods:add_dependency(From, To, Dependency).

add_info(Id, Info) ->
  mmods:add_info(Id, Info).

remove_info(Id, Info) ->
  mmods:remove_info(Id, Info).

transfer_info(From, To, Info) ->
  mmods:transfer_info(From, To, Info).

request_info(From, To, Info, Answers) ->
  mmods:request_info(From, To, Info, Answers).

get_state(Id) ->
  mmods:get_state(Id).

get_type(Id) ->
  mmods:get_type(Id).

get_relations(Id) ->
  mmods:get_relations(Id).

get_info(Id) ->
  mmods:get_info(Id).