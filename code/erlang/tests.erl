-module(tests).

-import(mmods, [start/1,
                add_relation/2,
                get_state/1,
                get_type/1,
                get_relations/1,
                add_dependency/3,
                add_info/2,
                remove_info/2,
                transfer_info/3,
                request_info/4]).

-import(aux, [read_at_index/2,
              write_v/4,
              read_v/3]).

-import(lists, [reverse/1]).

-compile(export_all).
-compile(nowarn_export_all).

main() ->
  Ship_a = ship,
  Service_a = service,
  Company_a = company,
  {ok, Ship}    = mmods:start(Ship_a),
  {ok, Service} = mmods:start(Service_a),
  {ok, Company} = mmods:start(Company_a),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  {ok, Service} = mmods:add_relation(Service, Ship),
  {ok, Company} = mmods:add_relation(Company, Service),
  mmods:add_dependency(Ship, Service, fun trivial/1),
  mmods:add_dependency(Ship, Service, fun trivial/1),
  mmods:add_dependency(Ship, Service, test3),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  A             = mmods:get_state(Ship),
  B             = mmods:get_state(Service),
  C             = mmods:get_state(Company),
  {A, B, C}.

trivial(_) -> true.

is_equal(X) ->
  case is_integer(X) of
    true -> X rem 2 == 0;
    false -> false
  end.

user(X) -> X == anton.

psswd(X) -> X == 1234.

deps_true() ->
  {ok, Ship}     = mmods:start(ship),
  {ok, Service1} = mmods:start(service),
  {ok, Service2} = mmods:start(service),
  {ok, Service3} = mmods:start(service),
  {ok, Service4} = mmods:start(service),
  {ok, Ship}     = mmods:add_relation(Ship, Service1),
  {ok, Ship}     = mmods:add_relation(Ship, Service2),
  {ok, Ship}     = mmods:add_relation(Ship, Service3),
  {ok, Ship}     = mmods:add_relation(Ship, Service4),
  ok             = mmods:add_dependency(Ship, Service1, fun trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun trivial/1),
  ok             = mmods:add_dependency(Ship, Service3, fun trivial/1),
  ok             = mmods:add_dependency(Ship, Service4, fun trivial/1),
  mmods:get_state(Ship).

transfer() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Company} = mmods:start(company),
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_info(Company, map),
  {ok, map}     = mmods:transfer_info(Company, Service, map),
  {ok, map}     = mmods:transfer_info(Service, Ship, map),
  Ship_info     = mmods:get_info(Ship),
  Company_info  = mmods:get_info(Company),
  (Ship_info == Company_info).

request() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Company} = mmods:start(company),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  mmods:add_dependency(Ship, Service, fun trivial/1),
  mmods:add_info(Company, map),
  mmods:request_info(Ship, Service, map, [ans]),
  mmods:get_info(Ship).

protocol() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Company} = mmods:start(company),

  ok            = mmods:add_info(Company, "map"),

  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),

  ok            = mmods:add_dependency(Ship, Service, fun psswd/1),
  ok            = mmods:add_dependency(Ship, Service, fun user/1),

  request_info(Ship, Service, "map", [anton, 1234]),

  A = mmods:get_info(Ship)    == ["map"],
  B = mmods:get_info(Service) == [],
  C = mmods:get_info(Company) == ["map"],
  {A, B, C}.


list_vals_nofuninfo() ->
  Sh = [{"ship",    "Ship"},    ["Service"]        ],
  Se = [{"service", "Service"}, ["Ship", "Company"]],
  Co = [{"company", "Company"}, ["Service"]        ],
  [Sh, Se, Co].

c_l_helper() ->
  Input_nofuninfo = list_vals_nofuninfo(),

  convert_lists(Input_nofuninfo).

loop_start(0, _, Name_list, Val_list, Rest_list) ->
  {Name_list, Val_list, Rest_list};
loop_start(Count, [IH|Input], Name_list, Val_list, Rest_list) ->
  {Upd_name_list, Upd_val_list, Rest} = start_helper(IH, Name_list, Val_list),
  loop_start(Count - 1, Input, Upd_name_list, Upd_val_list, Rest ++ Rest_list).

start_helper([{S_atom, S_name}|Rest_list], Name_list, Val_list) ->
  {Upd_name_list, Upd_val_list} = aux:write_v(S_name, S_atom, Name_list, Val_list),
  {Upd_name_list, Upd_val_list, Rest_list}.

loop_relations(0, _, _, Name_list, Val_list, Rest_list) ->
  {Name_list, Val_list, Rest_list};
loop_relations(Count, Rev_c, [IH|Input], Name_list, Val_list, Rest_list) ->
  relations_helper(length(IH), Rev_c,  IH, Name_list, Val_list),
  loop_relations(Count - 1, Rev_c + 1, Input, Name_list, Val_list, Rest_list).

relations_helper(0, _, [], _, _) ->
  ok;
relations_helper(Count, Rev_c, [IH|Input], Name_list, Val_list) ->
  From       = aux:read_v(aux:read_at_index(Rev_c, Name_list), Name_list, Val_list),
  To         = aux:read_v(IH, Name_list, Val_list),
  {ok, From} = mmods:add_relation(From, To ),
  relations_helper(Count - 1, Rev_c, Input, Name_list, Val_list).

convert_lists([]) ->
  ok;
convert_lists(Entities) ->
  {Name_list, Val_list, Relations} = loop_start(length(Entities), Entities, [], [], []),
  loop_relations(length(Relations), 1, Relations, Name_list, Val_list, []),
  _A = mmods:get_state(aux:read_at_index(1, Val_list)),
  _B = mmods:get_state(aux:read_at_index(2, Val_list)),
  _C = mmods:get_state(aux:read_at_index(3, Val_list)),
  {_A, _B, _C}.



% [{xmlText,[{relations,6},{ent,2},{entities,1}],1,[],"\n      ",text}, {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],2,[],[{xmlText,[{relation,2},{relations,6},{ent,2},{entities,1}],1,[],"Service",text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],3,[],"\n    ",text}]
% 
% [{xmlText,[{relations,6},{ent,2},{entities,1}],1,[],"\n      ",text}, {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],2,[],[{xmlText,[{relation,2},{relations,6},{ent,2},{entities,1}],1,[],"Service",text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],3,[],"\n      ",text},
%                                                                       {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],4,[],[{xmlText,[{relation,4},{relations,6},{ent,2},{entities,1}],1,[],"Company",text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],5,[],"\n    ",text}]
% 
% [{xmlText,[{relations,6},{ent,2},{entities,1}],1,[],"\n      ",text}, {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],2,[],[{xmlText,[{relation,2},{relations,6},{ent,2},{entities,1}],1,[],"Service",text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],3,[],"\n      ",text},
%                                                                       {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],4,[],[{xmlText,[{relation,4},{relations,6},{ent,2},{entities,1}],1,[],"Company",text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],5,[],"\n      ",text},
%                                                                       {xmlElement,relation,relation,[],{xmlNamespace,[],[]},[{relations,6},{ent,2},{entities,1}],6,[],[{xmlText,[{relation,6},{relations,6},{ent,2},{entities,1}],1,[],"Ship",   text}],[],undefined,undeclared},{xmlText,[{relations,6},{ent,2},{entities,1}],7,[],"\n    ",text}]          



%Right (
%  Tag [] "entities" (
%    Just [TagString "\n    ",Tag [] "ent" (
%      Just [TagString "\n        ",Tag [] "type" (
%        Just [TagString "ship"]
%      ), TagString "\n        ",Tag [] "name" (
%        Just [TagString "Ship"]
%      ),TagString "\n        ", Tag [] "relations" (
%        Just [TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Service"]
%        ),TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Company"]
%        ),TagString "\n        "]
%      ),TagString "\n        ",Tag [] "dependencies" (
%        Just [TagString "\n            ",Tag [] "dependency" (
%          Just [TagString "\n                ",Tag [] "dep" (
%            Just [TagString "\n                    ",Tag [] "to" (
%              Just [TagString "Service"]
%            ),TagString "\n                    ",Tag [] "constraint" (
%              Just []
%            ),TagString "\n                    ",Tag [] "constraint" (
%              Just []
%            ),TagString "\n                "]
%          ),TagString "\n            "]
%        ),TagString "\n        "]
%      ),TagString "\n        ",Tag [] "information" (
%        Just []
%      ),TagString "\n    "]
%    ),TagString "\n    ",Tag [] "ent" (
%      Just [TagString "\n        ",Tag [] "type" (
%        Just [TagString "service"]
%      ),TagString "\n        ",Tag [] "name" (
%        Just [TagString "Service"]
%      ),TagString "\n        ",Tag [] "relations" (
%        Just [TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Ship"]
%        ),TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Company"]
%        ),TagString "\n        "]
%      ),TagString "\n        ",Tag [] "dependencies" (
%        Just []
%      ),TagString "\n        ",Tag [] "information" (
%        Just []
%      ),TagString "\n    "]
%    ),TagString "\n    ",Tag [] "ent" (
%      Just [TagString " \n        ",Tag [] "type" (
%        Just [TagString "company"]
%      ),TagString "\n        ",Tag [] "name" (
%        Just [TagString "Company"]
%      ),TagString "\n        ",Tag [] "relations" (
%        Just [TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Service"]
%        ),TagString "\n        "]
%      ),TagString "\n        ",Tag [] "dependencies" (
%        Just [TagString " "]
%      ),TagString "\n        ",Tag [] "information" (
%        Just [TagString "\n            ",Tag [] "info" (
%          Just [TagString "map"]
%        ),TagString "\n        "]
%      ),TagString "\n    "]
%    ),TagString "\n    ",Tag [] "ent" (
%      Just [TagString " \n        ",Tag [] "type" (
%        Just [TagString "company"]
%      ),TagString "\n        ",Tag [] "name" (
%        Just [TagString "Company"]
%      ),TagString "\n        ",Tag [] "relations" (
%        Just [TagString "\n            ",Tag [] "relation" (
%          Just [TagString "Service"]
%        ),TagString "\n        "]
%      ),TagString "\n        ",Tag [] "dependencies" (
%        Just [TagString " "]
%      ),TagString "\n        ",Tag [] "information" (
%        Just [TagString "\n            ",Tag [] "info" (
%          Just [TagString "map"]
%        ),TagString "\n        "]
%      ),TagString "\n    "]
%    ),TagString "\n"]
%  )
%)




