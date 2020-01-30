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

-import(aux, [fun_token/1,
              read_at_index/2,
              write_v/4,
              read_v/3,
              fst/1,
              snd/1]).

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

user(X) -> X == "anton".

psswd(X) -> X == "1234".

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


list_vals_noreq() ->
  Sh = [{"ship",    "Ship"},    ["Service"],         [], [{"Service","fun user"}, {"Service","fun psswd"}]],
  Se = [{"service", "Service"}, ["Ship", "Company"], [], []                                               ],
  Co = [{"company", "Company"}, ["Service"],    ["map"], []                                               ],
  [Sh, Se, Co].

list_vals() ->
  Sh = [{"ship",    "Ship"},    ["Service"],         [], [{"Service","fun psswd"}, {"Service","fun user"}], [["Service", "map", ["anton", "1234"]]]],
  Se = [{"service", "Service"}, ["Ship", "Company"], [], [],                                                []],
  Co = [{"company", "Company"}, ["Service"],    ["map"], [],                                                []],
  [Sh, Se, Co].


c_l_helper() ->
  _Input       = list_vals(),
  _Input_noreq = list_vals_noreq(),
  convert_lists(_Input).

e_loop_start(0, _, Name_list, Val_list) ->
  {Name_list, Val_list};
e_loop_start(Count, [EH|Entities], Name_list, Val_list) ->
  {Upd_name_list, Upd_val_list} = start_helper(EH, Name_list, Val_list),
  e_loop_start(Count - 1, Entities, Upd_name_list, Upd_val_list).

start_helper([{S_atom, S_name}|_], Name_list, Val_list) ->
  aux:write_v(S_name, S_atom, Name_list, Val_list).

e_loop_add_relation(0, _, _, _, _) ->
  ok;
e_loop_add_relation(Count, C, Entities, Name_list, Val_list) ->
  ok = add_relation_help(C,  Entities, Name_list, Val_list),
  e_loop_add_relation(Count - 1, C + 1, Entities, Name_list, Val_list).

add_relation_help(C, Entities, Name_list, Val_list) ->
  Ent      = aux:read_at_index(C, Entities),
  Ent_pair = aux:read_at_index(1, Ent),
  Rels     = aux:read_at_index(2, Ent),
  Ent_name = aux:snd(Ent_pair),
  From     = aux:read_v(Ent_name, Name_list, Val_list),  
  r_loop_add_relation(From, Rels, Name_list, Val_list).

r_loop_add_relation(_, [], _, _) ->
  ok;
r_loop_add_relation(From, [RH|Rels], Name_list, Val_list) ->
  To = aux:read_v(RH, Name_list, Val_list),
  {ok, From} = mmods:add_relation(From, To),
  r_loop_add_relation(From, Rels, Name_list, Val_list).

e_loop_add_info(0, _, _, _, _) ->
  ok;
e_loop_add_info(Count, C, Entities, Name_list, Val_list) ->
  ok = add_info_help(C, Entities, Name_list, Val_list),
  e_loop_add_info(Count - 1, C + 1, Entities, Name_list, Val_list).

add_info_help(C, Entities, Name_list, Val_list) ->
  Ent      = aux:read_at_index(C, Entities),
  Ent_pair = aux:read_at_index(1, Ent),
  Info     = aux:read_at_index(3, Ent),
  Ent_name = aux:snd(Ent_pair),
  Id       = aux:read_v(Ent_name, Name_list, Val_list),  
  i_loop_add_info(Id, Info, Name_list, Val_list).

i_loop_add_info(_, [], _, _) ->
  ok;
i_loop_add_info(Id, [IH|Info], Name_list, Val_list) ->
  ok = mmods:add_info(Id, IH),
  i_loop_add_info(Id, Info, Name_list, Val_list).

e_loop_add_dependency(0, _, _, _, _) ->
  ok;
e_loop_add_dependency(Count, C, Entities, Name_list, Val_list) ->
  ok = add_dependency_help(C, Entities, Name_list, Val_list),
  e_loop_add_dependency(Count - 1, C + 1, Entities, Name_list, Val_list).

add_dependency_help(C, Entities, Name_list, Val_list) ->
  Ent        = aux:read_at_index(C, Entities),
  Ent_pair   = aux:read_at_index(1, Ent),
  Dependency = aux:read_at_index(4, Ent),
  Ent_name   = aux:snd(Ent_pair),
  From       = aux:read_v(Ent_name, Name_list, Val_list),  
  d_loop_add_dependency(From, Dependency, Name_list, Val_list).

d_loop_add_dependency(_, [], _, _) ->
  ok;
d_loop_add_dependency(Id, [DH|Dependency], Name_list, Val_list) ->
  To  = aux:read_v(aux:fst(DH), Name_list, Val_list),
  Dep = aux:fun_token(aux:snd(DH)),
  ok  = mmods:add_dependency(Id, To, Dep),
  d_loop_add_dependency(Id, Dependency, Name_list, Val_list).

e_loop_request(0, _, _, _, _) ->
  ok;
e_loop_request(Count, C, Entities, Name_list, Val_list) ->
  ok = request_help(C, Entities, Name_list, Val_list),
  e_loop_request(Count - 1, C + 1, Entities, Name_list, Val_list).

request_help(C, Entities, Name_list, Val_list) ->
  Ent      = aux:read_at_index(C, Entities),
  Ent_pair = aux:read_at_index(1, Ent),
  Request  = aux:read_at_index(5, Ent),
  Ent_name = aux:snd(Ent_pair),
  From     = aux:read_v(Ent_name, Name_list, Val_list),  
  r_loop_request(From, Request, Name_list, Val_list).

r_loop_request(_, [], _, _) ->
  ok;
r_loop_request(Id, [RH|Request], Name_list, Val_list) ->
  To      = aux:read_v(aux:read_at_index(1, RH), Name_list, Val_list),
  Info    = aux:read_at_index(2, RH),
  Answers = aux:read_at_index(3, RH),
  erlang:display(To),
  erlang:display(Info),
  erlang:display(Answers),
  mmods:request_info(Id, To, Info, Answers),
  r_loop_request(Id, Request, Name_list, Val_list).


convert_lists([]) ->
  ok;
convert_lists(Entities) ->
  {Name_list, Val_list} = e_loop_start(length(Entities), Entities, [], []),
  ok = e_loop_add_relation(length(Entities), 1, Entities, Name_list, Val_list),
  ok = e_loop_add_info(length(Entities), 1, Entities, Name_list, Val_list),
  ok = e_loop_add_dependency(length(Entities), 1, Entities, Name_list, Val_list),
  ok = e_loop_request(length(Entities), 1, Entities, Name_list, Val_list),
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




