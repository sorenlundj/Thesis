-module(interpreter).

-import(aux, [read_at_index/2,
              split_tuple_list/3,
              write_v/4,
              read_v/3,
              token/1]).

-export([interp/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maritime Model Interpreter                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interp(Term) ->
  {entities, Entities} = Term,
  {Name_list, Val_list} = loop_start(length(Entities), Entities, [], []),
  ok = loop_add_info(length(Entities), Entities, Name_list, Val_list),
  ok = loop_add_relations(length(Entities), Entities, Name_list, Val_list),
  ok = loop_add_dependency(length(Entities), Entities, Name_list, Val_list),
  ok = loop_request(length(Entities), Entities, Name_list, Val_list),
  _A = mmods:get_state(aux:read_at_index(1, Val_list)),
  _B = mmods:get_state(aux:read_at_index(2, Val_list)),
  _C = mmods:get_state(aux:read_at_index(3, Val_list)),
  {_A, _B, _C}.

loop_start(0, _, Name_list, Val_list) ->
  {Name_list, Val_list};
loop_start(C, Entities, Name_list, Val_list) ->
  {ent, Ent}                    = aux:read_at_index(C, Entities),
  {Fields, Vals}                = aux:split_tuple_list(Ent, [], []),
  Type                          = aux:read_at_index(aux:index_of(type, Fields, 0), Vals),
  Name                          = aux:read_at_index(aux:index_of(name, Fields, 0), Vals),
  {Upd_name_list, Upd_val_list} = aux:write_v(Name, Type, Name_list, Val_list),
  loop_start(C - 1, Entities, Upd_name_list, Upd_val_list).

loop_add_info(0, _, _, _) ->
  ok;
loop_add_info(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_at_index(aux:index_of(name, Fields, 0), Vals),
  Info           = aux:read_at_index(aux:index_of(information, Fields, 0), Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = add_info_helper(From, Info),
  loop_add_info(C - 1, Entities, Name_list, Val_list).

add_info_helper(_, []) ->
  ok;
add_info_helper(From, [I_head|Info]) ->
  {info, Inf} = I_head,
  mmods:add_info(From, Inf),
  add_info_helper(From, Info).

loop_add_relations(0, _, _, _) ->
  ok;
loop_add_relations(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_at_index(aux:index_of(name, Fields, 0), Vals),
  Relations      = aux:read_at_index(aux:index_of(relations, Fields, 0), Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = add_relation_helper(From, Relations, Name_list, Val_list),
  loop_add_relations(C - 1, Entities, Name_list, Val_list).

add_relation_helper(_, [], _, _) ->
  ok;
add_relation_helper(From, [R_head|Relations], Name_list, Val_list) ->
  {relation, Relation} = R_head,
  To = aux:read_v(Relation, Name_list, Val_list),
  {ok, From} = mmods:add_relation(From, To),
  add_relation_helper(From, Relations, Name_list, Val_list).

loop_add_dependency(0, _, _, _) ->
  ok;
loop_add_dependency(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_at_index(aux:index_of(name, Fields, 0), Vals),
  Dependencies   = aux:read_at_index(aux:index_of(dependencies, Fields, 0), Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = add_dependency_helper(From, Dependencies, Name_list, Val_list),
  loop_add_dependency(C - 1, Entities, Name_list, Val_list).

add_dependency_helper(_, [], _, _) ->
  ok;
add_dependency_helper(From, [D_head|Dependencies], Name_list, Val_list) ->
  {dependency, To_and_con} = D_head,
  {to,To_name}             = aux:read_at_index(1, To_and_con),
  {constraint, Con}        = aux:read_at_index(2, To_and_con),
  To                       = aux:read_v(To_name, Name_list, Val_list),
  mmods:add_dependency(From, To, aux:token(Con)),
  add_dependency_helper(From, Dependencies, Name_list, Val_list).

loop_request(0, _, _, _) ->
  ok;
loop_request(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_at_index(aux:index_of(name, Fields, 0), Vals),
  Requests       = aux:read_at_index(aux:index_of(requests, Fields, 0), Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = request_helper(From, Requests, Name_list, Val_list),
  loop_request(C - 1, Entities, Name_list, Val_list).

request_helper(_, [], _, _) ->
  ok;
request_helper(From, [R_head|Requests], Name_list, Val_list) ->
  {request, Req}   = R_head,
  {to, To_name}    = aux:read_at_index(1, Req),
  {data, Data}     = aux:read_at_index(2, Req),
  {answers, Ans_t} = aux:read_at_index(3, Req),
  To               = aux:read_v(To_name, Name_list, Val_list),
  {_, Ans}         = aux:split_tuple_list(Ans_t, [], []),
  mmods:request_info(From, To, Data, Ans),
  request_helper(From, Requests, Name_list, Val_list).
