-module(interpreter).

-import(aux, [read_at_index/2,
              split_tuple_list/3,
              read_val_by_id/3,
              write_v/4,
              read_v/3,
              token/1]).

-export([interp/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maritime Model Interpreter                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interp(Term) ->
  {entities, Entities} = Term,
  Counter       = length(Entities),
  {Names, Vals} = loop_start(Counter, Entities, [], []),
  ok            = loop_add_info(Counter, Entities, Names, Vals),
  ok            = loop_add_relations(Counter, Entities, Names, Vals),
  ok            = loop_add_dependency(Counter, Entities, Names, Vals),
  ok            = loop_request(Counter, Entities, Names, Vals),
  aux:get_all_states_wrapper(Vals).
  

loop_start(0, _, Name_list, Val_list) ->
  {Name_list, Val_list};
loop_start(C, Entities, Name_list, Val_list) ->
  {ent, Ent}                    = aux:read_at_index(C, Entities),
  {Fields, Vals}                = aux:split_tuple_list(Ent, [], []),
  Type                          = aux:read_val_by_id(type, Fields, Vals),
  Name                          = aux:read_val_by_id(name, Fields, Vals),
  {Upd_name_list, Upd_val_list} = aux:write_v(Name, Type, Name_list, Val_list),
  loop_start(C - 1, Entities, Upd_name_list, Upd_val_list).

loop_add_info(0, _, _, _) ->
  ok;
loop_add_info(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_val_by_id(name, Fields, Vals),
  Info           = aux:read_val_by_id(information, Fields, Vals),
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
  Name           = aux:read_val_by_id(name, Fields, Vals),
  Relations      = aux:read_val_by_id(relations, Fields, Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = add_relation_helper(From, Relations, Name_list, Val_list),
  loop_add_relations(C - 1, Entities, Name_list, Val_list).

add_relation_helper(_, [], _, _) ->
  ok;
add_relation_helper(From, [R_head|Relations], Name_list, Val_list) ->
  {relation, R} = R_head,
  To            = aux:read_v(R, Name_list, Val_list),
  {ok, From}    = mmods:add_relation(From, To),
  add_relation_helper(From, Relations, Name_list, Val_list).

loop_add_dependency(0, _, _, _) ->
  ok;
loop_add_dependency(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_val_by_id(name, Fields, Vals),
  Dependencies   = aux:read_val_by_id(dependencies, Fields, Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = add_dependency_helper(From, Dependencies, Name_list, Val_list),
  loop_add_dependency(C - 1, Entities, Name_list, Val_list).

add_dependency_helper(_, [], _, _) ->
  ok;
add_dependency_helper(From, [D_head|Dependencies], Name_list, Val_list) ->
  {dependency, To_and_con} = D_head,
  {Fields, Vals}           = aux:split_tuple_list(To_and_con, [], []),
  To_name                  = aux:read_val_by_id(to, Fields, Vals),
  Con                      = aux:read_val_by_id(constraint, Fields, Vals),
  To                       = aux:read_v(To_name, Name_list, Val_list),
  mmods:add_dependency(From, To, aux:token(Con)),
  add_dependency_helper(From, Dependencies, Name_list, Val_list).

loop_request(0, _, _, _) ->
  ok;
loop_request(C, Entities, Name_list, Val_list) ->
  {ent, Ent}     = aux:read_at_index(C, Entities),
  {Fields, Vals} = aux:split_tuple_list(Ent, [], []),  
  Name           = aux:read_val_by_id(name, Fields, Vals),
  Requests       = aux:read_val_by_id(requests, Fields, Vals),
  From           = aux:read_v(Name, Name_list, Val_list),
  ok             = request_helper(From, Requests, Name_list, Val_list),
  loop_request(C - 1, Entities, Name_list, Val_list).

request_helper(_, [], _, _) ->
  ok;
request_helper(From, [R_head|Requests], Name_list, Val_list) ->
  {request, Req} = R_head,
  {Fields, Vals} = aux:split_tuple_list(Req, [], []),
  To_name        = aux:read_val_by_id(to, Fields, Vals),
  Data           = aux:read_val_by_id(data, Fields, Vals),
  Ans_t          = aux:read_val_by_id(answers, Fields, Vals),
  To             = aux:read_v(To_name, Name_list, Val_list),
  {_, Ans}       = aux:split_tuple_list(Ans_t, [], []),
  mmods:request_info(From, To, Data, Ans),
  request_helper(From, Requests, Name_list, Val_list).
