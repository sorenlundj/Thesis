-module(tests).

-import(mmods, [start/1,
                add_relation/2,
                add_dependency/3,
                add_info/2,
                remove_info/2,
                transfer_info/3,
                get_state/1,
                get_type/1,
                get_relations/1,
                get_self/1,
                get_info/1,
                request_info/4]).

-import(aux, [read_val_by_id/3,
              split_tuple_list/3,
              read_at_index/2,
              write_v/4,
              read_v/3,
              trivial/1,
              user/1,
              psswd/1,
              get_all_states_wrapper/1]).

-import(main, [sparse/1,
               interpret/1,
               sparse_and_interpret/1,
               fparse_and_interpret/1]).

-import(examples, [protocol/0,
                   empty_entity/0,
                   multiple_services/0,
                   non_connected_ents/0]).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Contents:                                                         %
%   mmods tests:                                                    %
%     Function-specific tests                                       %
%       Base-level tests                                            %
%       Wrapper                                                     %
%       Master-wrapper                                              %
%                                                                   %
%     Scenario tests                                                %
%       Base-level tests                                            %
%       Wrapper                                                     %
%       Master-wrapper                                              %
%                                                                   %
%     Wrapper for all mmods tests                                   %
%                                                                   %
%   parser tests                                                    %
%                                                                   %
%   interpreter tests                                               %
%                                                                   %
%   Finite state machine comparer                                   %
%                                                                   %
%   other                                                           %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Base-level tests: Function-specific                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Start

start_ship() ->
  {ok, Entity} = mmods:start(ship),
  ?assertEqual(mmods:get_type(Entity), ship).

start_service() ->
  {ok, Entity} = mmods:start(service),
  ?assertEqual(mmods:get_type(Entity), service).

start_company() ->
  {ok, Entity} = mmods:start(company),
  ?assertEqual(mmods:get_type(Entity), company).

start_error() ->
  ?assertEqual(mmods:start(something_different), {error, not_an_entity}).

%%%%% Add_relation

add_relation_legal() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Company} = mmods:start(company),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  A             = mmods:get_relations(Ship),
  B             = mmods:get_relations(Service),
  C             = mmods:get_relations(Company),
  ?assertEqual({A, B, C}, {[{Service, []}], [{Company, []}, {Ship, []}], [{Service, []}]}).

add_relation_illegal() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Company} = mmods:start(company),
  {error, illegal_relation} = mmods:add_relation(Ship, Company),
  {error, illegal_relation} = mmods:add_relation(Company, Ship),
  A             = mmods:get_relations(Ship),
  B             = mmods:get_relations(Company),
  ?assertEqual({A, B}, {[], []}).

add_relation_existing() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {error, relation_already_exists} = mmods:add_relation(Ship, Service),
  A             = mmods:get_relations(Ship),
  ?assertEqual(A, [{Service, []}]).

%%%%% Add_dependency

add_dependency_legal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service), 
  ok = mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  ?assertEqual([{Service, [fun aux:trivial/1]}], mmods:get_relations(Ship)).

add_dependency_illegal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  ?assertEqual([], mmods:get_relations(Ship)).

%%%%% Add_info

add_info_multiple() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, b),
  ok         = mmods:add_info(Ship, c),
  ok         = mmods:add_info(Ship, d),
  ?assertEqual([d, c, b, a], mmods:get_info(Ship)).

add_info_existing() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, a),
  ?assertEqual([a], mmods:get_info(Ship)).

%%%%% Remove_info

remove_info_once() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:remove_info(Ship, a),
  ?assertEqual([], mmods:get_info(Ship)).

remove_info_multiple() ->
  {ok, Ship}   = mmods:start(ship),
  ok           = mmods:add_info(Ship, a),
  ok           = mmods:add_info(Ship, b),
  ok           = mmods:add_info(Ship, c),
  ok           = mmods:add_info(Ship, d),
  [d, c, b, a] = mmods:get_info(Ship),
  ok           = mmods:remove_info(Ship, b),
  ok           = mmods:remove_info(Ship, c),
  ok           = mmods:remove_info(Ship, d),
  ?assertEqual([a], mmods:get_info(Ship)).

%%%%% Request_info

request_info_no_deps() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_info(Service, a),
  ok            = mmods:add_info(Service, b),
  {ok, Ship}    = mmods:request_info(Ship, Service, a, []),
  A             = mmods:get_info(Service),
  B             = mmods:get_info(Ship),
  ?assertEqual({A, B}, {[b], [a]}).

request_info_deps() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_dependency(Ship, Service, fun (_) -> true end),
  ok            = mmods:add_info(Service, a),
  ok            = mmods:add_info(Service, b),
  {ok, Ship}    = mmods:request_info(Ship, Service, a, [answer]),
  A             = mmods:get_info(Service),
  B             = mmods:get_info(Ship),
  ?assertEqual({A, B}, {[b], [a]}).

request_info_non_existent() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_info(Service, a),
  {ok, []}      = mmods:request_info(Ship, Service, b, []),
  A             = mmods:get_info(Service),
  B             = mmods:get_info(Ship),
  ?assertEqual({A, B}, {[a], []}).

%%%%% Getter-functions

get_all() ->
  {ok, Ship}   = mmods:start(ship),
  {ok, Serv}   = mmods:start(service),
  {ok, Ship}   = mmods:add_relation(Ship, Serv),
  ok           = mmods:add_info(Ship, oh_wow),
  ok           = mmods:add_dependency(Ship, Serv, dep_placeholder),
  {T, R, S, I} = TRSI = mmods:get_state(Ship),
  T                   = mmods:get_type(Ship),
  R                   = mmods:get_relations(Ship),
  S                   = mmods:get_self(Ship),
  I                   = mmods:get_info(Ship),
  ?assertEqual(TRSI, {ship,[{Serv,[dep_placeholder]}], Ship, [oh_wow]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wrapper functions: Function-specific                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  start_ship(),
  start_service(),
  start_company(),
  start_error().

add_relation() ->
  add_relation_legal(),
  add_relation_illegal(),
  add_relation_existing().

add_dependency() ->
  add_dependency_legal(),
  add_dependency_illegal().

add_info() ->
  add_info_multiple(),
  add_info_existing().

remove_info() ->
  remove_info_once(),
  remove_info_multiple().

request_info() ->
  request_info_no_deps(),
  request_info_deps(),
  request_info_non_existent().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Master-wrapper function: Function-specific                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_funcs() ->
  start(),
  add_relation(),
  add_dependency(),
  add_info(),
  remove_info(),
  request_info(),
  get_all(),
  function_wrapper_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Base-level tests: Scenarios                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Multiple nodes

multiple_coms() ->
  {ok, Ship} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),
  {ok, Com1} = mmods:start(company),
  {ok, Com2} = mmods:start(company),
  {ok, Com3} = mmods:start(company),

  {ok, Ship} = mmods:add_relation(Ship, Serv),
  
  {ok, Serv} = mmods:add_relation(Serv, Ship),
  {ok, Serv} = mmods:add_relation(Serv, Com1),
  {ok, Serv} = mmods:add_relation(Serv, Com2),
  {ok, Serv} = mmods:add_relation(Serv, Com3),
  
  {ok, Com1} = mmods:add_relation(Com1, Serv),
  {ok, Com2} = mmods:add_relation(Com2, Serv),
  {ok, Com3} = mmods:add_relation(Com3, Serv),
  
  ok         = mmods:add_info(Com1, this),
  ok         = mmods:add_info(Com2, is),
  ok         = mmods:add_info(Com3, correct),
  
  {ok, []}   = mmods:request_info(Ship, Serv, correct, []),
  {ok, [correct]} = mmods:request_info(Ship, Serv, is, []),
  {ok, [is, correct]} = mmods:request_info(Ship, Serv, this, []),
  ?assertEqual([this, is, correct], mmods:get_info(Ship)).

multiple_serv(Order) ->
  {ok, Ship} = mmods:start(ship),
  {ok, Ser1} = mmods:start(service),
  {ok, Ser2} = mmods:start(service),
  {ok, Ser3} = mmods:start(service),
  {ok, Comp} = mmods:start(company),

  {ok, Ship} = mmods:add_relation(Ship, Ser1),
  {ok, Ship} = mmods:add_relation(Ship, Ser2),
  {ok, Ship} = mmods:add_relation(Ship, Ser3),

  {ok, Ser1} = mmods:add_relation(Ser1, Ship),
  {ok, Ser2} = mmods:add_relation(Ser2, Ship),
  {ok, Ser3} = mmods:add_relation(Ser3, Ship),

  {ok, Ser1} = mmods:add_relation(Ser1, Comp),
  {ok, Ser2} = mmods:add_relation(Ser2, Comp),
  {ok, Ser3} = mmods:add_relation(Ser3, Comp),

  {ok, Comp} = mmods:add_relation(Comp, Ser1),
  {ok, Comp} = mmods:add_relation(Comp, Ser2),
  {ok, Comp} = mmods:add_relation(Comp, Ser3),

  ok         = mmods:add_info(Comp, this),
  ok         = mmods:add_info(Comp, is),
  ok         = mmods:add_info(Comp, correct),

  req_it(Ship, Order, [Ser1, Ser2, Ser3], [correct, is, this], 1),

  ?assertEqual([this,is,correct], mmods:get_info(Ship)).

req_it(_, [], _, _, _) ->
  ok;
req_it(Reqqer, [Ind|Order], Servs, Info, C) ->
  mmods:request_info(Reqqer, aux:read_at_index(Ind, Servs), aux:read_at_index(C, Info), []),
  req_it(Reqqer, Order, Servs, Info, C + 1).


service_choose_company() ->
  {ok, Ship} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),
  {ok, Com1} = mmods:start(company),
  {ok, Com2} = mmods:start(company),
  {ok, Com3} = mmods:start(company),
  {ok, Com4} = mmods:start(company),
  {ok, Com5} = mmods:start(company),

  {ok, Ship} = mmods:add_relation(Ship, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Ship),

  {ok, Serv} = mmods:add_relation(Serv, Com1),
  {ok, Serv} = mmods:add_relation(Serv, Com2),
  {ok, Serv} = mmods:add_relation(Serv, Com3),
  {ok, Serv} = mmods:add_relation(Serv, Com4),
  {ok, Serv} = mmods:add_relation(Serv, Com5),

  {ok, Com1} = mmods:add_relation(Com1, Serv),
  {ok, Com2} = mmods:add_relation(Com2, Serv),
  {ok, Com3} = mmods:add_relation(Com3, Serv),
  {ok, Com4} = mmods:add_relation(Com4, Serv),
  {ok, Com5} = mmods:add_relation(Com5, Serv),

  ok         = mmods:add_info(Com1, a),
  ok         = mmods:add_info(Com2, b),
  ok         = mmods:add_info(Com3, c),
  ok         = mmods:add_info(Com4, d),
  ok         = mmods:add_info(Com5, e),

  {ok, []}   = mmods:request_info(Ship, Serv, c, []),

  ?assertEqual([c], mmods:get_info(Ship)).

bad_connection_from() ->
  {ok, Shp1} = mmods:start(ship),
  {ok, Shp2} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),
  {ok, Comp} = mmods:start(company),

  {ok, Shp1} = mmods:add_relation(Shp1, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp1),
  {ok, Shp2} = mmods:add_relation(Shp2, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp2),

  {ok, Serv} = mmods:add_relation(Serv, Comp),

  ok         = mmods:add_info(Comp, a),

  {ok, []}   = mmods:request_info(Shp1, Serv, a, []),
  {ok, []}   = mmods:request_info(Shp2, Serv, a, []),
  ?assertEqual([], mmods:get_info(Shp1)),
  ?assertEqual([], mmods:get_info(Shp2)).

bad_connection_to() ->
  {ok, Shp1} = mmods:start(ship),
  {ok, Shp2} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),
  {ok, Comp} = mmods:start(company),

  {ok, Shp1} = mmods:add_relation(Shp1, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp1),
  {ok, Shp2} = mmods:add_relation(Shp2, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp2),

  {ok, Comp} = mmods:add_relation(Comp, Serv),

  ok         = mmods:add_info(Comp, a),

  {ok, []}   = mmods:request_info(Shp1, Serv, a, []),
  {ok, []}   = mmods:request_info(Shp2, Serv, a, []),

  ?assertEqual([], mmods:get_info(Shp1)),
  ?assertEqual([], mmods:get_info(Shp2)).

ok_connection_two_ships() ->
  {ok, Shp1} = mmods:start(ship),
  {ok, Shp2} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),
  {ok, Comp} = mmods:start(company),

  {ok, Shp1} = mmods:add_relation(Shp1, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp1),
  {ok, Shp2} = mmods:add_relation(Shp2, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp2),

  {ok, Comp} = mmods:add_relation(Comp, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Comp),

  ok         = mmods:add_info(Comp, a),

  {ok, []}   = mmods:request_info(Shp1, Serv, a, []),
  {ok, []}   = mmods:request_info(Shp2, Serv, a, []),

  ?assertEqual([a], mmods:get_info(Shp1)),
  ?assertEqual([a], mmods:get_info(Shp2)).

first_come_first_served() ->
  {ok, Shp1} = mmods:start(ship),
  {ok, Shp2} = mmods:start(ship),
  {ok, Serv} = mmods:start(service),

  {ok, Shp1} = mmods:add_relation(Shp1, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp1),
  {ok, Shp2} = mmods:add_relation(Shp2, Serv),
  {ok, Serv} = mmods:add_relation(Serv, Shp2),

  ok         = mmods:add_info(Serv, a_steak),

  {ok, Shp1} = mmods:request_info(Shp1, Serv, a_steak, []),
  {ok, []}   = mmods:request_info(Shp2, Serv, a_steak, []),

  ?assertEqual([a_steak], mmods:get_info(Shp1)),
  ?assertEqual([], mmods:get_info(Shp2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scenario tests: wrapper                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_scenarios() ->
  multiple_coms(),
  multiple_serv([1,2,3]),
  multiple_serv([1,3,2]),
  multiple_serv([2,1,3]),
  multiple_serv([2,3,1]),
  multiple_serv([3,2,1]),
  multiple_serv([3,1,2]),
  service_choose_company(),
  bad_connection_from(),
  bad_connection_to(),
  ok_connection_two_ships(),
  first_come_first_served(),
  scenario_wrapper_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All mmods-tests-wrapper                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_mmods() ->
  main_funcs(),
  main_scenarios(),
  mmods_wrapper_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   parser tests                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_tests() ->
  Emp_tag        = "<a></a>",
  Num_tag        = "<a>1234</a>",
  Lis_tag        = "<a>[1,2,3,4]</a>",
  Let_tag        = "<a>abcde</a>",
  Mul_tag        = "<a><b>text</b><c>1234</c><d>[1,a,2,b]</d></a>",
  Sam_tag        = "<a><a></a></a>",
  Spa_tag        = "<a>1 2 3 4 5</a>",
  Tag_in_mul_tag = "<a><b><a>hello</a></b><c></c><d></d></a>",
  ?assertEqual(main:sparse(Emp_tag),{a,[]}),
  ?assertEqual(main:sparse(Num_tag),{a,"1234"}),
  ?assertEqual(main:sparse(Lis_tag),{a,"[1,2,3,4]"}),
  ?assertEqual(main:sparse(Let_tag),{a,"abcde"}),
  ?assertEqual(main:sparse(Mul_tag),{a,[{b,"text"},{c,"1234"},{d,"[1,a,2,b]"}]}),
  ?assertEqual(main:sparse(Sam_tag),{a,[{a,[]}]}),
  ?assertEqual(main:sparse(Spa_tag),{a,"1 2 3 4 5"}),
  ?assertEqual(main:sparse(Tag_in_mul_tag),{a,[{b,[{a,"hello"}]},{c,[]},{d,[]}]}),
  parse_tests_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   interpreter tests                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpreter_tests() ->
  No_entities = {entities, []},
  Model       = {entities,
    [{ent,
         [{type,"ship"},
          {name,"Ship"},
          {relations,[{relation,"Service"}]},
          {dependencies,
              [{dependency,[{to,"Service"},{constraint,"fun psswd"}]},
               {dependency,[{to,"Service"},{constraint,"fun user"}]}]},
          {information,[]},
          {requests,
              [{request,
                   [{to,"Service"},
                    {data,"map"},
                    {answers,[{answer,"anton"},{answer,"1234"}]}]}]}]},
     {ent,
         [{type,"service"},
          {name,"Service"},
          {relations,[{relation,"Ship"},{relation,"Company"}]},
          {dependencies,[]},
          {information,[]},
          {requests,[]}]},
     {ent,
         [{type,"company"},
          {name,"Company"},
          {relations,[{relation,"Service"}]},
          {dependencies,[]},
          {information,[{info,"map"}]},
          {requests,[]}]}]},
  [{_, _, CoId, _}, {_, _, SeId, _}, {_, _, ShId, _}] = main:interpret(Model),
  ?assertEqual(main:interpret(No_entities), []),
  [{CoIdRel, _}] = mmods:get_relations(CoId),
  [{SeIdRel1, _}, {SeIdRel2, _}] = mmods:get_relations(SeId),
  [{ShIdRel, _}] = mmods:get_relations(ShId),

  ?assertEqual(SeId, CoIdRel),
  ?assertEqual({CoId, ShId}, {SeIdRel1, SeIdRel2}),
  ?assertEqual(SeId, ShIdRel),
  interpreter_tests_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The one wrapper to rule them all                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
  main_mmods(),
  parse_tests(),
  interpreter_tests(),
  all_tests_passed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finite state machine comparer                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equiv_models_tests() ->
  PM = examples:protocol(),
  EM = examples:empty_entity(),
  MM = examples:multiple_services(),
  NM = examples:non_connected_ents(),

  PA = main:fparse_and_interpret('xml/protocol.xml'),
  EA = main:fparse_and_interpret('xml/empty_entity.xml'),
  MA = main:fparse_and_interpret('xml/multiple_services.xml'),
  NA = main:fparse_and_interpret('xml/non_connected_ents.xml'),

  ?assertEqual(equiv_models(PM, PM), no_inconsistencies_found),
  ?assertEqual(equiv_models(EM, EM), no_inconsistencies_found),
  ?assertEqual(equiv_models(MM, MM), no_inconsistencies_found),
  ?assertEqual(equiv_models(NM, NM), no_inconsistencies_found),

  ?assertEqual(equiv_models(PM, PA), no_inconsistencies_found),
  ?assertEqual(equiv_models(EM, EA), no_inconsistencies_found),
  ?assertEqual(equiv_models(MM, MA), no_inconsistencies_found),
  ?assertEqual(equiv_models(NM, NA), no_inconsistencies_found),

  ?assertEqual(equiv_models(PM, EM), error_length_mismatch),
  ?assertEqual(equiv_models(PM, MM), error_length_mismatch),
  ?assertEqual(equiv_models(PM, NM), error_length_mismatch),
  
  ?assertEqual(equiv_models(EM, PM), error_length_mismatch),
  ?assertEqual(equiv_models(EM, MM), error_length_mismatch),
  ?assertEqual(equiv_models(EM, NM), error_length_mismatch),

  ?assertEqual(equiv_models(MM, PM), error_length_mismatch),
  ?assertEqual(equiv_models(MM, EM), error_length_mismatch),
  ?assertEqual(equiv_models(MM, NM), error_length_mismatch),

  ?assertEqual(equiv_models(NM, PM), error_length_mismatch),
  ?assertEqual(equiv_models(NM, EM), error_length_mismatch),
  ?assertEqual(equiv_models(NM, MM), error_length_mismatch).

equiv_models(L_fsm, R_fsm) ->
  L_Length = length(L_fsm),
  R_Length = length(R_fsm),
  case L_Length == R_Length of
    true ->
      {L_ids, R_ids} = map_ids(L_fsm, R_fsm, [], []),
      case check_types(L_fsm, R_fsm) of
        true ->
          case check_relations(L_fsm, R_fsm, L_ids, R_ids) of
            true ->
              case check_info(L_fsm, R_fsm) of
                true ->
                  no_inconsistencies_found;
                false ->
                  error_info_mismatch
              end;
            false ->
              error_relation_mismatch
          end;
        false ->
          error_type_mismatch
      end;
    false ->
      error_length_mismatch
  end.

map_ids([], [], L_res, R_res) ->
  {L_res, R_res};
map_ids([L_head|L_fsm], [R_head|R_fsm], L_res, R_res) ->
  {_, _, L_Id, _} = L_head,
  {_, _, R_Id, _} = R_head,
  map_ids(L_fsm, R_fsm, L_res ++ [L_Id], R_res ++ [R_Id]).

check_types([], []) ->
  true;
check_types([L_head|L_fsm], [R_head|R_fsm]) ->
  {L_type, _, _, _} = L_head,
  {R_type, _, _, _} = R_head,
  case L_type == R_type of
    true ->
      check_types(L_fsm, R_fsm);
    false ->
      false
  end.

check_relations([],[], _, _) ->
  true;
check_relations([L_head|L_fsm], [R_head|R_fsm], L_ids, R_ids) ->
  {_, L_rels, _, _} = L_head,
  {_, R_rels, _, _} = R_head,
  {L_r, L_d} = aux:split_tuple_list(L_rels, [], []),
  {R_r, R_d} = aux:split_tuple_list(R_rels, [], []),
  case L_d == R_d of
    true ->
      case compare_relations(L_r, R_r, L_ids, R_ids) of
        true ->
          check_relations(L_fsm, R_fsm, L_ids, R_ids);
        false ->
          false
      end;
    false ->
      false
  end.

compare_relations([], [], _, _) ->
  true;
compare_relations([L_head|L_r], [R_head|R_r], L_ids, R_ids) ->
  R_test = aux:read_val_by_id(L_head, L_ids, R_ids),
  case R_test == R_head of
    true ->
      compare_relations(L_r, R_r, L_ids, R_ids);
    false ->
      false
  end.

check_info([], []) ->
  true;
check_info([L_head|L_fsm], [R_head|R_fsm]) ->
  {_, _, _, L_info} = L_head,
  {_, _, _, R_info} = R_head,
  case L_info == R_info of
    true ->
      check_info(L_fsm, R_fsm);
    false ->
      false
  end.

%%%%% other

test_write_read()->
  NL = [],
  VL = [],
  {NL1, VL1} = aux:write_v("Sh", "ship", NL, VL),
  {NL2, VL2} = aux:write_v("Se", "service", NL1, VL1),
  {NL3, VL3} = aux:write_v("Co", "company", NL2, VL2),
  A = mmods:get_type(aux:read_v("Sh", NL3, VL3)),
  B = mmods:get_type(aux:read_v("Se", NL3, VL3)),
  C = mmods:get_type(aux:read_v("Co", NL3, VL3)),
  ?assertEqual({A, B, C}, {ship, service, company}).

test_interp_anonymous_function() ->
  S ="<entities>
  <ent>
    <type>ship</type>
    <name>Ship</name>
    <relations>
      <relation>Service</relation>
    </relations>
    <dependencies>
      <dependency>
        <to>Service</to>
        <constraint>fun (_) -> true end</constraint>
      </dependency>
    </dependencies>
    <information></information>
    <requests></requests>
  </ent>
  <ent>
      <type>service</type>
      <name>Service</name>
      <relations>
        <relation>Ship</relation>
      </relations>
      <dependencies></dependencies>
      <information></information>
      <requests></requests>
  </ent>
</entities>",
  ?assertError({badmatch, error}, main:sparse_and_interpret(S), "Interpreter translates anonymous functions to error").
