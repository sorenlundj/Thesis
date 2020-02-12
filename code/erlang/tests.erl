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

-import(aux, [read_at_index/2,
              write_v/4,
              read_v/3,
              trivial/1,
              user/1,
              psswd/1,
              get_all_states_wrapper/1]).

-compile(export_all).
-compile(nowarn_export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Base-level tests: Function-specific                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Start

start_ship() ->
  {ok, Entity} = mmods:start(ship),
  mmods:get_type(Entity) == ship.

start_service() ->
  {ok, Entity} = mmods:start(service),
  mmods:get_type(Entity) == service.

start_company() ->
  {ok, Entity} = mmods:start(company),
  mmods:get_type(Entity) == company.

start_error() ->
  mmods:start(something_different) == {error, not_an_entity}.

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
  {A, B, C} == {[{Service, []}], [{Company, []}, {Ship, []}], [{Service, []}]}.

add_relation_illegal() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Company} = mmods:start(company),
  {error, illegal_relation} = mmods:add_relation(Ship, Company),
  {error, illegal_relation} = mmods:add_relation(Company, Ship),
  A             = mmods:get_relations(Ship),
  B             = mmods:get_relations(Company),
  {A, B} == {[], []}.

add_relation_existing() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {error, relation_already_exists} = mmods:add_relation(Ship, Service),
  A             = mmods:get_relations(Ship),
  A == [{Service, []}].

%%%%% Add_dependency

add_dependency_legal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service), 
  ok = mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  [{Service, [fun aux:trivial/1]}] == mmods:get_relations(Ship).

add_dependency_illegal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  [] == mmods:get_relations(Ship).

%%%%% Add_info

add_info_multiple() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, b),
  ok         = mmods:add_info(Ship, c),
  ok         = mmods:add_info(Ship, d),
  [d, c, b, a] == mmods:get_info(Ship).

add_info_existing() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:add_info(Ship, a),
  [a] == mmods:get_info(Ship).

%%%%% Remove_info

remove_info_once() ->
  {ok, Ship} = mmods:start(ship),
  ok         = mmods:add_info(Ship, a),
  ok         = mmods:remove_info(Ship, a),
  [] == mmods:get_info(Ship).

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
  [a] == mmods:get_info(Ship).

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
  {A, B} == {[b], [a]}.

request_info_deps() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  ok            = mmods:add_info(Service, a),
  ok            = mmods:add_info(Service, b),
  {ok, Ship}    = mmods:request_info(Ship, Service, a, [answer]),
  A             = mmods:get_info(Service),
  B             = mmods:get_info(Ship),
  {A, B} == {[b], [a]}.

request_info_non_existent() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  ok            = mmods:add_info(Service, a),
  {ok, []}      = mmods:request_info(Ship, Service, b, []),
  A             = mmods:get_info(Service),
  B             = mmods:get_info(Ship),
  {A, B} == {[a], []}.

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
  TRSI == {ship,[{Serv,[dep_placeholder]}], Ship, [oh_wow]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wrapper functions: Function-specific                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  true = start_ship(),
  true = start_service(),
  true = start_company(),
  true = start_error().

add_relation() ->
  true = add_relation_legal(),
  true = add_relation_illegal(),
  true = add_relation_existing().

add_dependency() ->
  true = add_dependency_legal(),
  true = add_dependency_illegal().

add_info() ->
  true = add_info_multiple(),
  true = add_info_existing().

remove_info() ->
  true = remove_info_once(),
  true = remove_info_multiple().

request_info() ->
  true = request_info_no_deps(),
  true = request_info_deps(),
  true = request_info_non_existent().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Master-wrapper function: Function-specific                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_funcs() ->
  true = start(),
  true = add_relation(),
  true = add_dependency(),
  true = add_info(),
  true = remove_info(),
  true = request_info(),
  true = get_all().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scenario tests                                                    %
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
  [this, is, correct] == mmods:get_info(Ship).

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

  [this,is,correct] == mmods:get_info(Ship).

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

  [c] == mmods:get_info(Ship).

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
  [] == mmods:get_info(Shp1) andalso [] == mmods:get_info(Shp2).

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

  [] == mmods:get_info(Shp1) andalso [] == mmods:get_info(Shp2).

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

  [a] == mmods:get_info(Shp1) andalso [a] == mmods:get_info(Shp2).

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

  [a_steak] == mmods:get_info(Shp1) andalso [] == mmods:get_info(Shp2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scenario tests: wrapper                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_scenarios() ->
  true = multiple_coms(),
  true = multiple_serv([1,2,3]),
  true = multiple_serv([1,3,2]),
  true = multiple_serv([2,1,3]),
  true = multiple_serv([2,3,1]),
  true = multiple_serv([3,2,1]),
  true = multiple_serv([3,1,2]),
  true = service_choose_company(),
  true = bad_connection_from(),
  true = bad_connection_to(),
  true = ok_connection_two_ships(),
  true = first_come_first_served().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The one wrapper to rule them all                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
  true = main_funcs(),
  true = main_scenarios().

%%%%% Other

test_write_read()->
  NL = [],
  VL = [],
  {NL1, VL1} = aux:write_v("Sh", "ship", NL, VL),
  {NL2, VL2} = aux:write_v("Se", "service", NL1, VL1),
  {NL3, VL3} = aux:write_v("Co", "company", NL2, VL2),
  A = mmods:get_type(aux:read_v("Sh", NL3, VL3)),
  B = mmods:get_type(aux:read_v("Se", NL3, VL3)),
  C = mmods:get_type(aux:read_v("Co", NL3, VL3)),
  {A, B, C} == {ship, service, company}.

