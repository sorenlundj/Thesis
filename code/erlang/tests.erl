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
                get_info/1,
                request_info/4]).

-import(aux, [write_v/4,
              read_v/3,
              trivial/1,
              user/1,
              psswd/1]).

-import(lists, [all/2]).

-compile(export_all).
-compile(nowarn_export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Base-level tests                                                  %
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
  {error, relation_already_exists} = mmods:add_relation(Ship, Service),
  {error, relation_already_exists} = mmods:add_relation(Ship, Service),
  {error, relation_already_exists} = mmods:add_relation(Ship, Service),
  A = mmods:get_relations(Ship),
  A == [{Service, []}].

%%%%% Add_dependency

add_dependency_legal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service), 
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  [{Service, [fun aux:trivial/1]}] == mmods:get_relations(Ship).

add_dependency_illegal() ->  
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  [] == mmods:get_relations(Ship).

%%%%% Add_info

add_info_multiple() ->
  {ok, Ship} = mmods:start(ship),
  mmods:add_info(Ship, a),
  mmods:add_info(Ship, b),
  mmods:add_info(Ship, c),
  mmods:add_info(Ship, d),
  [d, c, b, a] == mmods:get_info(Ship).

add_info_existing() ->
  {ok, Ship} = mmods:start(ship),
  mmods:add_info(Ship, a),
  mmods:add_info(Ship, a),
  mmods:add_info(Ship, a),
  [a] == mmods:get_info(Ship).

%%%%% Remove_info

remove_info_once() ->
  {ok, Ship} = mmods:start(ship),
  mmods:add_info(Ship, a),
  mmods:remove_info(Ship, a),
  [] == mmods:get_info(Ship).

remove_info_multiple() ->
  {ok, Ship} = mmods:start(ship),
  mmods:add_info(Ship, a),
  mmods:add_info(Ship, b),
  mmods:add_info(Ship, c),
  mmods:add_info(Ship, d),
  [d, c, b, a] = mmods:get_info(Ship),
  mmods:remove_info(Ship, b),
  mmods:remove_info(Ship, c),
  mmods:remove_info(Ship, d),
  [a] == mmods:get_info(Ship).

%%%%% Request_info

request_info_no_deps() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  mmods:add_info(Service, a),
  mmods:add_info(Service, b),
  mmods:request_info(Ship, Service, a, []),
  A = mmods:get_info(Service),
  B = mmods:get_info(Ship),
  {A, B} == {[b], [a]}.

request_info_deps() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  mmods:add_info(Service, a),
  mmods:add_info(Service, b),
  mmods:request_info(Ship, Service, a, [answer]),
  A = mmods:get_info(Service),
  B = mmods:get_info(Ship),
  {A, B} == {[b], [a]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wrapper functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  A = start_ship(),
  B = start_service(),
  C = start_company(),
  D = start_error(),
  lists:all(fun (X) -> X == true end, [A, B, C, D]).

add_relation() ->
  A = add_relation_legal(),
  B = add_relation_illegal(),
  C = add_relation_existing(),
  lists:all(fun (X) -> X == true end, [A, B, C]).

add_dependency() ->
  A = add_dependency_legal(),
  B = add_dependency_illegal(),
  lists:all(fun (X) -> X == true end, [A, B]).

add_info() ->
  A = add_info_multiple(),
  B = add_info_existing(),
  lists:all(fun (X) -> X == true end, [A, B]).

remove_info() ->
  A = remove_info_once(),
  B = remove_info_multiple(),
  lists:all(fun (X) -> X == true end, [A, B]).

request_info() ->
  A = request_info_no_deps(),
  B = request_info_deps(),
  lists:all(fun (X) -> X == true end, [A, B]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Master-wrapper function                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
  A = start(),
  B = add_relation(),
  C = add_dependency(),
  D = add_info(),
  E = remove_info(),
  F = request_info(),
  lists:all(fun (X) -> X == true end, [A, B, C, D, E, F]).

% Other

test_write_read()->
  NL = [],
  VL = [],
  {NL1, VL1} = aux:write_v("Ship", "ship", NL, VL),
  {NL2, VL2} = aux:write_v("Service", "service", NL1, VL1),
  {NL3, VL3} = aux:write_v("Company", "company", NL2, VL2),
  A = mmods:get_type(aux:read_v("Ship", NL3, VL3)),
  B = mmods:get_type(aux:read_v("Service", NL3, VL3)),
  C = mmods:get_type(aux:read_v("Company", NL3, VL3)),
  {A, B, C} == {ship, service, company}.

