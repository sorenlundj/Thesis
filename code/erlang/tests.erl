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

-import(aux, [write_v/4,
              read_v/3,
              trivial/1,
              user/1,
              psswd/1]).

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
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
  mmods:add_dependency(Ship, Service, test3),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  A             = mmods:get_state(Ship),
  B             = mmods:get_state(Service),
  C             = mmods:get_state(Company),
  {A, B, C}.

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
  ok             = mmods:add_dependency(Ship, Service1, fun aux:trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun aux:trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun aux:trivial/1),
  ok             = mmods:add_dependency(Ship, Service2, fun aux:trivial/1),
  ok             = mmods:add_dependency(Ship, Service3, fun aux:trivial/1),
  ok             = mmods:add_dependency(Ship, Service4, fun aux:trivial/1),
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
  mmods:add_dependency(Ship, Service, fun aux:trivial/1),
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

  ok            = mmods:add_dependency(Ship, Service, fun aux:psswd/1),
  ok            = mmods:add_dependency(Ship, Service, fun aux:user/1),

  request_info(Ship, Service, "map", ["anton", "1234"]),

  A = mmods:get_info(Ship)    == ["map"],
  B = mmods:get_info(Service) == [],
  C = mmods:get_info(Company) == ["map"],
  {A, B, C}.

test_wr()->
  NL = [],
  VL = [],
  {NL1, VL1} = aux:write_v("Ship", "ship", NL, VL),
  erlang:display({NL1, VL1}),
  {NL2, VL2} = aux:write_v("Service", "service", NL1, VL1),
  erlang:display({NL2, VL2}),
  {NL3, VL3} = aux:write_v("Company", "company", NL2, VL2),
  erlang:display({NL3, VL3}),
  erlang:display({ship,    aux:read_v("Ship", NL3, VL3)}),
  erlang:display({service, aux:read_v("Service", NL3, VL3)}),
  erlang:display({company, aux:read_v("Company", NL3, VL3)}).

