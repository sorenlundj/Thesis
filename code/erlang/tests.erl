-module(tests).

-import(mmods, [start/1,
                add_relation/2,
                get_state/1,
                get_type/1,
                get_relations/1,
                add_dependency/3,
                add_info/2]).

-compile(export_all).
-compile(nowarn_export_all).

main() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service} = mmods:start(service),
  {ok, Company} = mmods:start(company),
  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  {ok, Service} = mmods:add_relation(Service, Ship),
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, test1}   = mmods:add_dependency(Ship, Service, test1),
  {ok, test2}   = mmods:add_dependency(Ship, Service, test2),
  {ok, test3}   = mmods:add_dependency(Ship, Service, test3),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  mmods:add_info(Company, map),
  A             = mmods:get_state(Ship),
  B             = mmods:get_state(Service),
  C             = mmods:get_state(Company),
  {A, B, C}.

t2() ->
  {ok, Ship1} = mmods:start(ship),
  {ok, Ship2} = mmods:start(ship),
  A = mmods:get_state(Ship1),
  B = mmods:get_state(Ship2),
  {A, B}.

deps_true() ->
  {ok, Ship}    = mmods:start(ship),
  {ok, Service1} = mmods:start(service),
  {ok, Service2} = mmods:start(service),
  {ok, Service3} = mmods:start(service),
  {ok, Service4} = mmods:start(service),
  {ok, Ship}    = mmods:add_relation(Ship, Service1),
  {ok, Ship}    = mmods:add_relation(Ship, Service2),
  {ok, Ship}    = mmods:add_relation(Ship, Service3),
  {ok, Ship}    = mmods:add_relation(Ship, Service4),
  {ok, service1}   = mmods:add_dependency(Ship, Service1, service1),
  {ok, service2}   = mmods:add_dependency(Ship, Service2, service2),
  {ok, service3}   = mmods:add_dependency(Ship, Service3, service3),
  {ok, service4}   = mmods:add_dependency(Ship, Service4, service4),
  mmods:get_state(Ship).


