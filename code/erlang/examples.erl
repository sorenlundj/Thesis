-module(examples).

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

-import(aux, [get_all_states_wrapper/1]).

-export([protocol/0]).

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

  aux:get_all_states_wrapper([Ship, Service, Company]).