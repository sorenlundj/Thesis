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

-export([protocol/0,
         empty_entity/0,
         multiple_services/0,
         non_connected_ents/0]).

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

empty_entity() ->
  {ok, Ship}    = mmods:start(ship),
  aux:get_all_states_wrapper([Ship]).

multiple_services() ->
  {ok, Ship}     = mmods:start(ship),
  {ok, Service1} = mmods:start(service),
  {ok, Service2} = mmods:start(service),
  {ok, Service3} = mmods:start(service),

  {ok, Ship}     = mmods:add_relation(Ship, Service1),
  {ok, Ship}     = mmods:add_relation(Ship, Service2),
  {ok, Ship}     = mmods:add_relation(Ship, Service3),

  {ok, Service1} = mmods:add_relation(Service1, Ship),
  {ok, Service2} = mmods:add_relation(Service2, Ship),
  {ok, Service3} = mmods:add_relation(Service3, Ship),

  ok            = mmods:add_info(Ship, "Ship info"),
  ok            = mmods:add_info(Service1, "S1 info"),
  ok            = mmods:add_info(Service2, "S2 info"),
  ok            = mmods:add_info(Service3, "S3 info"),

  aux:get_all_states_wrapper([Ship, Service1, Service2, Service3]).

non_connected_ents() ->
  {ok, Ship1}    = mmods:start(ship),
  {ok, Ship2}    = mmods:start(ship),
  {ok, Service1} = mmods:start(service),
  {ok, Service2} = mmods:start(service),
  {ok, Company1} = mmods:start(company),
  {ok, Company2} = mmods:start(company),

  aux:get_all_states_wrapper([Ship1 ,Ship2 ,Service1 ,Service2 ,Company1 ,Company2]).
