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

user(X) -> X == anton.

psswd(X) -> X == 1234.

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
  ok             = mmods:add_dependency(Ship, Service1, trivial),
  ok             = mmods:add_dependency(Ship, Service2, trivial),
  ok             = mmods:add_dependency(Ship, Service2, trivial),
  ok             = mmods:add_dependency(Ship, Service2, trivial),
  ok             = mmods:add_dependency(Ship, Service3, trivial),
  ok             = mmods:add_dependency(Ship, Service4, trivial),
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

  ok            = mmods:add_info(Company, map),

  {ok, Ship}    = mmods:add_relation(Ship, Service),
  {ok, Service} = mmods:add_relation(Service, Company),
  
  {ok, Company} = mmods:add_relation(Company, Service),
  {ok, Service} = mmods:add_relation(Service, Ship),

  ok            = mmods:add_dependency(Ship, Service, fun psswd/1),
  ok            = mmods:add_dependency(Ship, Service, fun user/1),

  request_info(Ship, Service, map, [anton, 1234]),

  A = mmods:get_info(Ship)    == [map],
  B = mmods:get_info(Service) == [],
  C = mmods:get_info(Company) == [map],
  {A, B, C}.


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




