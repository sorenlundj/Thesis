-module(mmods).

-behaviour(gen_statem).

-export([start/1, add_relation/2, get_state/1, get_type/1, get_relations/1, add_dependency/3]).

%-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([init/1, callback_mode/0, terminate/3, code_change/4, mmods_handler/3]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-type name() :: pid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-spec start(Type) -> {ok, name() | error, any()}.
start(Type) ->
  case Type of
    ship    -> gen_statem:start_link(?MODULE, Type, []);
    service -> gen_statem:start_link(?MODULE, Type, []);
    company -> gen_statem:start_link(?MODULE, Type, []);
    _       -> {error, not_an_entity}
  end.

add_relation(From, To) ->
  gen_statem:call(From, {add_relation_call, To}).

add_dependency(From, To, Dependency) ->
 gen_statem:call(From, {add_dependency_call, {To, Dependency}}).

get_state(Id) ->
  gen_statem:call(Id, {get_state_call, none}).

get_type(Id) ->
  gen_statem:call(Id, {get_type_call, none}).

get_relations(Id) ->
  gen_statem:call(Id, {get_relations_call, none}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback functions                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Type)->
  {ok, mmods_handler, {Type, [], self()}}.

callback_mode() ->
  state_functions.

terminate(_Reason, _State, _Data) ->
%  State = locked,
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Top-level function                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%mmods_handler(cast, {Function_name, Request}, {Type, Relations}=State) ->
% case Function_name of
%   add_relation_call ->
%     {To, Dependency} = Request, 
%
%
% end.


mmods_handler({call, From}, {Function_name, Request}, {Type, Relations, Self}=State) ->
  case Function_name of
    add_relation_call ->
      To = Request,
      case legal_relation(Type, get_type(To)) of
        true  -> 
          {keep_state, {Type, [{To, []}|Relations], Self}, [{reply, From, {ok, Self}}]};
        false ->
          {keep_state, State, [{reply, From, {error, illegal_relation}}]}
      end;
    add_dependency_call ->
      {To, Dependency} = Request,
      case relation_exists(To, Relations) of
        true  ->
          Updated_relations = dep_to_rel_list(Dependency, Relations),
          {keep_state, {Type, Updated_relations, Self}, [{reply, From, {ok, Dependency}}]};
        false ->
          {keep_state, State, [{reply, From, {error, relation_doesnt_exist}}]}
      end;
    get_state_call ->
      {keep_state, State, [{reply, From, State}]};
    get_type_call ->
      {keep_state, State, [{reply, From, Type}]};
    get_relations_call ->
      {keep_state, State, [{reply, From, Relations}]};
    _ ->
      {keep_state, State, [{reply, From, {error, unhandled_case}}]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Non-op-level functions                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

legal_relation(From, To) ->
  case {From, To} of
    {ship, company} -> false;
    {company, ship} -> false;
    _               -> true
  end.

relation_exists(_, []) ->
  false;
relation_exists(From, [{R, _}|Relations]) ->
  case From == R of
    true  -> true;
    false -> relation_exists(From, Relations)
  end.

dep_to_rel_list(Dep, [{Rel_head, Dep_list}|Rel_list]) ->
  [{Rel_head, [Dep|Dep_list]}|Rel_list].
  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODOS
% felt i state 'data' (her ligger kortet)
% funktion 'transfer data', der tjekker om der er en relation, og så gør den bare
% funktion 'request' (ship->service->company), der ser om der er en vej fra ship til company, og så reqer data

% på linje 94 er det Relations, der skal bruges (og ikke get_relations(to))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




