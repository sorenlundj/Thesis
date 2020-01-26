-module(mmods).

-behaviour(gen_statem).

-import(lists, [delete/2, member/2]).

-export([start/1, add_relation/2, add_dependency/3, add_info/2, remove_info/2, transfer_info/3]).
-export([get_state/1, get_type/1, get_relations/1, get_info/1]).

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

add_info(Id, Info) ->
  gen_statem:cast(Id, {add_info_call, Info}).

remove_info(Id, Info) ->
  gen_statem:cast(Id, {remove_info_call, Info}).

transfer_info(From, To, Info) ->
  gen_statem:call(From, {transfer_info_call, {To, Info}}).

get_state(Id) ->
  gen_statem:call(Id, {get_state_call, none}).

get_type(Id) ->
  gen_statem:call(Id, {get_type_call, none}).

get_relations(Id) ->
  gen_statem:call(Id, {get_relations_call, none}).

get_info(Id) ->
  gen_statem:call(Id, {get_info_call, none}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback functions                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Type)->
  {ok, mmods_handler, {Type, [], self(), []}}.

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

mmods_handler(cast, {Function_name, Request}, {Type, Relations, Self, Info}=State) ->
  case Function_name of
    add_info_call ->
      Add_info = Request,
      {keep_state, {Type, Relations, Self, [Add_info|Info]}};
    remove_info_call ->
      Remove_info = Request,
      case lists:member(Remove_info, Info) of
        true ->
          {keep_state, {Type, Relations, Self, lists:delete(Remove_info, Info)}};
        false ->
          {keep_state, State}
      end
  end;
mmods_handler({call, From}, {Function_name, Request}, {Type, Relations, Self, Info}=State) ->
  case Function_name of
    add_relation_call ->
      To = Request,
      case legal_relation(Type, get_type(To)) of
        true  -> 
          {keep_state, {Type, [{To, []}|Relations], Self, Info}, [{reply, From, {ok, Self}}]};
        false ->
          {keep_state, State, [{reply, From, {error, illegal_relation}}]}
      end;
    add_dependency_call ->
      {To, Dependency} = Request,
      case relation_exists(To, Relations) of
        true  ->
          Updated_relations = dep_to_rel_list(To, Dependency, Relations, []),
          case Updated_relations of 
            error ->
              {keep_state, State, [{reply, From, {error, bug_in_relation_list}}]};
            _ ->
            {keep_state, {Type, Updated_relations, Self, Info}, [{reply, From, {ok, Dependency}}]}
          end;
        false ->
          {keep_state, State, [{reply, From, {error, nonexistent_relation}}]}
      end;
    transfer_info_call ->
      {To, Transfer_info} = Request,
      case lists:member(Transfer_info, Info) of
        true ->
          case relation_exists(To, Relations) of
            true ->
              case Type of
                ship ->
                  {keep_state, State, [{reply, From, {error, cannot_transfer_data_from_ship}}]};
                service ->
                  add_info(To, Transfer_info),
                  remove_info(Self, Transfer_info),
                  {keep_state, State, [{reply, From, {ok, Transfer_info}}]};
                company ->
                  add_info(To, Transfer_info),
                  {keep_state, State, [{reply, From, {ok, Transfer_info}}]}
              end;                  
            false ->
              {keep_state, State, [{reply, From, {error, nonexistent_relation}}]}
          end;
        false ->
          {keep_state, State, [{reply, From, {error, nonexistent_information}}]}
      end;
    get_state_call     -> {keep_state, State, [{reply, From, State}]};
    get_type_call      -> {keep_state, State, [{reply, From, Type}]};
    get_relations_call -> {keep_state, State, [{reply, From, Relations}]};
    get_info_call      -> {keep_state, State, [{reply, From, Info}]};
    _                  -> {keep_state, State, [{reply, From, {error, unhandled_case}}]}
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

dep_to_rel_list(_, _, [], _) ->
  error;
dep_to_rel_list(To, Dep, [{Rel_head, Dep_list}|Rel_list], Rest_list) ->
  case To == Rel_head of
    true  -> Rest_list ++ [{Rel_head, [Dep|Dep_list]}|Rel_list];
    false -> dep_to_rel_list(To, Dep, Rel_list, Rest_list ++ [{Rel_head, Dep_list}])
  end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODOS
% funktion 'request' (ship->service->company), der ser om der er en vej fra ship til company, og så reqer data
% add cases for dependencies
% 
% algo:
% 1: tjek kodeord
% 2: tjek om det reqquede atom er hvor der reqqes
% 3: hvis der reqqes til en service, tjek om det reqqede atom er i en nabo-company, og hvis det er muligt, overfør til service, derefter ship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




