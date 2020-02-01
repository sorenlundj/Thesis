-module(mmods).

-behaviour(gen_statem).

-import(lists, [delete/2, member/2]).

-import(aux, [relation_exists/2,
              dep_to_rel_list/4,
              legal_relation/2,
              get_dep_list/2,
              answers_to_deps/2,
              extract_rels/2,
              delete_at_index/2,
              filter_rels/3,
              index_of/3,
              request_loop/4]).

%%% Model-builder functions  %%%
-export([start/1, add_relation/2, add_dependency/3, add_info/2, remove_info/2, transfer_info/3]).

%%% Getter functions         %%%
-export([get_state/1, get_type/1, get_relations/1, get_info/1]).

%%% Simulation functions     %%%
-export([request_info/4]).

-export([init/1, callback_mode/0, terminate/3, code_change/4, mmods_handler/3]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
 gen_statem:cast(From, {add_dependency_call, {To, Dependency}}).

add_info(Id, Info) ->
  gen_statem:cast(Id, {add_info_call, Info}).

remove_info(Id, Info) ->
  gen_statem:cast(Id, {remove_info_call, Info}).

transfer_info(From, To, Info) ->
  gen_statem:call(From, {transfer_info_call, {To, Info}}).

request_info(From, To, Info, Answers) -> 
  gen_statem:call(From, {request_info_call, {To, Info, Answers}}).

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
      end;
    add_dependency_call ->
      {To, Dependency} = Request,
      case aux:relation_exists(To, Relations) of
        true  ->
          Updated_relations = aux:dep_to_rel_list(To, Dependency, Relations, []),
          case Updated_relations of 
            error ->
              {keep_state, State};
            _ ->
              {keep_state, {Type, Updated_relations, Self, Info}}
          end;
        false ->
          {keep_state, State}
      end
  end;
mmods_handler({call, From}, {Function_name, Request}, {Type, Relations, Self, Info}=State) ->
  case Function_name of
    add_relation_call ->
      To = Request,
      case aux:legal_relation(Type, get_type(To)) of
        true  -> 
          {keep_state, {Type, [{To, []}|Relations], Self, Info}, [{reply, From, {ok, Self}}]};
        false ->
          {keep_state, State, [{reply, From, {error, illegal_relation}}]}
      end;
    transfer_info_call ->
      {To, Transfer_info} = Request,
      case lists:member(Transfer_info, Info) of
        true ->
          case aux:relation_exists(To, Relations) of
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
    request_info_call ->
      {To, Req_info, Answers} = Request,
      Dep_list = aux:get_dep_list(To, Relations),
      case aux:answers_to_deps(Answers, Dep_list) of
        true  ->
          To_info = get_info(To),
          case lists:member(Req_info, To_info) of
            true ->
              transfer_info(To, Self, Req_info),
              {keep_state, State, [{reply, From, {ok, Self}}]};
            false ->
              Nb_ids  = aux:extract_rels(get_relations(To), []),
              New_ids = aux:delete_at_index(aux:index_of(Self, Nb_ids, 1), Nb_ids),
              Rels    = aux:filter_rels(company, New_ids, []),
              aux:request_loop(To, Rels, Req_info,[]),
              transfer_info(To, Self, Req_info),
              {keep_state, State, [{reply, From, {ok, Info}}]}    
          end;
        false ->
          {keep_state, State, [{reply, From, {error, incorrect_answers}}]}
      end;
    get_state_call     -> {keep_state, State, [{reply, From, State}]};
    get_type_call      -> {keep_state, State, [{reply, From, Type}]};
    get_relations_call -> {keep_state, State, [{reply, From, Relations}]};
    get_info_call      -> {keep_state, State, [{reply, From, Info}]};
    _                  -> {keep_state, State, [{reply, From, {error, unhandled_case}}]}
  end.
