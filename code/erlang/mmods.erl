-module(mmods).

-behaviour(gen_statem).

-import(lists, [delete/2, member/2, filter/2]).
-import(string, [str/2]).

%%% Model-builder functions
-export([start/1, add_relation/2, add_dependency/3, add_info/2, remove_info/2, transfer_info/3, request_info/4]).

%%% Getter-functions
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
      end;
    add_dependency_call ->
      {To, Dependency} = Request,
      case relation_exists(To, Relations) of
        true  ->
          Updated_relations = dep_to_rel_list(To, Dependency, Relations, []),
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
      case legal_relation(Type, get_type(To)) of
        true  -> 
          {keep_state, {Type, [{To, []}|Relations], Self, Info}, [{reply, From, {ok, Self}}]};
        false ->
          {keep_state, State, [{reply, From, {error, illegal_relation}}]}
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
    request_info_call ->
      {To, Req_info, Answers} = Request,
      Dep_list = get_dep_list(To, Relations),
      case answers_to_deps(Answers, Dep_list) of
        true  ->
          To_info = get_info(To),
          case lists:member(Req_info, To_info) of
            true ->
              transfer_info(To, Self, Req_info),
              {keep_state, State, [{reply, From, {ok, Self}}]};
            false ->
              Nb_ids  = extract_rels(get_relations(To), []),
              New_ids = delete_at_index(index_of(Self, Nb_ids), Nb_ids),
              Rels    = filter_rels(company, New_ids, []),
              request_loop(To, Rels, Req_info,[]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

legal_relation(From, To) ->
  case {From, To} of
    {ship, company} -> false;
    {ship, ship}    -> false;
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

answers_to_deps(_, []) ->
  true;
answers_to_deps([H_ans|Answers], [H_dep|Dependencies]) ->
  case H_dep(H_ans) of
    true  -> answers_to_deps(Answers, Dependencies);
    false -> false
  end.

get_dep_list(_, []) ->
  error;
get_dep_list(To, [{Rel_head, Dep_list}|Rel_list]) ->
  case To == Rel_head of
    true  -> Dep_list;
    false -> get_dep_list(To, Rel_list)
  end.

extract_rels([], Ids) ->
  Ids;
extract_rels([{Id, _}|Rels], Ids) ->
  extract_rels(Rels, [Id] ++ Ids).

filter_rels(_, [], Res) ->
  Res;
filter_rels(Type, [Id|Ids], Res) ->
  case Type == get_type(Id) of
    true  -> filter_rels(Type, Ids, [Id] ++ Res);
    false -> filter_rels(Type, Ids, Res)
  end.

delete_at_index(I, Items) ->
  {Left, [_|Right]} = lists:split(I, Items),
  Left ++ Right.

index_of(Element, List) ->
  string:str(List, [Element]) - 1.

request_loop(_, [], _, _) ->
  ok;
request_loop(From, [Head|To], Info, Answers) ->
  request_info(From, Head, Info, Answers),
  request_loop(From, To, Info, Answers).





