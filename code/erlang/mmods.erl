-module(mmods).

-behaviour(gen_statem).

-export([start/0, add_relation/2, get_state/1]).

%-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([init/1, callback_mode/0, terminate/3, code_change/4, mmods_handler/3]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type name() :: pid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> {ok, name() | error, any()}.
start() ->
  gen_statem:start_link(?MODULE, [], []).

add_relation(From, To) ->
	gen_statem:call(From, {add_relation_call, To}).

%add_dependency(From, To, Dependency) ->
%	gen_statem:call(From, {add_dependency_call, To, Dependency}).

get_state(Id) ->
	gen_statem:call(Id, {get_state_call, none}).

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
%	case Function_name of
%		add_relation_call ->
%			{To, Dependency} = Request, 
%
%
%	end.


mmods_handler({call, From}, {Function_name, Request}, {Type, Relations, Self}=State) ->
	case Function_name of
		add_relation_call -> %check if type matches here
			To = Request, 
			{keep_state, {Type, [To|Relations], Self}, [{reply, From, {ok, Self}}]};
%		add_dependency_call -> % Check if relation exists here
%			{keep_state, {Type, }}
%		get_state_call ->
%			{keep_state, State, [{reply, From, State}]};
		get_state_call ->
      {keep_state, State, [{reply, From, State}]};
		_ ->
			{keep_state, State, [{reply, From, {error, unhandled_case}}]}
		end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Non-op-level functions                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
