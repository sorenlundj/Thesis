-module(parser).

-include_lib("xmerl/include/xmerl.hrl"). 

-import(aux, [fst/1,
              snd/1,
              read_at_index/2]).

-export([ie/1, co_helper/0]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML parser                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ie(File) ->
  AccFun = fun(#xmlText{value=V} = _, Acc, GS) -> 
               case re:run(V, "^\\s*$") of
                 {match, _} -> {Acc, GS}; 
                 nomatch    -> {[V | Acc], GS} 
               end; 
             (E,Acc,GS) -> 
               {[E|Acc], GS} 
           end,
  HookFun = fun(#xmlElement{name=Name, content=[Cont]}, GS) 
                  when is_list(Cont) -> 
                    {{Name, Cont}, GS}; 
               (#xmlElement{name=Name, content=Cont}, GS) -> 
                    {{Name, Cont}, GS}; 
               (E, GS) -> 
                    {E, GS} 
            end,
  {Xml, _} = xmerl_scan:file(File, [{acc_fun, AccFun}, {hook_fun, HookFun}]),
  Xml.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary/tmp functions                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


loop_reqs([], Result) ->
  Result;
loop_reqs([Head|Requests], Result) ->
  H    = aux:snd(Head),
  To   = aux:snd(aux:read_at_index(1, H)),
  Info = aux:snd(aux:read_at_index(2, H)),
  Ans  = loop_info(aux:snd(aux:read_at_index(3, H)), []),
  loop_reqs(Requests, [[To, Info, Ans]] ++ Result).

loop_info([], Result) ->
  Result;
loop_info([Head|Information], Result) ->
  loop_info(Information, [aux:snd(Head)] ++ Result).

loop_deps([], Result) ->
  Result;
loop_deps([Head|Dependencies], Result) ->
  To  = aux:snd(aux:read_at_index(1, aux:snd(Head))),
  Con = aux:snd(aux:read_at_index(2, aux:snd(Head))),
  loop_deps(Dependencies, [{To, Con}] ++ Result).

loop_rels([], Result) ->
  Result;
loop_rels([Head|Relations], Result) ->
  loop_rels(Relations, [aux:snd(Head)] ++ Result).

loop_ents([], Result) ->
  Result;
loop_ents([Head|Ent_list], Result) ->
  {ent, Prop_list} = Head,
  [Type, Name, Relations, Dependencies, Information, Requests] = Prop_list,
  Obj_pair = {aux:snd(Type), aux:snd(Name)},
  Obj_rels = loop_rels(aux:snd(Relations), []),
  Obj_deps = loop_deps(aux:snd(Dependencies), []),
  Obj_info = loop_info(aux:snd(Information), []),
  Obj_reqs = loop_reqs(aux:snd(Requests), []),
  Entity   = [Obj_pair, Obj_rels, Obj_info, Obj_deps, Obj_reqs],
  loop_ents(Ent_list, [Entity] ++ Result).

create_objects(Entities) ->
  {entities, Ent_list} = Entities,
  loop_ents(Ent_list, []).

co_helper() ->
  Entities = ie('test.xml'),
  Ret = create_objects(Entities),
  Ret.



