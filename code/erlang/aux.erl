-module(aux).


-import(lists, [nth/2,
                member/2]).

-import(string, [str/2]).

-export([%Shared auxiliary functions
         index_of/3,
         insert_at_index/3,
         read_at_index/2,
         %mmods auxiliary functions
         legal_relation/2,
         relation_exists/2,
         dep_to_rel_list/4,
         answers_to_deps/2,
         get_dep_list/2,
         extract_rels/2,
         delete_at_index/2,
         filter_rels/3,
         request_loop/4,
         %parser auxiliary functions
         to_atom/1,
         write_v/4,
         read_v/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shared auxiliary functions for modules 'mmods' and 'parser'        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

index_of(Element, List, ZerOne) ->
  string:str(List, [Element]) - ZerOne. % - ZerOne

insert_at_index(Element, List, Index) ->
  {Left, Right} = lists:split(Index, List),
  Left ++ [Element|Right].

read_at_index(Index, List) ->
  lists:nth(Index, List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions for module 'mmods'                            %
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

delete_at_index(I, Items) ->
  {Left, [_|Right]} = lists:split(I, Items),
  Left ++ Right.

filter_rels(_, [], Res) ->
  Res;
filter_rels(Type, [Id|Ids], Res) ->
  case Type == mmods:get_type(Id) of
    true  -> filter_rels(Type, Ids, [Id] ++ Res);
    false -> filter_rels(Type, Ids, Res)
  end.

request_loop(_, [], _, _) ->
  ok;
request_loop(From, [Head|To], Info, Answers) ->
  mmods:request_info(From, Head, Info, Answers),
  request_loop(From, To, Info, Answers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions for module 'parser'                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: DO THIS FOR FUNCTIONS!!!!!

to_atom(String) ->
  case String of
    "ship"    -> ship;
    "service" -> service;
    "company" -> company;
    _         -> error
  end.

write_v(S_name, S_atom, Name_list, Val_list) ->
  case Name_list == [] andalso Val_list == [] of
    true  -> 
      Atom      = to_atom(S_atom),
      {ok, Val} = mmods:start(Atom),
      {[S_name], [Val]};
    false ->
      case member(S_name, Name_list) of
        true  -> {error, bound_variable};
        false ->
          Atom          = to_atom(S_atom),
          {ok, Val}     = mmods:start(Atom),
          Index         = index_of(S_name, Name_list, 0),
          Upd_name_list = insert_at_index(S_name, Name_list, Index),
          Upd_val_list  = insert_at_index(Val, Val_list, Index),
          {Upd_name_list, Upd_val_list}
      end
    end.

read_v(S_name, Name_list, Val_list) ->
  case member(S_name, Name_list) of
    true  -> 
      Index = index_of(S_name, Name_list, 0),
      read_at_index(Index, Val_list);
    false -> {error, unbound_variable}
  end.
