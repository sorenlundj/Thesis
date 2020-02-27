-module(parser).

-include_lib("xmerl/include/xmerl.hrl"). 

-export([file_parser/1,
         string_parser/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML parser                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_parser(File) ->
  parser(File, file).

string_parser(String) ->
  parser(String, string).

parser(File, Type) ->
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
  {Xml, _} = xmerl_scan:Type(File, [{acc_fun, AccFun}, {hook_fun, HookFun}]),
  Xml.

