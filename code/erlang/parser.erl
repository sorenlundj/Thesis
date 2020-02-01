-module(parser).

-include_lib("xmerl/include/xmerl.hrl"). 

-export([xml_parser/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML parser                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_parser(File) ->
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

