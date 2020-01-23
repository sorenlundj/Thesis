%%% xmltest.erl
%%%
%%% @author Hank Wang <drapho@gmail.com>
%%%
%%% @doc simple sample to parse XML by xmerl
%%%

-module(parser).

-include_lib("xmerl/include/xmerl.hrl"). 

-export([main/0]).

main() ->
    Body = "
<Bookstore>
  <Book>
    <ISBN>9781401309657</ISBN>
    <Name>The Last Letcture</Name>
    <Author>Randy Pausch</Author>
  </Book>
</Bookstore>" ,

    {Xml, _} = xmerl_scan:string(Body),
    [val(xmerl_xpath:string("//ISBN", Xml)),
     val(xmerl_xpath:string("//Name", Xml)),
     val(xmerl_xpath:string("//Author", Xml))
    ].
    
val(X) ->
    [#xmlElement{name = N, content = [#xmlText{value = V}|_]}] = X,
    {N, V}.