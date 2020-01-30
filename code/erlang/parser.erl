-module(parser).

-include_lib("xmerl/include/xmerl.hrl"). 

-import(lists, [member/2]).

-import(aux, [insert_at_index/3,
              read_at_index/2,
              to_atom/1,
              write_v/3,
              read_v/3]).

-export([main/1, m/0, get_entity/1, c_l_helper/0]).

-export([test_wr/0]).

m() -> 
Test = "
<entities>
    <ent> 
        <type>company</type>
        <name>Company</name>
        <relations>
            <relation>Service</relation>
        </relations>
        <dependencies> </dependencies>
        <information>
            <info>map</info>
        </information>
    </ent>
</entities>", %find mængde af tags med nedenstående, og gå iterativt op i niveau
{Xml, _} = xmerl_scan:string(Test),
_Layer_counter = count_subtags(Xml, "entities"),
Xml.

val(X) ->
    [#xmlElement{name = N, content = [#xmlText{value = V}|_]}] = X,
    {N, V}.

get_string(X) ->
  [#xmlElement{content = [#xmlText{value = V}|_]}] = X,
  V.

get_entity(File) ->
      {Xml, _} = xmerl_scan:string(File),
  Ent = [get_string(xmerl_xpath:string("//type", Xml))
        ,get_string(xmerl_xpath:string("//name", Xml))
        ,val(xmerl_xpath:string("//relations", Xml))
        ,val(xmerl_xpath:string("//dependencies", Xml))
        ,val(xmerl_xpath:string("//information", Xml))],
  Ent,
  [{_V0,%xmlElement
    _V1,%relations
    _V2,%relations
    _V3,%[]
    _V4,%{xmlNamespace,[],[]}
    _V5,%[{ent,2},{entities,1}]
    _V6,%6
    _V7,%[]
    _V8,%
    _V9,%[]
    _V10,%undefined
    _V11 %undeclared
    }] = xmerl_xpath:string("//information", Xml),
%  _V1 = get_multiple(xmerl_xpath:string("//relations", Xml)),
  (length(_V8) - 1) / 2.

count_subtags(Xml, Tag) ->
  [{_,_,_,_,_,_,_,_,T8,_,_,_}] = xmerl_xpath:string("//" ++ Tag, Xml),
  (length(T8) - 1) / 2.

main(File) ->
  {Xml, _} = xmerl_scan:file(File),
  Ent = [val(xmerl_xpath:string("//type", Xml))
        ,val(xmerl_xpath:string("//name", Xml))
        ,val(xmerl_xpath:string("//relations", Xml))
        ,val(xmerl_xpath:string("//dependencies", Xml))
        ,val(xmerl_xpath:string("//information", Xml))],
  Ent.
  %,val(xmerl_xpath:string("//t4", Xml))
  %].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary/tmp functions                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_vals() ->
  Sh = ["ship",    "Ship",    ["Service"],         [{"Service", fun (X) -> X == anton end}, {"Service", fun (X) -> X == 1234 end}], []],
  Se = ["service", "Service", ["Ship", "Company"], [],                                                                              []],
  Co = ["company", "Company", ["Service"],         [],                                                                           [map]],
  {Sh, Se, Co}.

list_vals_nofuninfo() ->
  Sh = ["ship",    "Ship",    ["Service"]        ],
  Se = ["service", "Service", ["Ship", "Company"]],
  Co = ["company", "Company", ["Service"]        ],
  {Sh, Se, Co}.

c_l_helper() ->
  _Input = list_vals(),
  Input_nofuninfo = list_vals_nofuninfo(),
  convert_lists(Input_nofuninfo).

convert_lists(Input) ->
  Input.

test_wr()->
  NL = [],
  VL = [],
  {NL1, VL1} = aux:write_v("Ship", NL, VL),
  erlang:display({NL1, VL1}),
  {NL2, VL2} = aux:write_v("Service", NL1, VL1),
  erlang:display({NL2, VL2}),
  {NL3, VL3} = aux:write_v("Company", NL2, VL2),
  erlang:display({NL3, VL3}),
  erlang:display({ship, aux:read_v("Ship", NL3, VL3)}),
  erlang:display({service, aux:read_v("Service", NL3, VL3)}),
  erlang:display({company, aux:read_v("Company", NL3, VL3)}).

