-module(tests).

-import(mmods, [start/0]).

-compile(export_all).
-compile(nowarn_export_all).

main() ->
	{ok, Ship}    = mmods:start(),
	{ok, Service} = mmods:start(),
	{ok, Company} = mmods:start(),
	{ok, Ship}    = mmods:add_relation(Ship, Service),
	A = mmods:get_state(Ship),
	B = mmods:get_state(Service),
	C = mmods:get_state(Company),
	{A, B, C}.