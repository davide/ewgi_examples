#!/usr/bin/env escript
%% -*- erlang -*-
main([AppSrc, AppTarget, Version]) ->
	EBinFolder = filename:dirname(AppTarget) ++ "/",
	Modules = get_app_modules(EBinFolder),
	generate_app_file(AppSrc, AppTarget, Version, Modules),
	ok;
main(_) ->
	io:format("Invalid arguments to gen_app.erl!").

get_app_modules(EBinFolder) ->
	BeamFiles = filelib:wildcard(EBinFolder ++ "*.beam"),
	Names = [extract_module(beam_lib:info(X)) || X <- BeamFiles],
	string:join(Names, ", ").

extract_module([]) ->
	"undefined";
extract_module([{module, Mod}|_]) ->
	atom_to_list(Mod);
extract_module([_|R]) ->
	extract_module(R).

generate_app_file(AppSrc, AppTarget, Version, Modules) ->
	{ok, AppFile} = file:read_file(AppSrc),
	App1 = re:replace(AppFile, "%VSN%", Version),
	App2 = re:replace(App1, "%MODULES%", Modules),
	ok = file:write_file(AppTarget, App2).

