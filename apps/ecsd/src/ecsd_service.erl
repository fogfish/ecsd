-module(ecsd_service).
-behavior(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).


start_link() ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
   {ok, handle, undefined}.

free(_, _) ->
   ok.

handle(#{<<"status">> := <<"die">>,  <<"Type">> := <<"container">>, <<"id">> := Id}, _Pipe, State) ->
   lager:info("[free] container ~s~n", [Id]),
   supervisor:start_child(ecsd_dns_sup, [rm, Id]),
   {next_state, handle, State};

handle(#{<<"status">> := <<"start">>, <<"Type">> := <<"container">>, <<"id">> := Id}, _Pipe, State) ->
   lager:info("[init] container ~s~n", [Id]),
   supervisor:start_child(ecsd_dns_sup, [up, Id]),
   {next_state, handle, State};

handle(_Msg, _Pipe, State) ->
   {next_state, handle, State}.
