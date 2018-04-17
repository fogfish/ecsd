%% @doc
%%   A boot sequence for each node
-module(ecsd_boot).
-behaviour(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   boot/3
]).

-record(state, {}).

start_link() ->
   pipe:start_link(?MODULE, [], []).

init(_) ->
   fs:foreach(
      fun(X) -> 
         case filelib:is_file(X) of
            true ->
               self() ! {script, X};
            _ ->
               ok
         end
      end,
      "/usr/local/boot"
   ),
   {ok, boot, #state{}}.

free(_, _) ->
   ok.

boot({script, File}, _, State) ->
   lager:info("[boot] ~s", [File]),
   spawn_link(
      fun() ->
         {ok, Result} = esh:run_link([sh, File]),
         lager:info("[boot] ~s~n~s~n", [File, Result])
      end
   ),
   {next_state, boot, State}.
