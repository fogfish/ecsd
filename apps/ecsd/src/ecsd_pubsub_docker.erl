-module(ecsd_pubsub_docker).
-behaviour(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

-record(state, {
   pid = undefined :: pid()
}).

start_link() ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
   {ok, Pid} = esh:spawn_link([docker, events, "--format '{{json .}}'"]),
   {ok, handle, #state{pid = Pid}}.

free(_, #state{pid = Pid}) ->
   esh:close(Pid).

handle({esh, _, Json}, _Pipe, State) ->
   pipe:send(ecsd_service, jsx:decode(Json, [return_maps])),
   {next_state, handle, State}.
