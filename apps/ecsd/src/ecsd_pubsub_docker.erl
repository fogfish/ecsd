%%
%%   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
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

handle({esh, _, {eof, _}}, _Pipe, State) ->
   {stop, eof, State};

handle({esh, _, Pckt}, _Pipe, State) ->
   lists:foreach(fun upstream/1, binary:split(Pckt, <<$\n>>, [trim, global])),
   {next_state, handle, State}.

%%
%%
upstream(Json) ->
   try
      pipe:send(ecsd_service, jsx:decode(Json, [return_maps]))
   catch Error:Reason ->
      error_logger:error_report([
         {error,  Error},
         {reason, Reason},
         {packet, Json},
         {stack,   erlang:get_stacktrace()}
      ])
   end.
