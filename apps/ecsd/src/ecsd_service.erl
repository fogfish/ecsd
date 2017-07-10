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
