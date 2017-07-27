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
%% @doc
%%   
-module(ecsd_dns).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

-record(state, {
   id = undefined :: binary()   %% container identity
}).

start_link(Action, Id) ->
   pipe:start_link(?MODULE, [Action, Id], []).

init([Action, Id]) ->
   self() ! Action,
   {ok, handle, #state{id = Id}}.

free(_, _) ->
   ok.

%%
handle(up, _, #state{id = Id} = State) ->
   Env = env(Id),
   [option || fqdn(Env), route53a(<<"UPSERT">>, _)],
   [option || port(Env), route53s(<<"UPSERT">>, _)],
   {stop, normal, State};

%%
handle(rm, _, #state{id = Id} = State) ->
   Env = env(Id),
   [option || fqdn(Env), route53a(<<"DELETE">>, _)],
   [option || port(Env), route53s(<<"DELETE">>, _)],
   {stop, normal, State}.


%%
env(Id) ->
   Lens = lens:c([lens:hd(#{}), lens:map(<<"Config">>, #{}), lens:map(<<"Env">>, [])]),
   {ok, Spec} = esh:run_link([docker, inspect, Id]),
   Json = jsx:decode(scalar:s(Spec), [return_maps]),
   lens:get(Lens, Json).

%%
%% SERVICE_FQDN = fqdn
%% SERVICE_FQDN_LOCAL = fqdn
fqdn([<<"SERVICE_FQDN=", FQDN/binary>> | _]) ->
   {'public-ipv4', FQDN};
fqdn([<<"SERVICE_FQDN_LOCAL=", FQDN/binary>> | _]) ->
   {'local-ipv4', FQDN};
fqdn([_ | Env]) ->
   fqdn(Env);
fqdn([]) ->
   undefined.

%%
%% SERVICE_PORT_<port> = fqdn
port([<<"SERVICE_PORT_", Spec/binary>> | Env]) ->
   case binary:split(Spec, <<$=>>) of
      [Port, FQDN] ->
         {Port, FQDN};
      _ ->
         port(Env)
   end;
port([_ | Env]) ->
   port(Env);
port([]) ->
   undefined.

%%
%%
route53a(Actn, {Mode, FQDN}) ->
   {ok, IP} = esh:run_link([sh, which(host), Mode]),
   lager:info("[~s] A ~s @ ~s", [Actn, FQDN, IP]),
   {ok,  _} = esh:run_link([sh, which(route53a), Actn, IP, FQDN, opts:val(hosted_zone_id, ecsd)]).

route53s(Actn, {Port, FQDN}) ->
   {ok, IP} = esh:run_link([sh, which(host), hostname]),
   lager:info("[~s] SRV ~s @ ~s", [Actn, FQDN, IP]),
   {ok, _} = esh:run_link([sh, which(route53srv), Actn, IP, Port, FQDN, opts:val(hosted_zone_id, ecsd)]).

%%
%%
which(Script) ->
   filename:join([
      code:priv_dir(ecsd),
      "aws",
      scalar:c(Script) ++ ".sh"
   ]).

