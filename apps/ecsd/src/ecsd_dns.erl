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
%% SERVICE_FQDN = name
fqdn([<<"SERVICE_FQDN=", FQDN/binary>> | _]) ->
   FQDN;
fqdn([_ | Env]) ->
   fqdn(Env);
fqdn([]) ->
   undefined.

%%
%% SERVICE_PORT_<port> = name
port([<<"SERVICE_PORT_", Spec/binary>> | Env]) ->
   case binary:split(Spec, <<$=>>) of
      [Port, Name] ->
         {Port, Name};
      _ ->
         port(Env)
   end;
port([_ | Env]) ->
   port(Env);
port([]) ->
   undefined.

%%
%%
route53a(Actn, FQDN) ->
   lager:info("[~s] ~s @ route53", [Actn, FQDN]),
   {ok, _} = esh:run_link([sh, which(route53a), Actn, FQDN, opts:val(hosted_zone_id, ecsd)]).

route53s(Actn, FQDN) ->
   ok.

%%
%%
which(Script) ->
   filename:join([
      code:priv_dir(ecsd),
      "aws",
      scalar:c(Script) ++ ".sh"
   ]).

