-module(ecsd_dns_sup).
-behaviour(supervisor).

-export([
   start_link/0, init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, transient, 5000, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   {ok,
      {
         {simple_one_for_one, 10, 1800},
         [
            ?CHILD(worker, ecsd_dns)
         ]
      }
   }.
