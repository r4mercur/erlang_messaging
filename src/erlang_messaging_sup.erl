%%%-------------------------------------------------------------------
%% @doc erlang_messaging top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_messaging_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ListenerChild = #{
        id => chat_listener,
        start => {chat_listener, start_link, [4040]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [chat_listener]
    },
    {ok, {{one_for_one, 5, 10}, [ListenerChild]}}.

%% internal functions
