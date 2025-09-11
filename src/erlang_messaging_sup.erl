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
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    PoolName = db_pool,
    PoolSize = 5,
    MaxOverflow = 5,

    PoolboyChild = #{
        id => db_pool,
        start => {poolboy, start_link, [
            [{name, {local, PoolName}},
             {worker_module, db_worker},
             {size, PoolSize},
             {max_overflow, MaxOverflow}]
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [poolboy]
    },

    ChatStore = #{
        id => chat_store,
        start => {chat_store, start_link, [PoolName]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [chat_store]
    },
    
    ChatListener = #{id => chat_listener,
                     start => {chat_listener, start_link, [4040]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [chat_listener]},
    
    WebServer = #{id => web_server,
                  start => {web_server, start_link, [8080]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [web_server]},
    
    Children = [ChatListener, WebServer, PoolboyChild, ChatStore],
    {ok, {SupFlags, Children}}.

%% internal functions
