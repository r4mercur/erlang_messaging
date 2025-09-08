-module(db_worker).
-behaviour(poolboy_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    % PostgreSQL Connection
    {ok, Conn} = epgsql:connect(#{
        host => "localhost",
        port => 5432,
        username => "messaging_user",
        password => "messaging_pass",
        database => "messaging_db"
    }),
    {ok, #state{conn = Conn}}.

handle_call({query, Query}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:equery(Conn, Query),
    {reply, Result, State};

handle_call({query, Query, Params}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:equery(Conn, Query, Params),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.