-module(db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),
    % PostgreSQL connection parameters
    ConnOpts = [
        {host, "host.docker.internal"},
        {port, 5432},
        {database, "messaging_db"},
        {username, "messaging_user"},
        {password, "messaging_pass"}
    ],
    
    case epgsql:connect(ConnOpts) of
        {ok, Conn} ->
            {ok, #state{conn = Conn}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({query, Sql}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:squery(Conn, Sql),
    {reply, Result, State};

handle_call({query, Sql, Params}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:equery(Conn, Sql, Params),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.