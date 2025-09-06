-module(web_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", web_handler, []},
            {"/chat", web_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http, 
        [{port, Port}], 
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("web_server: listening on port ~p~n", [Port]),
    {ok, #{port => Port}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.