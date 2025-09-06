-module(chat_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LADDR, any).

-record(state, {lsock, port}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [
        binary,
        {ip, ?LADDR},
        {packet, line},
        {active, false},
        {reuseaddr, true},
        {backlog, 1024}
    ]),
    self() ! accept,
    io:format("chat_listener: listening on ~p~n", [Port]),
    {ok, #state{lsock = LSock, port = Port}}.

handle_info(accept, State=#state{lsock=LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            ok = gen_tcp:controlling_process(Sock, spawn_session(Sock)),
            self() ! accept,
            {noreply, State};
        {error, Reason} ->
            io:format("accept error: ~p~n", [Reason]),
            {stop, Reason, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

spawn_session(Sock) ->
    Pid = proc_lib:spawn_link(fun() ->
        chat_session:start(Sock)
    end),
    Pid.
