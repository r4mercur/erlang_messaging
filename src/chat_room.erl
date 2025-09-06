-module(chat_room).
-behaviour(gen_server).

-export([start_link/1, join/3, leave/2, say/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    name,            % binary room name
    members = #{}    % pid() => Nick :: binary()
}).

%%% API

start_link(RoomBin) ->
    RegName = reg_name(RoomBin),
    case whereis(RegName) of
        undefined ->
            gen_server:start_link({local, RegName}, ?MODULE, [RoomBin], []);
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

join(RoomBin, Pid, Nick) ->
    {ok, _} = start_link(RoomBin),
    gen_server:cast(reg_name(RoomBin), {join, Pid, Nick}),
    ok.

leave(RoomBin, Pid) ->
    case whereis(reg_name(RoomBin)) of
        undefined -> ok;
        _ -> gen_server:cast(reg_name(RoomBin), {leave, Pid})
    end.

say(RoomBin, FromNick, Text) ->
    case whereis(reg_name(RoomBin)) of
        undefined -> ok;
        _ -> gen_server:cast(reg_name(RoomBin), {say, FromNick, Text})
    end.

reg_name(RoomBin) ->
    % Achtung: atom-Leak möglich – für PoC ok.
    list_to_atom("room_" ++ binary_to_list(RoomBin)).

%%% gen_server

init([RoomBin]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = RoomBin, members = #{}}}.

handle_cast({join, Pid, Nick}, S=#state{members=Ms, name=Room}) ->
    link(Pid),
    broadcast(Ms, Room, <<"system">>, <<(Nick)/binary, " joined">>),
    {noreply, S#state{members = Ms#{Pid => Nick}}};
handle_cast({leave, Pid}, S=#state{members=Ms, name=Room}) ->
    case maps:take(Pid, Ms) of
        error -> {noreply, S};
        {Nick, Ms2} ->
            broadcast(Ms2, Room, <<"system">>, <<(Nick)/binary, " left">>),
            {noreply, S#state{members = Ms2}}
    end;
handle_cast({say, FromNick, Text}, S=#state{members=Ms, name=Room}) ->
    broadcast(Ms, Room, FromNick, Text),
    {noreply, S}.

handle_call(_Req, _From, S) -> {reply, ok, S}.

handle_info({'EXIT', Pid, _Reason}, S=#state{members=Ms, name=Room}) ->
    case maps:take(Pid, Ms) of
        error -> {noreply, S};
        {Nick, Ms2} ->
            broadcast(Ms2, Room, <<"system">>, <<(Nick)/binary, " disconnected">>),
            {noreply, S#state{members = Ms2}}
    end;
handle_info(_I, S) -> {noreply, S}.

terminate(_Reason, _State) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

broadcast(Ms, Room, FromNick, Text) ->
    lists:foreach(
      fun({Pid, _Nick}) ->
          catch Pid ! {room_msg, Room, FromNick, Text}
      end,
      maps:to_list(Ms)
    ).
