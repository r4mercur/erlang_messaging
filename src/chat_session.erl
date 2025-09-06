-module(chat_session).

-define(PROMPT, <<"Commands: NICK <name> | JOIN <room> | MSG <text> | LEAVE | QUIT\r\n">>).

-export([start/1]).

-record(state, {
    sock,
    nick = <<"anon">>,
    room = undefined  % atom 'undefined' or binary Roomname
}).

start(Sock) ->
    ok = gen_tcp:send(Sock, <<"Welcome to ErlangChat!\r\n">>),
    ok = gen_tcp:send(Sock, ?PROMPT),
    inet:setopts(Sock, [{active, once}]),
    loop(#state{sock = Sock}).

loop(State=#state{sock=Sock}) ->
    receive
        {tcp, Sock, LineBin} ->
            Line = binary_to_list(LineBin),
            NewState = handle_line(string:trim(Line), State),
            inet:setopts(Sock, [{active, once}]),
            loop(NewState);
        {tcp_closed, Sock} ->
            cleanup(State),
            ok;
        {room_msg, Room, FromNick, Text} ->
            % incoming broadcast message
            Msg = io_lib:format("ROOM ~s ~s: ~s\r\n", [Room, FromNick, Text]),
            catch gen_tcp:send(Sock, list_to_binary(Msg)),
            loop(State)
    after 600000 ->
            % Idle Timeout 10min
            cleanup(State),
            catch gen_tcp:close(Sock)
    end.

handle_line("", State) -> State;
handle_line("QUIT", State=#state{sock=Sock}) ->
    gen_tcp:send(Sock, <<"Bye!\r\n">>), 
    cleanup(State),
    catch gen_tcp:close(Sock),
    State;
handle_line(Line, State) ->
    case string:tokens(Line, " ") of
        ["NICK"|Rest] ->
            Nick = unicode:characters_to_binary(string:trim(string:join(Rest, " "))),
            send_ok(State#state.sock, <<"NICK set\r\n">>),
            State#state{nick = Nick};
        ["JOIN"|Rest] ->
            Room = unicode:characters_to_binary(string:trim(string:join(Rest, " "))),
            maybe_leave(State),
            ok = chat_room:join(Room, self(), State#state.nick),
            send_ok(State#state.sock, <<"JOIN ok\r\n">>),
            State#state{room = Room};
        ["MSG"|Rest] ->
            Text = unicode:characters_to_binary(string:join(Rest, " ")),
            case State#state.room of
                undefined ->
                    send_err(State#state.sock, <<"JOIN a room first\r\n">>), State;
                Room ->
                    chat_room:say(Room, State#state.nick, Text),
                    State
            end;
        ["LEAVE"|_] ->
            maybe_leave(State),
            send_ok(State#state.sock, <<"LEAVE ok\r\n">>),
            State#state{room = undefined};
        _Else ->
            send_err(State#state.sock, ?PROMPT),
            State
    end.

send_ok(Sock, B) -> catch gen_tcp:send(Sock, B).
send_err(Sock, B) -> catch gen_tcp:send(Sock, B).

maybe_leave(#state{room=undefined}) -> ok;
maybe_leave(#state{room=Room}) ->
    chat_room:leave(Room, self()), ok.

cleanup(State=#state{room=undefined}) -> State;
cleanup(State=#state{room=Room}) ->
    chat_room:leave(Room, self()),
    State#state{room=undefined}.
