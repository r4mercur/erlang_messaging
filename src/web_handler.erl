-module(web_handler).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

-record(ws_state, {
    session_pid,
    nick = <<"anon">>,
    room = undefined,
    user_id = undefined
}).

%% HTTP Handler for static files and WebSocket upgrade
init(Req0, _State) ->
    case cowboy_req:header(<<"upgrade">>, Req0) of
        <<"websocket">> ->
            {cowboy_websocket, Req0, #ws_state{}};
        _ ->
            % Statische HTML-Seite ausliefern
            serve_html(Req0)
    end.

serve_html(Req0) ->
    Html = get_chat_html(),
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"text/html">>}, 
        Html, Req0),
    {ok, Req, no_state}.

%% WebSocket Callbacks
websocket_init(State) ->
    SessionPid = spawn_link(fun() -> websocket_session_loop(self()) end),
    {ok, State#ws_state{session_pid = SessionPid}}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"nick">>, <<"data">> := Nick} ->
            % Get or create user in database
            case chat_store:get_or_create_user(Nick) of
                {ok, #{id := UserId}} ->
                    NewState = State#ws_state{nick = Nick, user_id = UserId},
                    {reply, {text, jsx:encode(#{type => <<"nick_ok">>, data => <<"Nick set">>})}, NewState};
                _ ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Failed to set nick">>})}, State}
            end;
        
        #{<<"type">> := <<"join">>, <<"data">> := Room} ->
            try
                maybe_leave_room(State),
                case chat_room:join(Room, self(), State#ws_state.nick) of
                    ok ->
                        NewState = State#ws_state{room = Room},
                        {reply, {text, jsx:encode(#{type => <<"join_ok">>, room => Room})}, NewState};
                    _ ->
                        {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Failed to join room">>})}, State}
                end
            catch
                _:_ ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Failed to join room">>})}, State}
            end;
        
        #{<<"type">> := <<"message">>, <<"data">> := Text} ->
            case State#ws_state.room of
                undefined ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Join a room first">>})}, State};
                Room ->
                    chat_room:say(Room, State#ws_state.nick, Text),
                    {ok, State}
            end;
        
        #{<<"type">> := <<"leave">>} ->
            maybe_leave_room(State),
            NewState = State#ws_state{room = undefined},
            {reply, {text, jsx:encode(#{type => <<"leave_ok">>})}, NewState};
        
        #{<<"type">> := <<"get_history">>, <<"data">> := Limit} ->
            case State#ws_state.room of
                undefined ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Join a room first">>})}, State};
                Room ->
                    case chat_room:get_room_history(Room, Limit) of
                        {ok, Messages} ->
                            HistoryMsg = jsx:encode(#{
                                type => <<"history">>, 
                                messages => format_messages_for_client(Messages)
                            }),
                            {reply, {text, HistoryMsg}, State};
                        _ ->
                            {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Failed to get history">>})}, State}
                    end
            end;
        
        _ ->
            {reply, {text, jsx:encode(#{type => <<"error">>, data => <<"Unknown command">>})}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({room_msg, Room, FromNick, Text}, State) ->
    Msg = jsx:encode(#{
        type => <<"room_message">>, 
        room => Room, 
        nick => FromNick, 
        message => Text,
        timestamp => erlang:system_time(millisecond)
    }),
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    {ok, State}.

websocket_terminate(_Reason, _Req, State) ->
    maybe_leave_room(State),
    ok.

%% Helper Functions
maybe_leave_room(#ws_state{room = undefined}) -> ok;
maybe_leave_room(#ws_state{room = Room}) ->
    chat_room:leave(Room, self()).

format_messages_for_client(Messages) ->
    [#{
        nick => maps:get(sender_nick, Msg, <<"system">>),
        message => maps:get(body, Msg),
        timestamp => format_timestamp(maps:get(sent_at, Msg))
    } || Msg <- Messages].

format_timestamp(Timestamp) ->
    % Convert PostgreSQL timestamp to milliseconds
    calendar:datetime_to_gregorian_seconds(Timestamp) * 1000.

websocket_session_loop(WebSocketPid) ->
    receive
        {room_msg, Room, FromNick, Text} ->
            WebSocketPid ! {room_msg, Room, FromNick, Text},
            websocket_session_loop(WebSocketPid);
        _ ->
            websocket_session_loop(WebSocketPid)
    end.

% ... rest of get_chat_html() bleibt gleich ...
get_chat_html() ->
    <<"<!DOCTYPE html>
<html lang='de'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>Chat App</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background-color: #f0f0f0; }
        .container { max-width: 800px; margin: 0 auto; background: white; border-radius: 10px; padding: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
        .header { text-align: center; color: #e74c3c; margin-bottom: 20px; }
        .chat-area { height: 400px; border: 1px solid #ddd; padding: 10px; overflow-y: auto; background-color: #fafafa; margin-bottom: 10px; }
        .input-area { display: flex; gap: 10px; margin-bottom: 10px; }
        .input-area input { flex: 1; padding: 10px; border: 1px solid #ddd; border-radius: 5px; }
        .input-area button { padding: 10px 20px; background-color: #e74c3c; color: white; border: none; border-radius: 5px; cursor: pointer; }
        .input-area button:hover { background-color: #c0392b; }
        .controls { display: flex; gap: 10px; margin-bottom: 10px; }
        .controls input { padding: 8px; border: 1px solid #ddd; border-radius: 5px; }
        .controls button { padding: 8px 15px; background-color: #3498db; color: white; border: none; border-radius: 5px; cursor: pointer; }
        .controls button:hover { background-color: #2980b9; }
        .message { margin-bottom: 5px; }
        .message.own { color: #27ae60; font-weight: bold; }
        .message.other { color: #2c3e50; }
        .message.system { color: #7f8c8d; font-style: italic; }
        .message.history { opacity: 0.7; }
        .status { padding: 10px; background-color: #ecf0f1; border-radius: 5px; margin-bottom: 10px; }
    </style>
</head>
<body>
    <div class='container'>
        <h1 class='header'>Chat App</h1>
        
        <div class='status' id='status'>
            Status: Verbindung wird hergestellt...
        </div>
        
        <div class='controls'>
            <input type='text' id='nickInput' placeholder='Dein Name' value='anon'>
            <button onclick='setNick()'>Name setzen</button>
            <input type='text' id='roomInput' placeholder='Raum-Name' value='singles'>
            <button onclick='joinRoom()'>Raum beitreten</button>
            <button onclick='leaveRoom()'>Raum verlassen</button>
            <button onclick='getHistory()'>History laden</button>
        </div>
        
        <div class='chat-area' id='chatArea'></div>
        
        <div class='input-area'>
            <input type='text' id='messageInput' placeholder='Nachricht eingeben...' onkeypress='if(event.key==\"Enter\") sendMessage()'>
            <button onclick='sendMessage()'>Senden</button>
        </div>
    </div>

    <script>
        let ws;
        let currentRoom = null;
        let currentNick = 'anon';
        
        function connect() {
            const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            ws = new WebSocket(protocol + '//' + window.location.host + '/chat');
            
            ws.onopen = function() {
                updateStatus('Verbunden!', 'success');
                addMessage('System: Willkommen in der Chat App!', 'system');
            };
            
            ws.onmessage = function(event) {
                const data = JSON.parse(event.data);
                handleMessage(data);
            };
            
            ws.onclose = function() {
                updateStatus('Verbindung getrennt. Neuverbindung in 3 Sekunden...', 'error');
                setTimeout(connect, 3000);
            };
            
            ws.onerror = function() {
                updateStatus('Verbindungsfehler', 'error');
            };
        }
        
        function handleMessage(data) {
            switch(data.type) {
                case 'nick_ok':
                    updateStatus('Name gesetzt: ' + currentNick, 'success');
                    break;
                case 'join_ok':
                    currentRoom = data.room;
                    updateStatus('Raum beigetreten: ' + currentRoom, 'success');
                    addMessage('Du bist dem Raum \"' + currentRoom + '\" beigetreten', 'system');
                    break;
                case 'leave_ok':
                    addMessage('Du hast den Raum \"' + currentRoom + '\" verlassen', 'system');
                    currentRoom = null;
                    updateStatus('Kein Raum', 'info');
                    break;
                case 'room_message':
                    const isOwn = data.nick === currentNick;
                    addMessage(data.nick + ': ' + data.message, isOwn ? 'own' : 'other');
                    break;
                case 'history':
                    clearChatArea();
                    data.messages.forEach(msg => {
                        const isOwn = msg.nick === currentNick;
                        const msgClass = msg.nick === 'system' ? 'system history' : (isOwn ? 'own history' : 'other history');
                        addMessage(msg.nick + ': ' + msg.message, msgClass);
                    });
                    addMessage('--- Ende der History ---', 'system');
                    break;
                case 'error':
                    updateStatus('Fehler: ' + data.data, 'error');
                    addMessage('Fehler: ' + data.data, 'system');
                    break;
            }
        }
        
        function setNick() {
            const nick = document.getElementById('nickInput').value.trim();
            if (nick) {
                currentNick = nick;
                send({type: 'nick', data: nick});
            }
        }
        
        function joinRoom() {
            const room = document.getElementById('roomInput').value.trim();
            if (room) {
                send({type: 'join', data: room});
            }
        }
        
        function leaveRoom() {
            if (currentRoom) {
                send({type: 'leave'});
            }
        }
        
        function getHistory() {
            if (currentRoom) {
                send({type: 'get_history', data: 50});
            } else {
                updateStatus('Tritt zuerst einem Raum bei!', 'error');
            }
        }
        
        function sendMessage() {
            const input = document.getElementById('messageInput');
            const message = input.value.trim();
            if (message && currentRoom) {
                send({type: 'message', data: message});
                input.value = '';
            } else if (!currentRoom) {
                updateStatus('Tritt zuerst einem Raum bei!', 'error');
            }
        }
        
        function send(data) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(data));
            }
        }
        
        function addMessage(text, type) {
            const chatArea = document.getElementById('chatArea');
            const div = document.createElement('div');
            div.className = 'message ' + type;
            div.textContent = new Date().toLocaleTimeString() + ' - ' + text;
            chatArea.appendChild(div);
            chatArea.scrollTop = chatArea.scrollHeight;
        }
        
        function clearChatArea() {
            document.getElementById('chatArea').innerHTML = '';
        }
        
        function updateStatus(text, type) {
            const status = document.getElementById('status');
            status.textContent = 'Status: ' + text;
            status.style.backgroundColor = type === 'success' ? '#d5f4e6' : 
                                         type === 'error' ? '#fadbd8' : '#ecf0f1';
        }
        
        // Bei Laden der Seite verbinden
        connect();
    </script>
</body>
</html>">>.