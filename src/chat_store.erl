-module(chat_store).
-behaviour(gen_server).

-export([start_link/1]).
-export([create_user/1, get_user_by_nick/1, get_or_create_user/1]).
-export([create_conversation/1, create_conversation/2, get_conversation/1, get_user_conversations/1, get_conversation_by_name/1]).
-export([save_message/4, get_conversation_messages/2, get_conversation_messages/3]).
-export([add_participant/2, remove_participant/2, get_conversation_participants/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pool_name, room_table}).

start_link(PoolName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolName], []).

init([PoolName]) ->
    % Create ETS table for room name to conversation ID mapping
    RoomTable = ets:new(room_conversations, [set, protected, named_table]),
    {ok, #state{pool_name = PoolName, room_table = RoomTable}}.

%% User API
create_user(Nick) ->
    gen_server:call(?MODULE, {create_user, Nick}).

get_user_by_nick(Nick) ->
    gen_server:call(?MODULE, {get_user_by_nick, Nick}).

get_or_create_user(Nick) ->
    case get_user_by_nick(Nick) of
        {ok, User} -> {ok, User};
        {error, not_found} -> create_user(Nick);
        Error -> Error
    end.


%% Conversation API
create_conversation(Kind) ->
    gen_server:call(?MODULE, {create_conversation, Kind}).

create_conversation(Kind, RoomName) ->
    gen_server:call(?MODULE, {create_conversation, Kind, RoomName}).

get_conversation(ConversationId) ->
    gen_server:call(?MODULE, {get_conversation, ConversationId}).

get_user_conversations(UserId) ->
    gen_server:call(?MODULE, {get_user_conversations, UserId}).

get_conversation_by_name(RoomName) ->
    gen_server:call(?MODULE, {get_conversation_by_name, RoomName}).

%% Message API
save_message(ConversationId, SenderId, Body, ClientMsgId) ->
    gen_server:call(?MODULE, {save_message, ConversationId, SenderId, Body, ClientMsgId}).

get_conversation_messages(ConversationId, Limit) ->
    gen_server:call(?MODULE, {get_conversation_messages, ConversationId, Limit}).

get_conversation_messages(ConversationId, Limit, Offset) ->
    gen_server:call(?MODULE, {get_conversation_messages, ConversationId, Limit, Offset}).

%% Participant API
add_participant(ConversationId, UserId) ->
    gen_server:call(?MODULE, {add_participant, ConversationId, UserId}).

remove_participant(ConversationId, UserId) ->
    gen_server:call(?MODULE, {remove_participant, ConversationId, UserId}).

get_conversation_participants(ConversationId) ->
    gen_server:call(?MODULE, {get_conversation_participants, ConversationId}).

%% Gen Server Callbacks
handle_call({create_user, Nick}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "INSERT INTO app_user (nick) VALUES ($1) RETURNING id, nick",
        case gen_server:call(Worker, {query, Query, [Nick]}) of
            {ok, _Columns, [{Id, ReturnedNick}]} ->
                {ok, #{id => Id, nick => ReturnedNick}};
            {error, {error, error, <<"23505">>, unique_violation, _}} ->
                {error, nick_already_exists};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_user_by_nick, Nick}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT id, nick FROM app_user WHERE nick = $1",
        case gen_server:call(Worker, {query, Query, [Nick]}) of
            {ok, _Columns, [{Id, ReturnedNick}]} ->
                {ok, #{id => Id, nick => ReturnedNick}};
            {ok, _Columns, []} ->
                {error, not_found};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({create_conversation, Kind}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "INSERT INTO conversation (kind) VALUES ($1) RETURNING id, kind",
        case gen_server:call(Worker, {query, Query, [Kind]}) of
            {ok, _Columns, [{Id, ReturnedKind}]} ->
                {ok, #{id => Id, kind => ReturnedKind}};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({create_conversation, Kind, RoomName}, _From, #state{pool_name = Pool, room_table = RoomTable} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "INSERT INTO conversation (kind) VALUES ($1) RETURNING id, kind",
        case gen_server:call(Worker, {query, Query, [Kind]}) of
            {ok, _Count, _Columns, [{Id, ReturnedKind}]} ->
                % Store room name mapping in ETS
                ets:insert(RoomTable, {RoomName, Id}),
                {ok, #{id => Id, kind => ReturnedKind, room_name => RoomName}};
            {ok, _Columns, [{Id, ReturnedKind}]} ->
                % Handle alternative response format
                ets:insert(RoomTable, {RoomName, Id}),
                {ok, #{id => Id, kind => ReturnedKind, room_name => RoomName}};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_conversation, ConversationId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT id, kind FROM conversation WHERE id = $1",
        case gen_server:call(Worker, {query, Query, [ConversationId]}) of
            {ok, _Columns, [{Id, Kind}]} ->
                {ok, #{id => Id, kind => Kind}};
            {ok, _Columns, []} ->
                {error, not_found};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({save_message, ConversationId, SenderId, Body, ClientMsgId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "INSERT INTO message (conversation_id, sender_id, body, client_msg_id) VALUES ($1, $2, $3, $4) RETURNING id, sent_at",
        case gen_server:call(Worker, {query, Query, [ConversationId, SenderId, Body, ClientMsgId]}) of
            {ok, _Columns, [{Id, SentAt}]} ->
                {ok, #{id => Id, sent_at => SentAt}};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_conversation_messages, ConversationId, Limit}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT m.id, m.body, m.sent_at, u.nick, m.client_msg_id 
                FROM message m 
                LEFT JOIN app_user u ON m.sender_id = u.id 
                WHERE m.conversation_id = $1 
                ORDER BY m.sent_at DESC, m.id DESC 
                LIMIT $2",
        case gen_server:call(Worker, {query, Query, [ConversationId, Limit]}) of
            {ok, _Columns, Rows} ->
                Messages = [#{
                    id => Id, 
                    body => Body, 
                    sent_at => SentAt, 
                    sender_nick => Nick,
                    client_msg_id => ClientMsgId
                } || {Id, Body, SentAt, Nick, ClientMsgId} <- Rows],
                {ok, lists:reverse(Messages)};  % Reverse to get chronological order
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_conversation_messages, ConversationId, Limit, Offset}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT m.id, m.body, m.sent_at, u.nick, m.client_msg_id 
                FROM message m 
                LEFT JOIN app_user u ON m.sender_id = u.id 
                WHERE m.conversation_id = $1 
                ORDER BY m.sent_at DESC, m.id DESC 
                LIMIT $2 OFFSET $3",
        case gen_server:call(Worker, {query, Query, [ConversationId, Limit, Offset]}) of
            {ok, _Columns, Rows} ->
                Messages = [#{
                    id => Id, 
                    body => Body, 
                    sent_at => SentAt, 
                    sender_nick => Nick,
                    client_msg_id => ClientMsgId
                } || {Id, Body, SentAt, Nick, ClientMsgId} <- Rows],
                {ok, lists:reverse(Messages)};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({add_participant, ConversationId, UserId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "INSERT INTO conversation_participant (conversation_id, user_id) VALUES ($1, $2) ON CONFLICT DO NOTHING",
        case gen_server:call(Worker, {query, Query, [ConversationId, UserId]}) of
            {ok, _} -> ok;
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({remove_participant, ConversationId, UserId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "DELETE FROM conversation_participant WHERE conversation_id = $1 AND user_id = $2",
        case gen_server:call(Worker, {query, Query, [ConversationId, UserId]}) of
            {ok, _} -> ok;
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_conversation_participants, ConversationId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT u.id, u.nick FROM app_user u 
                JOIN conversation_participant cp ON u.id = cp.user_id 
                WHERE cp.conversation_id = $1",
        case gen_server:call(Worker, {query, Query, [ConversationId]}) of
            {ok, _Columns, Rows} ->
                Participants = [#{id => Id, nick => Nick} || {Id, Nick} <- Rows],
                {ok, Participants};
            Error -> Error
        end
    end),
    {reply, Result, State};

handle_call({get_user_conversations, UserId}, _From, #state{pool_name = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        Query = "SELECT c.id, c.kind FROM conversation c 
                JOIN conversation_participant cp ON c.id = cp.conversation_id 
                WHERE cp.user_id = $1",
        case gen_server:call(Worker, {query, Query, [UserId]}) of
            {ok, _Columns, Rows} ->
                Conversations = [#{id => Id, kind => Kind} || {Id, Kind} <- Rows],
                {ok, Conversations};
            Error -> Error
        end
    end),
    {reply, Result, State};


handle_call({get_conversation_by_name, RoomName}, _From, #state{room_table = RoomTable} = State) ->
    case ets:lookup(RoomTable, RoomName) of
        [{RoomName, ConversationId}] ->
            {reply, {ok, #{id => ConversationId, kind => <<"room">>}}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.