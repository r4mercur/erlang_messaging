-module(chat_store).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API exports
-export([
    get_or_create_user/1,
    create_conversation/1,
    get_conversation_by_name/1,
    add_participant/2,
    remove_participant/2,
    save_message/4,
    get_conversation_messages/2
]).

-record(state, {pool_name}).

%% API Functions

start_link(PoolName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolName], []).

get_or_create_user(Nick) ->
    gen_server:call(?MODULE, {get_or_create_user, Nick}).

create_conversation(Kind) ->
    gen_server:call(?MODULE, {create_conversation, Kind}).

get_conversation_by_name(RoomName) ->
    gen_server:call(?MODULE, {get_conversation_by_name, RoomName}).

add_participant(ConvId, UserId) ->
    gen_server:call(?MODULE, {add_participant, ConvId, UserId}).

remove_participant(ConvId, UserId) ->
    gen_server:call(?MODULE, {remove_participant, ConvId, UserId}).

save_message(ConvId, SenderId, Text, ClientMsgId) ->
    gen_server:call(?MODULE, {save_message, ConvId, SenderId, Text, ClientMsgId}).

get_conversation_messages(ConvId, Limit) ->
    gen_server:call(?MODULE, {get_conversation_messages, ConvId, Limit}).

%% gen_server Callbacks

init([PoolName]) ->
    {ok, #state{pool_name = PoolName}}.

handle_call({get_or_create_user, Nick}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        % First try to get existing user
        case gen_server:call(Worker, {query, "SELECT id, nick FROM app_user WHERE nick = $1", [Nick]}) of
            {ok, _Columns, [{Id, Nick}]} ->
                {ok, #{id => Id, nick => Nick}};
            {ok, _Columns, []} ->
                % User doesn't exist, create new one
                case gen_server:call(Worker, {query, "INSERT INTO app_user (nick) VALUES ($1) RETURNING id", [Nick]}) of
                    {ok, _, [{Id}]} ->
                        {ok, #{id => Id, nick => Nick}};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call({create_conversation, Kind}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        case gen_server:call(Worker, {query, "INSERT INTO conversation (kind) VALUES ($1) RETURNING id", [Kind]}) of
            {ok, _Columns, [{Id}]} ->
                {ok, #{id => Id, kind => Kind}};
            {error, Reason} ->
                {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call({get_conversation_by_name, _RoomName}, _From, State) ->
    % For now, always return not_found to force room creation
    % In a real implementation, you might store room names in a separate table
    {reply, {error, not_found}, State};

handle_call({add_participant, ConvId, UserId}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        case gen_server:call(Worker, {query, 
            "INSERT INTO conversation_participant (conversation_id, user_id) VALUES ($1, $2) ON CONFLICT DO NOTHING", 
            [ConvId, UserId]}) of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call({remove_participant, ConvId, UserId}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        case gen_server:call(Worker, {query, 
            "DELETE FROM conversation_participant WHERE conversation_id = $1 AND user_id = $2", 
            [ConvId, UserId]}) of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call({save_message, ConvId, SenderId, Text, ClientMsgId}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        case gen_server:call(Worker, {query, 
            "INSERT INTO message (conversation_id, sender_id, body, client_msg_id) VALUES ($1, $2, $3, $4) RETURNING id", 
            [ConvId, SenderId, Text, ClientMsgId]}) of
            {ok, _Columns, [{Id}]} ->
                {ok, #{id => Id}};
            {error, Reason} ->
                {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call({get_conversation_messages, ConvId, Limit}, _From, State) ->
    Result = with_db_worker(State, fun(Worker) ->
        case gen_server:call(Worker, {query, 
            "SELECT m.id, m.body, m.sent_at, u.nick as sender_nick " ++
            "FROM message m " ++
            "LEFT JOIN app_user u ON m.sender_id = u.id " ++
            "WHERE m.conversation_id = $1 " ++
            "ORDER BY m.sent_at DESC LIMIT $2", 
            [ConvId, Limit]}) of
            {ok, _Columns, Rows} ->
                Messages = [#{
                    id => Id,
                    body => Body,
                    sent_at => SentAt,
                    sender_nick => SenderNick
                } || {Id, Body, SentAt, SenderNick} <- Rows],
                {ok, lists:reverse(Messages)};
            {error, Reason} ->
                {error, Reason}
        end
    end),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

with_db_worker(#state{pool_name = PoolName}, Fun) ->
    poolboy:transaction(PoolName, Fun).