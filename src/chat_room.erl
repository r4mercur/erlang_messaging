-module(chat_room).
-behaviour(gen_server).

-export([start_link/1, join/3, leave/2, say/3, get_room_history/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    name,              % binary room name
    conversation_id,   % database conversation ID
    members = #{}      % pid() => #{nick => binary(), user_id => integer()}
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
    gen_server:call(reg_name(RoomBin), {join, Pid, Nick}).

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

get_room_history(RoomBin, Limit) ->
    case whereis(reg_name(RoomBin)) of
        undefined -> {error, room_not_found};
        _ -> gen_server:call(reg_name(RoomBin), {get_history, Limit})
    end.

reg_name(RoomBin) ->
    % Achtung: atom-Leak möglich – für PoC ok.
    list_to_atom("room_" ++ binary_to_list(RoomBin)).

%%% gen_server

init([RoomBin]) ->
    process_flag(trap_exit, true),
    
    % Get or create conversation for this room
    ConversationId = get_or_create_conversation(RoomBin),
    
    {ok, #state{
        name = RoomBin, 
        conversation_id = ConversationId,
        members = #{}
    }}.

handle_call({join, Pid, Nick}, _From, S=#state{members=Ms, name=Room, conversation_id=ConvId}) ->
    % Get or create user
    case chat_store:get_or_create_user(Nick) of
        {ok, #{id := UserId}} ->
            % Add user as participant
            chat_store:add_participant(ConvId, UserId),
            
            % Link process and update members
            link(Pid),
            UserInfo = #{nick => Nick, user_id => UserId},
            NewMembers = Ms#{Pid => UserInfo},
            
            % Broadcast join message
            broadcast(NewMembers, Room, <<"system">>, <<(Nick)/binary, " joined">>),
            
            % Send room history to new member
            case chat_store:get_conversation_messages(ConvId, 20) of
                {ok, Messages} ->
                    send_history_to_user(Pid, Room, Messages);
                _ -> ok
            end,
            
            {reply, ok, S#state{members = NewMembers}};
        Error ->
            {reply, Error, S}
    end;

handle_call({get_history, Limit}, _From, S=#state{conversation_id=ConvId}) ->
    Result = chat_store:get_conversation_messages(ConvId, Limit),
    {reply, Result, S};

handle_call(_Req, _From, S) -> 
    {reply, ok, S}.

handle_cast({leave, Pid}, S=#state{members=Ms, name=Room, conversation_id=ConvId}) ->
    case maps:take(Pid, Ms) of
        error -> {noreply, S};
        {#{nick := Nick, user_id := UserId}, Ms2} ->
            % Remove from database participants
            chat_store:remove_participant(ConvId, UserId),
            
            broadcast(Ms2, Room, <<"system">>, <<(Nick)/binary, " left">>),
            {noreply, S#state{members = Ms2}}
    end;

handle_cast({say, FromNick, Text}, S=#state{members=Ms, name=Room, conversation_id=ConvId}) ->
    % Find sender's user_id
    SenderInfo = find_member_by_nick(Ms, FromNick),
    
    % Save message to database
    case SenderInfo of
        {ok, #{user_id := SenderId}} ->
            ClientMsgId = generate_client_msg_id(),
            chat_store:save_message(ConvId, SenderId, Text, ClientMsgId);
        _ -> ok
    end,
    
    % Broadcast to all members
    broadcast(Ms, Room, FromNick, Text),
    {noreply, S}.

handle_info({'EXIT', Pid, _Reason}, S=#state{members=Ms, name=Room, conversation_id=ConvId}) ->
    case maps:take(Pid, Ms) of
        error -> {noreply, S};
        {#{nick := Nick, user_id := UserId}, Ms2} ->
            % Remove from database participants
            chat_store:remove_participant(ConvId, UserId),
            
            broadcast(Ms2, Room, <<"system">>, <<(Nick)/binary, " disconnected">>),
            {noreply, S#state{members = Ms2}}
    end;
handle_info(_I, S) -> {noreply, S}.

terminate(_Reason, _State) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

%% Helper Functions

broadcast(Ms, Room, FromNick, Text) ->
    lists:foreach(
      fun({Pid, _UserInfo}) ->
          catch Pid ! {room_msg, Room, FromNick, Text}
      end,
      maps:to_list(Ms)
    ).

find_member_by_nick(Members, Nick) ->
    case [UserInfo || {_Pid, UserInfo = #{nick := MemberNick}} <- maps:to_list(Members), MemberNick =:= Nick] of
        [UserInfo|_] -> {ok, UserInfo};
        [] -> {error, not_found}
    end.

get_or_create_conversation(RoomName) ->
    RoomNameStr = binary_to_list(RoomName),
    case chat_store:get_conversation_by_name(RoomNameStr) of
        {ok, #{id := ConvId}} -> ConvId;
        {error, not_found} ->
            % Create new conversation for this room
            case chat_store:create_conversation(<<"room">>, RoomNameStr) of
                {ok, #{id := ConvId}} -> ConvId;
                {error, Reason} -> 
                    io:format("Failed to create conversation: ~p~n", [Reason]),
                    error(failed_to_create_conversation);
                Other -> 
                    io:format("Unexpected response creating conversation: ~p~n", [Other]),
                    error(failed_to_create_conversation)
            end;
        Error ->
            io:format("Error getting conversation: ~p~n", [Error]),
            error(failed_to_get_conversation)
    end.

send_history_to_user(Pid, Room, Messages) ->
    lists:foreach(fun(#{body := Body, sender_nick := SenderNick}) ->
        case SenderNick of
            null -> 
                catch Pid ! {room_msg, Room, <<"system">>, Body};
            Nick -> 
                catch Pid ! {room_msg, Room, Nick, Body}
        end
    end, Messages).

generate_client_msg_id() ->
    % Generate a simple UUID-like string
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                                  [A, B, C, D, E])).