-module(chat_server_rooms).
-behavior(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/0, get_all_rooms/0, get_users/1, is_room_available/1, put_room/1, delete_room/1, is_user_in_room/2, get_all_rooms_by_user/1, join_room/2, leave_room/2, delete_pid/1]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
  gen_server:call(?MODULE, {get_state}).

get_all_rooms() ->
  Resp = get_state(),
  {current_state, State} = Resp,
  maps:keys(State).

get_users(Room) ->
  case is_room_available(Room) of
    true ->
      Resp = get_state(),
      {current_state, State} = Resp,
      maps:get(Room, State);
    false ->
      []
  end.

is_room_available(Room) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  L = maps:keys(State),
  lists:member(Room,L).

put_room(Room) ->
  gen_server:cast(?MODULE, {put_room, {Room}}).

delete_room(Room) ->
  gen_server:cast(?MODULE, {delete_room, {Room}}).

is_user_in_room(Room, User) ->
  case is_room_available(Room) of
    true ->
      Resp = get_state(),
      {current_state, State} = Resp,
      L = maps:get(Room, State),
      lists:member(User, L);
    false ->
      false
  end.

get_all_rooms_by_user(User) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  Pred = fun(K,V) -> 
    LUsersPerRoom = maps:get(K, State, undefined),
    lists:member(User,LUsersPerRoom)
  end,
  Filtered = maps:filter(Pred,State),
  maps:keys(Filtered).

join_room(Room, User) ->
  gen_server:cast(?MODULE, {join_room, {Room, User}}).

leave_room(Room, User) ->
  gen_server:cast(?MODULE, {leave_room, {Room, User}}).

delete_pid(Pid) ->
  Resp = get_state(),
  {current_state, State} = Resp,
  L = maps:keys(State),
  delete_pid(L, Pid).

delete_pid([], _) ->
  ok;

delete_pid([H|T], Pid) ->
  leave_room(H, Pid),
  delete_pid(T, Pid).

init([]) ->
  {ok, #{}}.

handle_call({get_state}, _From, State) ->
    Response = {current_state, State},
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({put_room, {Key}}, State) ->
    NewState = maps:put(Key, [], State),
    {noreply, NewState};

handle_cast({delete_room, {Key}}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState};

handle_cast({join_room, {Key, User}}, State) ->
    % io:format("a~n"),
    L = maps:get(Key, State),
    % io:format("b~n"),
    NewL = lists:append(L, [User]),
    % io:format("c~n"),
    NewState = maps:put(Key, NewL, State),
    {noreply, NewState};

handle_cast({leave_room, {Key, User}}, State) ->
    L = maps:get(Key, State),
    NewL = lists:delete(User,L),
    NewState = maps:put(Key, NewL, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.