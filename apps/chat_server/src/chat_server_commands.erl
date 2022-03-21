-module(chat_server_commands).
-export([cmd_user_list/0, cmd_user_setname/2, cmd_user_whoami/1, cmd_msg/2, cmd_default/1,cmd_name_not_set/0, cmd_room_list/0, cmd_room_create/1, cmd_room_delete/1, cmd_room_join/2, cmd_room_leave/2, cmd_room_userlist/1, cmd_room_msg/2, cmd_room_belong/1]).

-define(ReValidName, "^[a-zA-Z0-9]{3,}$").
-define(StrNameNotValid, "name is not valid; at least 3 chars of letters/numbers").
-define(StrNameAlreadyUsed, "name is already used; please choose another one").
-define(StrRoomNotFound, "room not found").

cmd_user_list() ->
  io:format("[cmd] <<user,list>>~n",[]),
  Names = chat_server_users:getAllNames(),
  io:format("~p~n",[Names]),
  Resp = "users:" ++ string:join(Names,",") ++ "\n",
  {ok, Resp}.

cmd_user_setname(Pid, Data) ->
  io:format("[cmd] <<user,setname>>~n",[]),
  Name = re:replace(Data, "<<user,setname>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test1 = is_valid_name(Trimmed),
  Test2 = chat_server_users:nameIsSetByName(Trimmed),
  if
    not Test1 ->
      {ok, ?StrNameNotValid ++ "\n"};
    Test2 ->
      {ok, ?StrNameAlreadyUsed ++ "\n"};   
    true ->
      chat_server_users:putName(Pid, Trimmed),
      {ok, "ok\n"}
  end.

cmd_user_whoami(Pid) ->
  Name = chat_server_users:getName(Pid),
  case io_lib:char_list(Name) of
    true -> {ok, Name ++ "\n"};
    false -> {ok, "name is not set yet\n"}
  end.

cmd_msg(Pid, Data) ->
  RecvText = re:replace(Data, "<<msg,", "", [{return,list}]),
  RecvTextNoRC = re:replace(RecvText, "\n", "", [{return,list}]),
  Text = re:replace(RecvTextNoRC, "[a-zA-Z0-9]{3,}+>>", "", [{return,list}]),
  Recv = re:replace(RecvTextNoRC, ">>.*", "", [{return,list}]),
  io:format("Recv:~s~n",[Recv]),
  io:format("Text:~s~n",[Text]),
  NameSendIsSet = chat_server_users:nameIsSet(Pid), % sender
  NameRecvIsSet = chat_server_users:nameIsSetByName(Recv), % receiver
  if
    not NameSendIsSet ->
      {ok, "name send not set\n"};
    not NameRecvIsSet ->
      {ok, "name recv not set\n"};
    true ->
      NameSend = chat_server_users:getName(Pid),
      PidRecv = chat_server_users:getPidByName(Recv),
      StrToSend = "--msgFrom:" ++ NameSend ++ ">" ++ Text ++ "\n",
      chat_server_socket:sendByPid([PidRecv], StrToSend),
      {ok, "ok\n"}
  end.

cmd_default(Data) ->
  io:format("[cmd_default] ~s~n",[Data]),
  {ok, Data ++ "\n"}.

is_valid_name(S) ->
  case re:run(S, ?ReValidName) of
    {match, _} -> true;
    nomatch -> false
  end.

cmd_name_not_set() ->
  io:format("cmd_name_not_set~n"),
  Resp = "name is not set; please set name, example <<user,setname>>somename123\n",
  {ok, Resp}.

cmd_room_list() ->
  io:format("cmd_room_list~n"),
  Rooms = chat_server_rooms:get_all_rooms(),
  io:format("~p~n",[Rooms]),
  Resp = "rooms:" ++ string:join(Rooms,",") ++ "\n",
  {ok, Resp}.

cmd_room_create(Data) ->
  io:format("cmd_room_create~n"),
  Name = re:replace(Data, "<<room,create>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test1 = is_valid_name(Trimmed),
  Test2 = chat_server_rooms:is_room_available(Trimmed),
  if
    not Test1 ->
      {ok, ?StrNameNotValid ++ "\n"};
    Test2 ->
      {ok, ?StrNameAlreadyUsed ++ "\n"};   
    true ->
      chat_server_rooms:put_room(Trimmed),
      {ok, "ok\n"}
  end.

cmd_room_delete(Data) ->
  io:format("cmd_room_delete~n"),
  Name = re:replace(Data, "<<room,delete>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test = chat_server_rooms:is_room_available(Trimmed),
  if
    not Test ->
      {ok, ?StrRoomNotFound ++ "\n"};   
    true ->
      chat_server_rooms:delete_room(Trimmed),
      {ok, "ok\n"}
  end.

cmd_room_join(Pid, Data) ->
  io:format("cmd_room_join~n"),
  Name = re:replace(Data, "<<room,join>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test1 = chat_server_rooms:is_room_available(Trimmed),
  Test2 = chat_server_rooms:is_user_in_room(Trimmed, Pid),
  if
    not Test1 ->
      {ok, ?StrRoomNotFound ++ "\n"}; 
    Test2 ->
      {ok, "user already in room" ++ "\n"};  
    true ->
      chat_server_rooms:join_room(Trimmed, Pid),
      {ok, "ok\n"}
  end.

cmd_room_leave(Pid, Data) ->
  io:format("cmd_room_leave~n"),
  Name = re:replace(Data, "<<room,leave>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test1 = chat_server_rooms:is_room_available(Trimmed),
  Test2 = chat_server_rooms:is_user_in_room(Trimmed, Pid),
  if
    not Test1 ->
      {ok, ?StrRoomNotFound ++ "\n"};  
    not Test2 ->
      {ok, "user not in room" ++ "\n"}; 
    true ->
      chat_server_rooms:leave_room(Trimmed, Pid),
      {ok, "ok\n"}
  end.

cmd_room_userlist(Data) ->
  io:format("cmd_room_userlist~n"),
  Name = re:replace(Data, "<<room,userlist>>", "", [{return,list}]),
  Trimmed = string:trim(Name),
  Test1 = chat_server_rooms:is_room_available(Trimmed),
  if
    not Test1 ->
      {ok, ?StrRoomNotFound ++ "\n"};  
    true ->
      UsersPid = chat_server_rooms:get_users(Trimmed),
      UsersName = chat_server_users:from_pid_to_name(UsersPid),
      Resp = "userlist in room:" ++ string:join(UsersName,",") ++ "\n",
      {ok, Resp}
  end.

cmd_room_msg(Pid, Data) ->
  io:format("cmd_room_msg~n"),
  RoomText = re:replace(Data, "<<room,msg,", "", [{return,list}]),
  RoomTextTrimmed = string:trim(RoomText),
  Text = re:replace(RoomTextTrimmed, "[a-zA-Z0-9]{3,}+>>", "", [{return,list}]),
  Room = re:replace(RoomTextTrimmed, ">>.*", "", [{return,list}]),
  io:format("Room:~s~n",[Room]),
  io:format("Text:~s~n",[Text]),
  Test1 = chat_server_rooms:is_room_available(Room),
  if
    not Test1 ->
      {ok, ?StrRoomNotFound ++ "\n"};  
    true ->
      UsersPid = chat_server_rooms:get_users(Room),
      NameSend = chat_server_users:getName(Pid),
      StrToSend = "--msgFrom:" ++ NameSend ++ ">" ++ Text ++ "\n",
      chat_server_socket:sendByPid(UsersPid, StrToSend),
      {ok, "ok\n"}
  end.

cmd_room_belong(Pid) ->
  io:format("cmd_room_belong~n"),
  Rooms = chat_server_rooms:get_all_rooms_by_user(Pid),
  io:format("~p~n",[Rooms]),
  Resp = "rooms belong:" ++ string:join(Rooms,",") ++ "\n",
  {ok, Resp}.
