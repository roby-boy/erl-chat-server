-module(chat_server_manager).
-export([manage_cmd_get_resp/2]).

-define(ReValidName, "[a-zA-Z0-9]{3,}").
-define(RePatternUserlist, "<<user,list>>+").
-define(RePatternWhoami, "<<user,whoami>>+").
-define(RePatternSetname, "<<user,setname>>+").
-define(RePatternMsg, "<<msg," ++ ?ReValidName ++ ">>+").
-define(RePatternRoomlist,"<<room,list>>+").
-define(RePatternRoomCreate, "<<room,create>>+").
-define(RePatternRoomDelete, "<<room,delete>>+").
-define(RePatternRoomUserlist, "<<room,userlist>>+").
-define(RePatternRoomJoin, "<<room,join>>+").
-define(RePatternRoomLeave, "<<room,leave>>+").
-define(RePatternRoomMsg, "<<room,msg," ++ ?ReValidName ++ ">>+").
-define(RePatternRoomBelong, "<<room,belong>>+").

manage_cmd_get_resp(Data, Pid) ->
  Str = binary_to_list(string:chomp(Data)),
  Pattern_user_setname = pattern_user_setname(Data),
  Pattern_user_list_res = pattern_user_list(Data),
  Pattern_user_whoami = pattern_user_whoami(Data),
  Pattern_msg = pattern_msg(Data),
  Pattern_room_list = pattern_room_list(Data),
  Pattern_room_create = pattern_room_create(Data),
  Pattern_room_delete = pattern_room_delete(Data),
  Pattern_room_userlist = pattern_room_userlist(Data),
  Pattern_room_join = pattern_room_join(Data),
  Pattern_room_leave = pattern_room_leave(Data),
  Pattern_room_msg = pattern_room_msg(Data),
  Pattern_room_belong = pattern_room_belong(Data),
  IsNameSet = chat_server_users:nameIsSet(Pid),
  if
    Pattern_user_setname ->
      chat_server_commands:cmd_user_setname(Pid, Data);
    not IsNameSet -> % avoid all below commands without name
      chat_server_commands:cmd_name_not_set();
    Pattern_user_list_res ->
      chat_server_commands:cmd_user_list();
    Pattern_user_whoami ->
      chat_server_commands:cmd_user_whoami(Pid);
    Pattern_msg ->
      chat_server_commands:cmd_msg(Pid, Data);
    Pattern_room_list ->
      chat_server_commands:cmd_room_list();
    Pattern_room_create ->
      chat_server_commands:cmd_room_create(Data);
    Pattern_room_delete ->
      chat_server_commands:cmd_room_delete(Data);
    Pattern_room_userlist ->
      chat_server_commands:cmd_room_userlist(Data);
    Pattern_room_join ->
      chat_server_commands:cmd_room_join(Pid, Data);
    Pattern_room_leave ->
      chat_server_commands:cmd_room_leave(Pid, Data);
    Pattern_room_msg ->
      chat_server_commands:cmd_room_msg(Pid, Data);
    Pattern_room_belong ->
      chat_server_commands:cmd_room_belong(Pid);
    true ->
      chat_server_commands:cmd_default(Str)
  end.

pattern_user_setname(Data) ->
  case re:run(Data, ?RePatternSetname) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_list(Data) ->
  case re:run(Data, ?RePatternUserlist) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_user_whoami(Data) ->
  case re:run(Data, ?RePatternWhoami) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_msg(Data) ->
  case re:run(Data, ?RePatternMsg) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_list(Data) ->
  case re:run(Data, ?RePatternRoomlist) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_create(Data) ->
  case re:run(Data, ?RePatternRoomCreate) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_delete(Data) ->
  case re:run(Data, ?RePatternRoomDelete) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_userlist(Data) ->
  case re:run(Data, ?RePatternRoomUserlist) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_join(Data) ->
  case re:run(Data, ?RePatternRoomJoin) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_leave(Data) ->
  case re:run(Data, ?RePatternRoomLeave) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_msg(Data) ->
  case re:run(Data, ?RePatternRoomMsg) of
    {match, _} -> true;
    nomatch -> false
  end.

pattern_room_belong(Data) ->
  case re:run(Data, ?RePatternRoomBelong) of
    {match, _} -> true;
    nomatch -> false
  end.
