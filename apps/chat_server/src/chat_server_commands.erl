-module(chat_server_commands).
-export([cmd_user_list/0, cmd_user_setname/2, cmd_user_whoami/1, cmd_msg/2, cmd_default/1]).
-define(ReValidName, "^[a-zA-Z0-9]{3,}+$").
-define(RePatternUserlist, "<<user,list>>.*").
-define(RePatternWhoami, "<<user,whoami>>.*").
-define(RePatternSetname, "<<user,setname>>.+").
-define(RePatternMsg, "<<msg,[a-zA-Z0-9]{3,}+>>+").

cmd_user_list() ->
  io:format("[cmd] <<user,list>>~n",[]),
  Names = chat_server_users:getAllNames(),
  io:format("~p~n",[Names]),
  Resp = "users:" ++ string:join(Names,",") ++ "\n",
  {ok, Resp}.

cmd_user_setname(Pid, Data) ->
  io:format("[cmd] <<user,setname>>~n",[]),
  N1 = re:replace(Data, "<<user,setname>>", "", [{return,list}]),
  N2 = string:trim(N1),
  Ntest1 = is_valid_name(N2),
  Ntest2 = chat_server_users:nameIsSetByName(N2),
  if
    not Ntest1 ->
      {ok, "name is not valid; at least 3 chars of letters/numbers\n"};
    Ntest2 ->
      {ok, "name is already used\n"};   
    true ->
      chat_server_users:putName(Pid, N2),
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
