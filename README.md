Simple Chat Server
=====

Simple chat server on port 7000<br /> 
An Erlang/OTP 22 application

Build & Launch
-----
By rebar3 shell<br /> 
```rebar3 shell```

By rebar3 release<br /> 
```
rebar3 release
./_build/default/rel/chat_server/bin/chat_server foreground
```

Client Commands
-----
Telnet can be used to chat with the server

Below the available commands (I hope they are self explicative)
```
<<user,setname>>
<<user,whoami>>
<<user,list>>
<<msg,{username}>>{textmsg}
<<room,list>>
<<room,create>>{roomname}
<<room,delete>>{roomname}
<<room,userlist>>{roomname}
<<room,join>>{roomname}
<<room,leave>>{roomname}
<<room,msg,{roomname}>>{textmsg}
<<room,belong>>
regex for name user / room: [a-zA-Z0-9]{3,}
```

Note: as soon as you connect to the server, you have to setname, otherwise the all other commands will be forbidden

Chat responses:<br />
if the command sent if not present in the list, server respond with the same text sent;<br />
if the command is correct, the answer is "ok" for PUT / DELETE / UPDATE request (ex <<user,setname>>), arbitrary data for GET request (ex <<user,list>>)
