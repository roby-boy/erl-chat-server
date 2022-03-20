Simple Chat Server
=====

Simple chat server on port 7000<br /> 
An Erlang/OTP 22

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