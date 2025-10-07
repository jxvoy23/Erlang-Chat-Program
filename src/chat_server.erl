-module(chat_server).
-export([start/0, send_broadcast/2]).
-export([server_loop/1]). % Exported for spawn/3

%% Starts the server process and registers it locally.
start() ->
  % Spawn the server loop and register it immediately.
  % register/2 is used to assign the atom 'chat_server' to the PID.
  ServerPid = spawn(?MODULE, server_loop, [[]]),
  register(chat_server, ServerPid),

  io:format("~p: Chat Server started and registered as 'chat_server'.~n", [ServerPid]),

  % Return the PID immediately so the shell is not blocked.
  ServerPid.

%% Sends a message to all connected clients.
send_broadcast(Clients, Message) ->
  lists:foreach(
    fun({_Nick, Pid}) ->
      Pid ! Message
    end, Clients).

%% The main infinite loop where the server waits for messages.
%% State is the list of connected clients: [{Nickname, Pid}]
server_loop(Clients) ->
  receive
  % A client wants to join
    {join, ClientPid, Nickname} ->
      io:format("~p: Client ~s joined (~p).~n", [self(), Nickname, ClientPid]),

      % Send a welcome message to the new client
      ClientPid ! {welcome, self()},

      % Broadcast the join event to existing clients
      send_broadcast(Clients, {system, Nickname, Nickname ++ " has joined the chat."}),

      % Add the new client to the state and continue the loop
      NewClients = [{Nickname, ClientPid} | Clients],
      server_loop(NewClients);

  % A client wants to send a message
    {msg, SenderPid, SenderNick, Text} ->
      io:format("~p: Message received from ~s: ~s~n", [self(), SenderNick, Text]),

      % Broadcast the message to all clients *except* the sender.
      send_broadcast(
        lists:keydelete(SenderNick, 1, Clients), % Remove sender from broadcast list
        {user_msg, SenderNick, Text}
      ),
      server_loop(Clients);

  % The client process died/exited
    {'EXIT', ClientPid, _Reason} ->
      % Find the nickname of the exited client
      case lists:keyfind(ClientPid, 2, Clients) of
        {Nickname, _} ->
          io:format("~p: Client ~s (~p) disconnected.~n", [self(), Nickname, ClientPid]),

          % Broadcast the exit event
          send_broadcast(lists:keydelete(Nickname, 1, Clients),
            {system, Nickname, Nickname ++ " has left the chat."}),

          % Remove the client from the state and continue
          NewClients = lists:keydelete(Nickname, 1, Clients),
          server_loop(NewClients);
        false ->
          % The PID wasn't a registered client, ignore.
          server_loop(Clients)
      end;

  % Unknown message received
    Unknown ->
      io:format("~p: Received unknown message: ~p~n", [self(), Unknown]),
      server_loop(Clients)
  end.
