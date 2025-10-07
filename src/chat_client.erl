-module(chat_client).
-export([start/1, send/2, stop/1]). %% <-- UPDATED EXPORT LIST
-export([client_loop/1]).          %% <-- EXPORTED FOR SPAWN/3

%% Starts a new client process with the given Nickname (string).
start(Nickname) ->
  % Spawn a new process and link it to the current shell process.
  ClientPid = spawn_link(?MODULE, client_loop, [Nickname]),

  % Register the client's PID globally using the nickname as an atom.
  register(list_to_atom(Nickname), ClientPid),

  io:format("Client ~s (~p) started. Use chat_client:send('Nickname', \"Message\") to talk.~n",
    [Nickname, ClientPid]),

  ClientPid.

%% Sends a text message from the shell using the registered Nickname Atom.
send(NicknameAtom, Text) when is_atom(NicknameAtom) ->
  % 1. Find the PID of the client process registered under NicknameAtom
  ClientPid = whereis(NicknameAtom),

  % 2. If found, send a message {send_msg, Text} directly to that client process.
  if ClientPid /= undefined ->
    ClientPid ! {send_msg, Text};
    true ->
      io:format("ERROR: Client ~p not found or registered.~n", [NicknameAtom])
  end.

%% Stops the client process using its registered name (atom).
stop(NicknameAtom) ->
  ClientPid = whereis(NicknameAtom),
  if ClientPid /= undefined ->
    exit(ClientPid, normal);
    true ->
      io:format("ERROR: Client ~p not found.~n", [NicknameAtom])
  end.


%% --- Internal Functions ---

%% The main loop for the client process, run by spawn_link.
client_loop(Nickname) ->
  ServerPid = whereis(chat_server),

  if ServerPid == undefined ->
    io:format("~p: ERROR: Server 'chat_server' not running!~n", [self()]),
    exit(no_server_running);
    true ->
      % Save the server PID and Nickname in the process dictionary
      put(server_pid, ServerPid),
      put(nickname, Nickname),

      % Send the join request to the server
      ServerPid ! {join, self(), Nickname},

      % Enter the receive loop
      receive_loop()
  end.

%% The receive loop continuously waits for messages (from server OR shell command).
receive_loop() ->
  receive
  % NEW: Message received from the shell/user to send a chat message
    {send_msg, Text} ->
      Nickname = get(nickname),
      ServerPid = get(server_pid),
      % Forward the message to the server
      ServerPid ! {msg, self(), Nickname, Text},
      receive_loop();

  % Received the welcome message from the server
    {welcome, ServerPid} ->
      io:format("~p: *** Connected to server ~p ***~n", [self(), ServerPid]),
      receive_loop();

  % Received a broadcasted system announcement
    {system, _Sender, Text} ->
      io:format("~p: [SYSTEM] ~s~n", [self(), Text]),
      receive_loop();

  % Received a broadcasted user message
    {user_msg, SenderNick, Text} ->
      io:format("~p: <~s> ~s~n", [self(), SenderNick, Text]),
      receive_loop();

  % Received an unexpected message
    Unknown ->
      io:format("~p: Received unknown message: ~p~n", [self(), Unknown]),
      receive_loop()
  after 1000 ->
    receive_loop()
  end.
