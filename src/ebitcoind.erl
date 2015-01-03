%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2015, Eric des Courtis
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2015 6:22 PM
%%%-------------------------------------------------------------------
-module(ebitcoind).
-author("Eric des Courtis").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, getinfo/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER,                  ?MODULE).
-define(TIMEOUT,                 30000  ).
-define(SEED_BYTES,              16     ).
-define(HTTP_REQUEST_TIMEOUT,    30000  ).
-define(HTTP_CONNECTION_TIMEOUT, 3000   ).


-record(bitcoin_jsonrpc_config, {
    user     = <<"">>          :: binary(),
    password = <<"">>          :: binary(),
    host     = <<"127.0.0.1">> :: binary(),
    port     = 8332            :: pos_integer(),
    ssl      = false           :: boolean()
}).

-type bitcoin_jsonrpc_config() :: #bitcoin_jsonrpc_config{}.

-record(state, {
    config          :: bitcoin_jsonrpc_config(),
    seed = crypto:strong_rand_bytes(?SEED_BYTES)
                    :: binary()
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    {ok, [[HomePath|_]]} = init:get_argument(home),
    BitcoinConfPath = filename:join([HomePath, ".bitcoin", "bitcoin.conf"]),
    true = filelib:is_regular(BitcoinConfPath),
    Config = get_settings_from_bitcoin_conf(BitcoinConfPath),
    gen_server:start_link(?MODULE, [Config], []).

-spec(start_link(binary()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Url) when is_binary(Url) ->
    Config = url_to_bitcoin_jsonrpc_config(
        unicode:characters_to_list(Url)
    ),
    gen_server:start_link(?MODULE, [Config], []).


getinfo(Pid) ->
    gen_server:call(Pid, getinfo, ?TIMEOUT).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Config = #bitcoin_jsonrpc_config{}]) ->
    {ok, #state{config = Config}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State = #state{seed = Seed, config = Config})
    when is_atom(Request) ->
    Method = erlang:atom_to_binary(Request, utf8),
    Params = [],
    JsonReq = jsonrpc2_client:create_request({Method, Params, seed_to_utf8(Seed)}),
    {
        reply,
        do_jsonrpc_request(JsonReq, Config),
        State#state{seed = increment_seed(Seed)}
    };
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_settings_from_bitcoin_conf(BitcoinConfPath) ->
    {ok, Data} = file:read_file(BitcoinConfPath),
    Settings = [
        fun() ->
            [H | T] = string:tokens(string:strip(X), "="),
            {H, string:join(T, "=")} end()
        || X <- string:tokens(unicode:characters_to_list(Data), "\r\n"),
        hd(X) =/= $#
    ],
    G  = fun(Key, Default) -> proplists:get_value(Key, Settings, Default) end,
    C  = fun unicode:characters_to_binary/1,
    I  = fun erlang:list_to_integer/1,
    LI = fun erlang:integer_to_list/1,
    B  = fun("true") -> true; ("false") -> false end,
    LB = fun(true) -> "true"; (false) -> "false" end,
    Def = #bitcoin_jsonrpc_config{},
    #bitcoin_jsonrpc_config{
        user     = C(G("rpcuser"    ,    Def#bitcoin_jsonrpc_config.user    )),
        password = C(G("rpcpassword",    Def#bitcoin_jsonrpc_config.password)),
        host     = C(G("rpcconnect" ,    Def#bitcoin_jsonrpc_config.host    )),
        port     = I(G("rpcport"    , LI(Def#bitcoin_jsonrpc_config.port)   )),
        ssl      = B(G("rpcssl"     , LB(Def#bitcoin_jsonrpc_config.ssl)    ))
    }.

url_to_bitcoin_jsonrpc_config(Url) when is_list(Url) ->
    {ok, {HttpOrHttps, UserPass, Host, Port, _, _}} = http_uri:parse(Url),
    {Username, Password} = userpass_to_username_and_password(UserPass),
    #bitcoin_jsonrpc_config{
        user     = Username,
        password = Password,
        host     = Host,
        port     = Port,
        ssl      = case HttpOrHttps of https -> true; http -> false end
    }.

userpass_to_username_and_password(UserPass) when is_list(UserPass) ->
    userpass_to_username_and_password(string:chr(UserPass, $:), UserPass).

userpass_to_username_and_password(0, []) ->
    Def = #bitcoin_jsonrpc_config{},
    {Def#bitcoin_jsonrpc_config.user, Def#bitcoin_jsonrpc_config.password};
userpass_to_username_and_password(0, User) ->
    Def = #bitcoin_jsonrpc_config{},
    {User, Def#bitcoin_jsonrpc_config.password};
userpass_to_username_and_password(Pos, UserPass) ->
    User = string:substr(UserPass, Pos + 1),
    Pass = string:substr(UserPass, 1, Pos - 1),
    {User, Pass}.

increment_seed(<<Num:?SEED_BYTES/unsigned-integer-unit:8>>) ->
    <<(Num + 1):?SEED_BYTES/unsigned-integer-unit:8>>;
increment_seed(Bin) when is_binary(Bin) ->
    crypto:strong_rand_bytes(?SEED_BYTES).

seed_to_utf8(Seed) when is_binary(Seed) ->
    base64:encode(Seed).

do_jsonrpc_request(JsonReq, Config) ->
    Url = unicode:characters_to_list(bitcoin_jsonrpc_config_to_url(Config)),
    Body = iolist_to_binary(mochijson2:encode(JsonReq)),
    ContentType = "application/json",
    C = fun unicode:characters_to_list/1,
    Headers = [
        {"Authorization",
            "Basic " ++ base64:encode_to_string(
                C(Config#bitcoin_jsonrpc_config.user)
                ++ ":" ++
                C(Config#bitcoin_jsonrpc_config.password)
            )
        }
    ],
    Request = {Url, Headers, ContentType, Body},
    HTTPOptions = [
        {timeout,         ?HTTP_REQUEST_TIMEOUT   },
        {connect_timeout, ?HTTP_CONNECTION_TIMEOUT},
        {autoredirect,    true                    }
    ],
    Options = [],
    lager:info(
        "Request: ~p, HTTPOptions: ~p Options: ~p with URL: ~ts",
        [Request, HTTPOptions, Options, Url]
    ),
    {ok, Response = {
        {_HTTPVersion, _StatusCode = 200, _StateText},
        _RespHeaders,
        RespBody}
    } = httpc:request(post, Request, HTTPOptions, Options),
    lager:info(
        "Request: ~p, Response: ~p for Config: ~p with URL: ~ts",
        [Request, Response, Config, Url]
    ),
    RespDecoded = jsxn:decode(unicode:characters_to_binary(RespBody)),
    maps:get(<<"result">>, RespDecoded, RespDecoded).


bitcoin_jsonrpc_config_to_url(#bitcoin_jsonrpc_config{
    user = _User,
    password = _Pass,
    host = Host,
    port = Port,
    ssl = Ssl
}) ->
    <<
        (case Ssl of
             true -> <<"https">>;
             false -> <<"http">>
         end)/binary, <<"://">>/binary,
        %%User/binary, <<":">>/binary, Pass/binary,
        %%<<"@">>/binary,
        Host/binary, <<":">>/binary,
        (erlang:integer_to_binary(Port))/binary,
        <<"/">>/binary
    >>.