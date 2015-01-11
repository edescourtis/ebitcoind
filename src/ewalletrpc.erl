%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @author Patrick Begley
%%% @copyright (C) 2015, Eric des Courtis
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2015 6:22 PM
%%% Based off API documentation @
%%%  https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_calls_list
%%%
%%%
%%%-------------------------------------------------------------------
-module(ewalletrpc).
-author("Eric des Courtis").
-author("Patrick Begley").

-behaviour(gen_server).

-include("ewalletrpc.hrl").

-type bitcoin_jsonrpc_config() :: #ewallet_jsonrpc_config{}.


-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER,                  ?MODULE).
-define(SEED_BYTES,              16     ).
-define(HTTP_REQUEST_TIMEOUT,    30000  ).
-define(HTTP_CONNECTION_TIMEOUT, 3000   ).

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
-spec(start_link(bitcoin_jsonrpc_config()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Config = #ewallet_jsonrpc_config{}) ->
    gen_server:start_link(?MODULE, [Config], []).


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
init([Config = #ewallet_jsonrpc_config{}]) ->
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
handle_call(Request, _From, State)
    when is_atom(Request) ->
    handle_rpc_request(Request, [], State);
handle_call( {Request, Params}, _From, State)
    when is_atom(Request), is_list(Params) ->
    handle_rpc_request(Request, Params, State);
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
handle_rpc_request( Request, Params, State = #state{seed = Seed, config = Config}) ->
    Method = erlang:atom_to_binary(Request, utf8),
    JsonReq = jsonrpc2_client:create_request(
        {Method, mochijson2:decode(jsxn:encode(Params)), seed_to_utf8(Seed)}
    ),
    {
        reply,
        do_jsonrpc_request(JsonReq, Config),
        State#state{seed = increment_seed(Seed)}
    }.


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
                C(Config#ewallet_jsonrpc_config.user)
                ++ ":" ++
                C(Config#ewallet_jsonrpc_config.password)
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
    lager:debug(
        "Request: ~p, HTTPOptions: ~p Options: ~p with URL: ~ts",
        [Request, HTTPOptions, Options, Url]
    ),
    {ok, Response = {
        {_HTTPVersion, StatusCode, StatusText},
        _RespHeaders,
        RespBody}
    } = httpc:request(post, Request, HTTPOptions, Options),
    lager:debug(
        "Request: ~p, Response: ~p for Config: ~p with URL: ~ts",
        [Request, Response, Config, Url]
    ),
    RespDecoded = jsxn:decode(unicode:characters_to_binary(RespBody)),
    case StatusCode of
        GoodStatus when (GoodStatus >= 200) and (GoodStatus =< 299)->
            {ok, maps:get(<<"result">>, RespDecoded, RespDecoded)};
        _ ->
            lager:warning(
                "HTTP returned status ~p ~ts from Request: ~p with Response: ~p",
                [StatusCode, StatusText, Request, RespDecoded]
            ),
            {error, maps:get(<<"error">>, RespDecoded, RespDecoded)}
    end.



bitcoin_jsonrpc_config_to_url(#ewallet_jsonrpc_config{
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