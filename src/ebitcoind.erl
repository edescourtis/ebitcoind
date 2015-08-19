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
-module(ebitcoind).
-author("Eric des Courtis").
-author("Patrick Begley").

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         addmultisigaddress/3,
         addmultisigaddress/4,
         addnode/3,
         backupwallet/2,
         createmultisig/3,
         createrawtransaction/3,
         decoderawtransaction/2,
         dumpprivkey/2,
         encryptwallet/2,
         getaccount/2,
         getaccountaddress/2,
         getaddednodeinfo/2,
         getaddednodeinfo/3,
         getaddressesbyaccount/2,
         getbalance/1,
         getbalance/2,
         getbalance/3,
         getbestblockhash/1,
         getblock/2,
         getblockcount/1,
         getblockhash/2,
         %getblocknumber/1,
         getblocktemplate/1,
         getblocktemplate/2,
         getconnectioncount/1,
         getdifficulty/1,
         getgenerate/1,
         gethashespersec/1,
         getinfo/1,
         %getmemorypool/2,
         getmininginfo/1,
         getnewaddress/1,
         getnewaddress/2,
         getpeerinfo/1,
         getrawchangeaddress/1,
         getrawchangeaddress/2,
         getrawmempool/1,
         getrawtransaction/2,
         getrawtransaction/3,
         getreceivedbyaccount/1,
         getreceivedbyaccount/2,
         getreceivedbyaccount/3,
         getreceivedbyaddress/2,
         getreceivedbyaddress/3,
         gettransaction/2,
         gettxout/3,
         gettxout/4,
         gettxoutsetinfo/1,
         getwork/1,
         getwork/2,
         help/1,
         help/2,
         importprivkey/2,
         importprivkey/3,
         importprivkey/4,
         keypoolrefill/1,
         listaccounts/1,
         listaccounts/2,
         listaddressgroupings/1,
         listreceivedbyaccount/1,
         listreceivedbyaccount/2,
         listreceivedbyaccount/3,
         listreceivedbyaddress/1,
         listreceivedbyaddress/2,
         listreceivedbyaddress/3,
         listsinceblock/1,
         listsinceblock/2,
         listsinceblock/3,
         listtransactions/1,
         listtransactions/2,
         listtransactions/3,
         listtransactions/4,
         listunspent/1,
         listunspent/2,
         listunspent/3,
         listlockunspent/1,
         lockunspent/2,
         lockunspent/3,
         move/4,
         move/5,
         move/6,
         sendfrom/4,
         sendfrom/5,
         sendfrom/6,
         sendfrom/7,
         sendmany/3,
         sendmany/4,
         sendmany/5,
         sendrawtransaction/2,
         sendtoaddress/3,
         sendtoaddress/4,
         sendtoaddress/5,
         setaccount/3,
         setgenerate/2,
         setgenerate/3,
         settxfee/2,
         signmessage/3,
         signrawtransaction/2,
         signrawtransaction/3,
         signrawtransaction/4,
         stop/1,
         submitblock/2,
         submitblock/3,
         validateaddress/2,
         verifymessage/4,
         walletlock/1,
         walletpassphrase/3,
         walletpassphrasechange/3
        ]).

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


-spec( addmultisigaddress(pid(), pos_integer(), [binary()] ) -> {ok, binary()} | {error, term()} ).
addmultisigaddress( Pid, NRequired, Keys )
    when is_pid(Pid), is_integer(NRequired), is_list(Keys), NRequired > 0 ->
    gen_server:call(Pid, {addmultisigaddress, [NRequired,Keys]}, ?TIMEOUT).
-spec( addmultisigaddress(pid(), pos_integer(), [binary()], binary() ) -> {ok, binary()} | {error, term()} ).
addmultisigaddress( Pid, NRequired, Keys, Account )
    when is_pid(Pid), is_integer(NRequired), is_list(Keys), NRequired > 0, is_binary(Account) ->
    gen_server:call(Pid, {addmultisigaddress, [NRequired,Keys,Account]}, ?TIMEOUT).

-spec( addnode(pid(), binary(), add|remove|onetry) -> {ok, term()} | {error, term()} ).
addnode( Pid, Node, Action )
    when is_pid(Pid),
         is_binary(Node),
         ( Action =:= add ) or (Action =:= remove) or (Action =:= onetry ) ->
    gen_server:call( Pid, {addnode, [Node, erlang:atom_to_binary(Action,utf8)]}, ?TIMEOUT).

-spec( backupwallet(pid(), binary() ) -> {ok, term()} | {error, term()} ).
backupwallet( Pid, Destination )
    when is_pid(Pid),
         is_binary(Destination) ->
    gen_server:call(Pid, {backupwallet, [Destination]}, ?TIMEOUT).

-spec( createmultisig(pid(), pos_integer(), [binary()]) -> {ok, map()} | {error, term()} ).
createmultisig( Pid, NRequired, Keys )
    when is_pid(Pid),
         is_integer(NRequired),
         is_list(Keys),
         NRequired > 0 ->
    gen_server:call(Pid, {createmultisig, [NRequired, Keys]}, ?TIMEOUT).

-spec( createrawtransaction(pid(), [map()], map()) -> {ok, binary()} | {error, term()} ).
createrawtransaction( Pid, Inputs, Outputs )
    when is_pid(Pid),
         is_list(Inputs),
         is_map(Outputs) ->
    gen_server:call(Pid, {createrawtransaction, [Inputs, Outputs]}, ?TIMEOUT).

-spec( decoderawtransaction(pid(), binary() ) -> {ok, map()} | {error, term()} ).
decoderawtransaction( Pid, HexString )
    when is_pid(Pid),
         is_binary(HexString) ->
    gen_server:call(Pid, {decoderawtransaction, [HexString]}, ?TIMEOUT).

-spec( dumpprivkey(pid(), binary()) -> {ok, binary()} | {error, term()} ).
dumpprivkey( Pid, WalletAddress )
    when is_pid(Pid),
         is_binary(WalletAddress) ->
    gen_server:call(Pid, {dumpprivkey, [WalletAddress]}, ?TIMEOUT).

-spec( encryptwallet(pid(), binary() ) -> {ok, binary()} | {error, term()} ).
encryptwallet( Pid, PassPhrase )
    when is_pid(Pid),
         is_binary(PassPhrase) ->
    gen_server:call(Pid, {encryptwallet, [PassPhrase]}, ?TIMEOUT).

-spec( getaccount(pid(), binary() ) -> {ok, term()} | {error, term()} ).
getaccount( Pid, WalletAddress )
    when is_pid(Pid),
         is_binary(WalletAddress) ->
    gen_server:call(Pid, {getaccount, [WalletAddress]}, ?TIMEOUT).

-spec( getaccountaddress(pid(), binary()) -> {ok, binary()} | {error, term()} ).
getaccountaddress(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {getaccountaddress, [Account]}, ?TIMEOUT).


-spec( getaddednodeinfo(pid(), binary()) -> {ok, term()} | {error, term()} ).
getaddednodeinfo(Pid, DNS )
    when is_pid(Pid),
    is_binary(DNS) ->
    gen_server:call(Pid, {getaddednodeinfo, [DNS]}, ?TIMEOUT).
-spec( getaddednodeinfo(pid(), binary(), binary()) -> {ok, term()} | {error, term()} ).
getaddednodeinfo(Pid, DNS, Node )
    when is_pid(Pid),
    is_binary(DNS),
    is_binary(Node) ->
    gen_server:call(Pid, {getaddednodeinfo, [DNS, Node]}, ?TIMEOUT).


-spec( getaddressesbyaccount(pid(), binary()) -> {ok, [binary()]} | {error, term()}).
getaddressesbyaccount(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {getaddressesbyaccount, [Account]}, ?TIMEOUT).


-spec( getbalance(pid() ) -> {ok, float()} | {error, term()}).
getbalance(Pid) when is_pid(Pid) ->
    gen_server:call( Pid, getbalance, ?TIMEOUT).
-spec( getbalance(pid(), binary() ) -> {ok, float()} | {error, term()}).
getbalance(Pid, Account ) when is_pid(Pid), is_binary(Account) ->
    gen_server:call( Pid, {getbalance, [Account]}, ?TIMEOUT).
-spec( getbalance(pid(), binary(), integer() ) -> {ok, float()} | {error, term()}).
getbalance(Pid, Account, MinConf)
    when is_pid(Pid), is_binary(Account), is_integer(MinConf) ->
    gen_server:call( Pid, {getbalance, [Account, MinConf]}, ?TIMEOUT).


-spec( getbestblockhash(pid()) -> {ok, binary()} | {error, term()}).
getbestblockhash(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getbestblockhash, ?TIMEOUT).

-spec( getblock(pid(), binary()) -> {ok, term()} | {error, term()}).
getblock( Pid, Hash )
    when is_pid(Pid),
         is_binary(Hash) ->
    gen_server:call(Pid, {getblock, [Hash]}, ?TIMEOUT).

-spec( getblockcount(pid()) -> {ok, non_neg_integer()} | {error, term()}).
getblockcount(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getblockcount, ?TIMEOUT).


-spec( getblockhash(pid(), non_neg_integer()) -> {ok, binary()} | {error, term()}).
getblockhash(Pid, Index)
    when is_pid(Pid),
         is_integer(Index),
         Index >= 0 ->
    gen_server:call(Pid, {getblockhash, [Index]}, ?TIMEOUT).

%% Deprecated in 0.7
%-spec( getblocknumber(pid()) -> {ok, non_neg_integer()} | {error, term()} ).
%getblocknumber( Pid) when is_pid(Pid) ->
%    gen_server:call(Pid, getblocknumber, ?TIMEOUT).
%%

-spec( getblocktemplate(pid()) -> {ok, term()} | {error, term()}).
getblocktemplate(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, getblocktemplate, ?TIMEOUT).
-spec( getblocktemplate(pid(), term()) -> {ok, term()} | {error, term()}).
getblocktemplate(Pid, Params)
    when is_pid(Pid) ->
    gen_server:call(Pid, {getblocktemplate, [Params]}, ?TIMEOUT).


-spec( getconnectioncount(pid()) -> {ok, non_neg_integer()} | {error, term()} ).
getconnectioncount(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getconnectioncount, ?TIMEOUT).

-spec( getdifficulty(pid()) -> {ok, float()} | {error, term()} ).
getdifficulty(Pid) when is_pid( Pid) ->
    gen_server:call(Pid, getdifficulty, ?TIMEOUT).

-spec( getgenerate(pid()) -> {ok, boolean()} | {error, term()} ).
getgenerate(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getgenerate, ?TIMEOUT).

-spec( gethashespersec(pid()) -> {ok, non_neg_integer()} | {error, term()} ).
gethashespersec(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, gethashespersec, ?TIMEOUT).

-spec( getinfo(pid())-> {ok, map()} | {error, term()} ).
getinfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getinfo, ?TIMEOUT).


%% Replaced in v0.7.0 with getblocktemplate, submitblock, getrawmempool
%-spec( getmemorypool(pid(), term()) -> {ok, term()} | {error, term()}).
%getmemorypool(Pid, Data)
%    when is_pid(Pid) ->
%    gen_server:call(Pid, {getmemorypool, [Data]}, ?TIMEOUT).
%%

-spec( getmininginfo(pid()) -> {ok, map()} | {error, term()} ).
getmininginfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getmininginfo, ?TIMEOUT).

-spec( getnewaddress(pid()) -> {ok, binary()} | {error, term()} ).
getnewaddress(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, getnewaddress, ?TIMEOUT).
-spec( getnewaddress(pid(), binary()) -> {ok, binary()} | {error, term()} ).
getnewaddress(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {getnewaddress, [Account]}, ?TIMEOUT).

-spec( getpeerinfo(pid()) -> {ok, [map()]} | {error, term()} ).
getpeerinfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getpeerinfo, ?TIMEOUT).

-spec( getrawchangeaddress(pid()) -> {ok, binary()} | {error, term()}).
getrawchangeaddress(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, getrawchangeaddress, ?TIMEOUT).
-spec( getrawchangeaddress(pid(), binary()) -> {ok, binary()} | {error, term()}).
getrawchangeaddress(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {getrawchangeaddress, [Account]}, ?TIMEOUT).

-spec( getrawmempool(pid()) -> {ok, list()} | {error, term()} ).
getrawmempool(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getrawmempool, ?TIMEOUT).

-spec( getrawtransaction(pid(), binary()) -> {ok, term()} | {error, term()} ).
getrawtransaction(Pid, Txid)
    when is_pid(Pid),
    is_binary(Txid) ->
    gen_server:call(Pid, {getrawtransaction, [Txid]}, ?TIMEOUT).
-spec( getrawtransaction(pid(), binary(), non_neg_integer()) -> {ok, term()} | {error, term()} ).
getrawtransaction(Pid, Txid, Verbose)
    when is_pid(Pid),
    is_binary(Txid),
    is_integer(Verbose),
    Verbose >= 0 ->
    gen_server:call(Pid, {getrawtransaction, [Txid, Verbose]}, ?TIMEOUT).

-spec( getreceivedbyaccount(pid()) -> {ok, float()} | {error, term()} ).
getreceivedbyaccount(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, getreceivedbyaccount, ?TIMEOUT).
-spec( getreceivedbyaccount(pid(), binary()) -> {ok, float()} | {error, term()} ).
getreceivedbyaccount(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {getreceivedbyaccount, [Account]}, ?TIMEOUT).
-spec( getreceivedbyaccount(pid(), binary(), non_neg_integer()) -> {ok, float()} | {error, term()} ).
getreceivedbyaccount(Pid, Account, MinConfs)
    when is_pid(Pid),
         is_binary(Account),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {getreceivedbyaccount, [Account,MinConfs]}, ?TIMEOUT).

-spec( getreceivedbyaddress(pid(), binary()) -> {ok, float()} | {error, term()} ).
getreceivedbyaddress(Pid, WalletAddress)
    when is_pid(Pid),
         is_binary(WalletAddress) ->
    gen_server:call(Pid, {getreceivedbyaddress, [WalletAddress]}, ?TIMEOUT).
-spec( getreceivedbyaddress(pid(), binary(), non_neg_integer()) -> {ok, float()} | {error, term()} ).
getreceivedbyaddress(Pid, WalletAddress, MinConfs)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {getreceivedbyaddress, [WalletAddress, MinConfs]}, ?TIMEOUT).

-spec( gettransaction(pid(), binary()) -> {ok, map()} | {error, term()} ).
gettransaction(Pid, Txid)
    when is_pid(Pid),
         is_binary(Txid) ->
    gen_server:call(Pid, {gettransaction, [Txid]}, ?TIMEOUT).

-spec( gettxout(pid(), binary(), integer()) -> {ok, term()} | {error, term()} ).
gettxout(Pid, Txid, N)
    when is_pid(Pid),
    is_binary(Txid) ->
    gen_server:call(Pid, {gettxout, [Txid, N]}, ?TIMEOUT).
-spec( gettxout(pid(), binary(), integer(), boolean()) -> {ok, term()} | {error, term()} ).
gettxout(Pid, Txid, N, IncludeMemPool)
    when is_pid(Pid),
    is_binary(Txid),
    is_boolean(IncludeMemPool)->
    gen_server:call(Pid, {gettxout, [Txid, N, IncludeMemPool]}, ?TIMEOUT).

-spec( gettxoutsetinfo(pid()) -> {ok, map()} | {error, term()} ).
gettxoutsetinfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, gettxoutsetinfo, ?TIMEOUT).

-spec( getwork(pid()) -> {ok, map() | boolean()} | {error, term()}).
getwork(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, getwork, ?TIMEOUT).
-spec( getwork(pid(), term()) -> {ok, map() | boolean()} | {error, term()} ).
getwork(Pid, Data)
    when is_pid(Pid) ->
    gen_server:call(Pid, {getwork, [Data]}, ?TIMEOUT).

-spec( help(pid()) -> {ok, binary()} | {error, term()}).
help(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, help, ?TIMEOUT).
-spec( help(pid(), binary()) -> {ok, binary()} | {error, term()}).
help(Pid, Command)
    when is_pid(Pid),
         is_binary(Command) ->
    gen_server:call(Pid, {help, [Command]}, ?TIMEOUT).

-spec( importprivkey(pid(), binary()) -> {ok, term()} | {error, term()}).
importprivkey(Pid, PrivateKey)
    when is_pid(Pid),
         is_binary(PrivateKey) ->
    gen_server:call(Pid, {importprivkey, [PrivateKey]}, ?TIMEOUT).
-spec( importprivkey(pid(), binary(), binary()) -> {ok, term()} | {error, term()}).
importprivkey(Pid, PrivateKey, Label)
    when is_pid(Pid),
         is_binary(PrivateKey),
         is_binary(Label) ->
    gen_server:call(Pid, {importprivkey, [PrivateKey, Label]}, ?TIMEOUT).
-spec( importprivkey(pid(), binary(), binary(), boolean()) -> {ok, term()} | {error, term()}).
importprivkey(Pid, PrivateKey, Label, Rescan)
    when is_pid(Pid),
         is_binary(PrivateKey),
         is_binary(Label),
         is_boolean(Rescan) ->
    gen_server:call(Pid, {importprivkey, [PrivateKey, Label, Rescan]}, ?TIMEOUT).


-spec( keypoolrefill(pid()) -> {ok, null | term()} | {error, term()} ).
keypoolrefill(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, keypoolrefill, ?TIMEOUT).

-spec( listaccounts(pid()) -> {ok, map()} | {error, term()} ).
listaccounts(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listaccounts, ?TIMEOUT).
-spec( listaccounts(pid(), non_neg_integer()) -> {ok, map()} | {error, term()} ).
listaccounts(Pid, MinConfs)
    when is_pid(Pid),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {listaccounts, [MinConfs]}, ?TIMEOUT).

-spec( listaddressgroupings(pid()) -> {ok, list()} | {error, term()} ).
listaddressgroupings(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, listaddressgroupings, ?TIMEOUT).

-spec( listreceivedbyaccount(pid()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaccount(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listreceivedbyaccount, ?TIMEOUT).
-spec( listreceivedbyaccount(pid(), non_neg_integer()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaccount(Pid, MinConfs)
    when is_pid(Pid),
    is_integer(MinConfs),
    MinConfs >= 0 ->
    gen_server:call(Pid, {listreceivedbyaccount, [MinConfs]}, ?TIMEOUT).
-spec( listreceivedbyaccount(pid(), non_neg_integer(), boolean()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaccount(Pid, MinConfs, IncludeEmpty)
    when is_pid(Pid),
    is_integer(MinConfs),
    MinConfs >= 0,
    is_boolean(IncludeEmpty) ->
    gen_server:call(Pid, {listreceivedbyaccount, [MinConfs, IncludeEmpty]}, ?TIMEOUT).



-spec( listreceivedbyaddress(pid()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaddress(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listreceivedbyaddress, ?TIMEOUT).
-spec( listreceivedbyaddress(pid(), non_neg_integer()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaddress(Pid, MinConfs)
    when is_pid(Pid),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {listreceivedbyaddress, [MinConfs]}, ?TIMEOUT).
-spec( listreceivedbyaddress(pid(), non_neg_integer(), boolean()) -> {ok, [map()]} | {error, term()} ).
listreceivedbyaddress(Pid, MinConfs, IncludeEmpty)
    when is_pid(Pid),
         is_integer(MinConfs),
         MinConfs >= 0,
         is_boolean(IncludeEmpty) ->
    gen_server:call(Pid, {listreceivedbyaddress, [MinConfs, IncludeEmpty]}, ?TIMEOUT).

-spec( listsinceblock(pid()) -> {ok, term()} | {error, term()}).
listsinceblock(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listsinceblock, ?TIMEOUT).
-spec( listsinceblock(pid(), binary()) -> {ok, term()} | {error, term()}).
listsinceblock(Pid, BlockHash)
    when is_pid(Pid),
         is_binary(BlockHash)->
    gen_server:call(Pid, {listsinceblock, [BlockHash]}, ?TIMEOUT).
-spec( listsinceblock(pid(), binary(), non_neg_integer()) -> {ok, term()} | {error, term()}).
listsinceblock(Pid, BlockHash, TargetConfs)
    when is_pid(Pid),
         is_binary(BlockHash),
         is_integer(TargetConfs),
         TargetConfs >= 0 ->
    gen_server:call(Pid, {listsinceblock, [BlockHash,TargetConfs]}, ?TIMEOUT).

-spec( listtransactions(pid()) -> {ok, list()} | {error, term()}).
listtransactions(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listtransactions, ?TIMEOUT).
-spec( listtransactions(pid(), binary()) -> {ok, list()} | {error, term()}).
listtransactions(Pid, Account)
    when is_pid(Pid),
         is_binary(Account) ->
    gen_server:call(Pid, {listtransactions, [Account]}, ?TIMEOUT).
-spec( listtransactions(pid(), binary(), integer()) -> {ok, list()} | {error, term()}).
listtransactions(Pid, Account, Count)
    when is_pid(Pid),
         is_binary(Account),
         is_integer(Count),
         Count > 0 ->
    gen_server:call(Pid, {listtransactions, [Account, Count]}, ?TIMEOUT).
-spec( listtransactions(pid(), binary(), integer(), non_neg_integer()) -> {ok, list()} | {error, term()}).
listtransactions(Pid, Account, Count, From)
    when is_pid(Pid),
         is_binary(Account),
         is_integer(Count),
         Count > 0,
         is_integer(From),
         From >= 0 ->
    gen_server:call(Pid, {listtransactions, [Account, Count, From]}, ?TIMEOUT).

-spec( listunspent(pid()) -> {ok, list()} | {error, term()} ).
listunspent(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, listunspent, ?TIMEOUT).
-spec( listunspent(pid(), non_neg_integer()) -> {ok, list()} | {error, term()} ).
listunspent(Pid, MinConfs)
    when is_pid(Pid),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {listunspent, [MinConfs]}, ?TIMEOUT).
-spec( listunspent(pid(), non_neg_integer(), pos_integer()) -> {ok, list()} | {error, term()} ).
listunspent(Pid, MinConfs, MaxConfs)
    when is_pid(Pid),
         is_integer(MinConfs),
         MinConfs >= 0,
         is_integer(MaxConfs),
         (MaxConfs >= 0) and (MaxConfs =< 999999) ->
    gen_server:call(Pid, {listunspent, [MinConfs,MaxConfs]}, ?TIMEOUT).


-spec( listlockunspent(pid()) -> {ok, list()} | {error, term()} ).
listlockunspent(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, listlockunspent, ?TIMEOUT).

-spec( lockunspent(pid(), boolean()) -> {ok, term()} | {error, term()}).
lockunspent(Pid, Unlock)
    when is_pid(Pid),
         is_boolean(Unlock) ->
    gen_server:call(Pid, {lockunspent, [Unlock]}, ?TIMEOUT).
-spec( lockunspent(pid(), boolean(), [map()]) -> {ok, term()} | {error, term()}).
lockunspent(Pid, Unlock, Objects)
    when is_pid(Pid),
         is_boolean(Unlock),
         is_list(Objects) ->
    gen_server:call(Pid, {lockunspent, [Unlock,Objects]}, ?TIMEOUT).

-spec( move(pid(), binary(), binary(), float()) -> {ok, term()} | {error, term()}).
move(Pid, FromAccount, ToAccount, Amount)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToAccount),
         is_float(Amount),
         Amount > 0.00000001 ->
    gen_server:call(Pid, {move, [FromAccount, ToAccount, Amount]}, ?TIMEOUT).
-spec( move(pid(), binary(), binary(), float(), non_neg_integer()) ->
    {ok, term()} | {error, term()}).
move(Pid, FromAccount, ToAccount, Amount, MinConfs)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToAccount),
         is_float(Amount),
         Amount > 0.00000001,
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {move, [FromAccount, ToAccount, Amount, MinConfs]}, ?TIMEOUT).
-spec( move(pid(), binary(), binary(), float(), non_neg_integer(), binary()) ->
    {ok, term()} | {error, term()}).
move(Pid, FromAccount, ToAccount, Amount, MinConfs, Comment)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToAccount),
         is_float(Amount),
         Amount > 0.00000001,
         is_integer(MinConfs),
         MinConfs >= 0,
         is_binary(Comment)->
    gen_server:call(Pid, {move, [FromAccount, ToAccount, Amount, MinConfs, Comment]}, ?TIMEOUT).

-spec( sendfrom(pid(), binary(), binary(), float()) -> {ok, binary()} | {error, term()}).
sendfrom(Pid, FromAccount, ToWalletAddress, Amount)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToWalletAddress),
         is_float(Amount),
         Amount > 0.00000001 ->
    gen_server:call(Pid, {sendfrom, [FromAccount, ToWalletAddress, Amount]}, ?TIMEOUT).
-spec( sendfrom(pid(), binary(), binary(), float(), non_neg_integer()) -> {ok, binary()} | {error, term()}).
sendfrom(Pid, FromAccount, ToWalletAddress, Amount, MinConfs)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToWalletAddress),
         is_float(Amount),
         Amount > 0.00000001,
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {sendfrom, [FromAccount, ToWalletAddress, Amount, MinConfs]}, ?TIMEOUT).
-spec( sendfrom(pid(), binary(), binary(), float(), non_neg_integer(), binary()) -> {ok, binary()} | {error, term()}).
sendfrom(Pid, FromAccount, ToWalletAddress, Amount, MinConfs, Comment)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToWalletAddress),
         is_float(Amount),
         Amount > 0.00000001,
         is_integer(MinConfs),
         MinConfs >= 0,
         is_binary(Comment) ->
    gen_server:call(
            Pid,
            {sendfrom, [FromAccount, ToWalletAddress, Amount, MinConfs, Comment]},
            ?TIMEOUT
    ).
-spec( sendfrom(pid(), binary(), binary(), float(), non_neg_integer(), binary(), binary())
        -> {ok, binary()} | {error, term()}).
sendfrom(Pid, FromAccount, ToWalletAddress, Amount, MinConfs, Comment, ToComment)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_binary(ToWalletAddress),
         is_float(Amount),
         Amount > 0.00000001,
         is_integer(MinConfs),
         MinConfs >= 0,
         is_binary(Comment),
         is_binary(ToComment) ->
    gen_server:call(
        Pid,
        {sendfrom, [FromAccount, ToWalletAddress, Amount, MinConfs, Comment, ToComment]},
        ?TIMEOUT
    ).

-spec( sendmany(pid(), binary(), map()) -> {ok, [binary()]} | {error, term()}).
sendmany(Pid, FromAccount, Amounts)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_map(Amounts) ->
    gen_server:call(Pid, {sendmany, [FromAccount, Amounts]}, ?TIMEOUT).
-spec( sendmany(pid(), binary(), map(), non_neg_integer()) -> {ok, [binary()]} | {error, term()}).
sendmany(Pid, FromAccount, Amounts, MinConfs)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_map(Amounts),
         is_integer(MinConfs),
         MinConfs >= 0 ->
    gen_server:call(Pid, {sendmany, [FromAccount, Amounts, MinConfs]}, ?TIMEOUT).
-spec( sendmany(pid(), binary(), map(), non_neg_integer(), binary()) ->
    {ok, [binary()]} | {error, term()}).
sendmany(Pid, FromAccount, Amounts, MinConfs, Comment)
    when is_pid(Pid),
         is_binary(FromAccount),
         is_map(Amounts),
         is_integer(MinConfs),
         MinConfs >= 0,
         is_binary(Comment)->
    gen_server:call(Pid, {sendmany, [FromAccount, Amounts, MinConfs, Comment]}, ?TIMEOUT).

-spec( sendrawtransaction(pid(), binary()) -> {ok, term()} | {error, term()}).
sendrawtransaction(Pid, HexString)
    when is_pid(Pid),
         is_binary(HexString) ->
    gen_server:call(Pid, {sendrawtransaction, [HexString]}, ?TIMEOUT).

-spec( sendtoaddress(pid(), binary(), float()) -> {ok, binary()} | {error, term()}).
sendtoaddress(Pid, WalletAddress, Amount)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_float(Amount),
         Amount >= 0.00000001 ->
    gen_server:call(Pid, {sendtoaddress, [WalletAddress, Amount]}, ?TIMEOUT).
-spec( sendtoaddress(pid(), binary(), float(), binary()) ->
    {ok, binary()} | {error, term()}).
sendtoaddress(Pid, WalletAddress, Amount, Comment)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_float(Amount),
         Amount >= 0.00000001,
         is_binary(Comment) ->
    gen_server:call(Pid, {sendtoaddress, [WalletAddress, Amount, Comment]}, ?TIMEOUT).
-spec( sendtoaddress(pid(), binary(), float(), binary(), binary()) ->
    {ok, binary()} | {error, term()}).
sendtoaddress(Pid, WalletAddress, Amount, Comment, ToComment)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_float(Amount),
         Amount >= 0.00000001,
         is_binary(Comment),
         is_binary(ToComment) ->
    gen_server:call(Pid, {sendtoaddress, [WalletAddress, Amount, Comment, ToComment]}, ?TIMEOUT).

-spec( setaccount(pid(), binary(), binary()) -> {ok, term()} | {error, term()}).
setaccount(Pid, WalletAddress, Account)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_binary(Account) ->
    gen_server:call(Pid, {setaccount, [WalletAddress, Account]}, ?TIMEOUT).

-spec( setgenerate(pid(), boolean()) -> {ok, null} | {error, term()}).
setgenerate(Pid, Generate)
    when is_pid(Pid),
         is_boolean(Generate) ->
    gen_server:call(Pid, {setgenerate, [Generate]}, ?TIMEOUT).
-spec( setgenerate(pid(), boolean(), pos_integer() | -1) -> {ok, null} | {error, term()}).
setgenerate(Pid, Generate, GenProcLimit)
    when is_pid(Pid),
         is_boolean(Generate),
         is_integer(GenProcLimit),
         (GenProcLimit > 0) or (GenProcLimit =:= -1) ->
    gen_server:call(Pid, {setgenerate, [Generate, GenProcLimit]}, ?TIMEOUT).

-spec( settxfee(pid(), float()) -> {ok, term()} | {error, term()}).
settxfee(Pid, Amount)
    when is_pid(Pid),
         is_float(Amount),
         Amount >= 0.00000000 ->
    gen_server:call(Pid, {settxfee, [Amount]}, ?TIMEOUT).

-spec( signmessage(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}).
signmessage(Pid, WalletAddress, Message)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_binary(Message) ->
    gen_server:call(Pid, {signmessage, [WalletAddress, Message]}, ?TIMEOUT).

-spec( signrawtransaction(pid(), binary()) ->
    {ok, binary()} | {error, term()}).
signrawtransaction(Pid, HexString)
    when is_pid(Pid),
         is_binary(HexString) ->
    gen_server:call(Pid, {signrawtransaction, [HexString]}, ?TIMEOUT).

-spec( signrawtransaction(pid(), binary(), [map()]) ->
    {ok, binary()} | {error, term()}).
signrawtransaction(Pid, HexString, Txns)
    when is_pid(Pid),
         is_binary(HexString),
         is_list(Txns) ->
    gen_server:call(Pid, {signrawtransaction, [HexString, Txns]}, ?TIMEOUT).

-spec( signrawtransaction(pid(), binary(), [map()], [binary()]) ->
    {ok, binary()} | {error, term()}).
signrawtransaction(Pid, HexString, Txns, Keys)
    when is_pid(Pid),
         is_binary(HexString),
         is_list(Txns),
         is_list(Keys) ->
    gen_server:call(Pid, {signrawtransaction, [HexString, Txns, Keys]}, ?TIMEOUT).

-spec( stop(pid()) -> {ok, binary()} | {error, term()} ).
stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop, ?TIMEOUT).

-spec( submitblock(pid(), binary()) -> {ok, term()} | {error, term()}).
submitblock(Pid, HexData)
    when is_pid(Pid),
         is_binary(HexData) ->
    gen_server:call(Pid, {submitblock, [HexData]}, ?TIMEOUT).
-spec( submitblock(pid(), binary(), map()) -> {ok, term()} | {error, term()}).
submitblock(Pid, HexData, Params)
    when is_pid(Pid),
         is_binary(HexData),
         is_map(Params) ->
    gen_server:call(Pid, {submitblock, [HexData, Params]}, ?TIMEOUT).

-spec( validateaddress(pid(), binary()) -> {ok, term()} | {error, term()}).
validateaddress(Pid, WalletAddress)
    when is_pid(Pid),
         is_binary(WalletAddress) ->
    gen_server:call(Pid, {validateaddress, [WalletAddress]}, ?TIMEOUT).

-spec( verifymessage(pid(), binary(), binary(), binary()) -> {ok, boolean()} | {error, term()}).
verifymessage(Pid, WalletAddress, Signature, Message)
    when is_pid(Pid),
         is_binary(WalletAddress),
         is_binary(Signature),
         is_binary(Message) ->
    gen_server:call(Pid, {verifymessage, [WalletAddress, Signature, Message]}, ?TIMEOUT).

-spec( walletlock(pid()) -> {ok, term()} | {error, term()} ).
walletlock(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, walletlock, ?TIMEOUT).

-spec( walletpassphrase(pid(), binary(), non_neg_integer()) -> {ok, term()} | {error, term()}).
walletpassphrase(Pid, PassPhrase, Timeout)
    when is_pid(Pid),
         is_binary(PassPhrase),
         is_integer(Timeout),
         Timeout >= 0 ->
    gen_server:call(Pid, {walletpassphrase, [PassPhrase, Timeout]}, ?TIMEOUT).

-spec( walletpassphrasechange(pid(), binary(), binary()) -> {ok, term()} | {error, term()}).
walletpassphrasechange(Pid, OldPassPhrase, NewPassPhrase)
    when is_pid(Pid),
         is_binary(OldPassPhrase),
         is_binary(NewPassPhrase) ->
    gen_server:call(Pid, {walletpassphrasechange, [OldPassPhrase, NewPassPhrase]}, ?TIMEOUT).





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
    C  = fun unicode:characters_to_binary/1,
    #bitcoin_jsonrpc_config{
        user     = C(Username),
        password = C(Password),
        host     = C(Host),
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
    Pass = string:substr(UserPass, Pos + 1),
    User = string:substr(UserPass, 1, Pos - 1),
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
