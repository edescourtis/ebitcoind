%%%-------------------------------------------------------------------
%%% @author Patrick Begley
%%% @author Eric des Courtis
%%% @copyright (C) 2015, Patrick Begley
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2015 9:58 PM
%%%-------------------------------------------------------------------
-module(earmoryd).
-author("Patrick Begley").
-author("Eric des Courtis").


-include("ewalletrpc.hrl").

%% API
-export([
    start_link/0,
    backupwallet/2,
    clearaddressmetadata/1,
    createlockbox/3,
    createustxformany/2,
    createustxtoaddress/3,
    decoderawtransaction/2,
    dumpprivkey/2,
    encryptwallet/2,
    getactivelockbox/1,
    getactivewallet/1,
    getaddrbalance/2,
    getaddrbalance/3,
    getaddressmetadata/1,
    getarmorydinfo/1,
    getbalance/1,
    getbalance/2,
    getblock/2,
    gethextxtobroadcast/2,
    getledger/1,
    getledgersimple/2,
    getledgersimple/3,
    getledgersimple/4,
    getlockboxinfo/1,
    getlockboxinfo/2,
    getlockboxinfo/3,
    getnewaddress/1,
    getrawtransaction/2,
    getrawtransaction/3,
    getrawtransaction/4,
    getreceivedbyaddress/2,
    gettransaction/2,
    gettxout/3,
    gettxout/4,
    getwalletinfo/1,
    getwalletinfo/2,
    help/1
]).


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
    ArmoryConfPath = filename:join([HomePath, ".armory", "armoryd.conf"]),
    true = filelib:is_regular(ArmoryConfPath),
    Config = get_settings_from_armory_conf(ArmoryConfPath),
    ewalletrpc:start_link(Config).

-spec(backupwallet(pid(), binary()) -> {ok, map()} | {error, term()}).
backupwallet(Pid, BackupFilePath) 
    when is_pid(Pid), is_binary(BackupFilePath) ->
    gen_server:call(Pid, {backupwallet, [BackupFilePath]}, ?TIMEOUT).

-spec(clearaddressmetadata(pid()) -> {ok, term()} | {error, term()}).
clearaddressmetadata(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, clearaddressmetadata, ?TIMEOUT).

-spec(createlockbox(pid(), pos_integer(), list()) -> {ok, map()} | {error, term()}).
createlockbox(Pid, WalletsOrPublicKeysRequired, WalletsOrPublicKeys) 
    when is_pid(Pid), 
    is_integer(WalletsOrPublicKeysRequired), WalletsOrPublicKeysRequired > 0,
    is_list(WalletsOrPublicKeys), length(WalletsOrPublicKeys) > 0,
    length(WalletsOrPublicKeys) >= WalletsOrPublicKeysRequired ->
    gen_server:call(
        Pid,
        {createlockbox, [
            WalletsOrPublicKeysRequired,
            length(WalletsOrPublicKeys),
            WalletsOrPublicKeys]
        },
        ?TIMEOUT
    ).

-spec(createustxformany(pid(), term()) -> {ok, binary()} | {error, term()}).
createustxformany(Pid, Args) when is_pid(Pid) ->
    gen_server:call(Pid, {createustxformany, [Args]}, ?TIMEOUT).

-spec(createustxtoaddress(pid(), binary(), float()) -> {ok, binary()} | {error, term()}).
createustxtoaddress(Pid, RecipientAddress, Amount) 
    when is_pid(Pid), is_binary(RecipientAddress), is_float(Amount) ->
    gen_server:call(Pid, {createustxtoaddress, [RecipientAddress, Amount]}, ?TIMEOUT).

-spec(decoderawtransaction(pid(), binary()) -> {ok, binary()} | {error, term()}).
decoderawtransaction(Pid, TransactionHexString)
    when is_pid(Pid), is_binary(TransactionHexString) ->
    gen_server:call(Pid, {decoderawtransaction, [TransactionHexString]}, ?TIMEOUT).

-spec(dumpprivkey(pid(), binary()) -> {ok, binary()} | {error, term()}).
dumpprivkey(Pid, Base58Address) when is_pid(Pid), is_binary(Base58Address) ->
    gen_server:call(Pid, {dumpprivkey, [Base58Address]}, ?TIMEOUT).

-spec(encryptwallet(pid(), binary()) -> {ok, binary()} | {error, term()}).
encryptwallet(Pid, PassPhrase) when is_pid(Pid), is_binary(PassPhrase) ->
    gen_server:call(Pid, {encryptwallet, [PassPhrase]}, ?TIMEOUT).

-spec(getactivelockbox(pid()) -> {ok, binary()} | {error, term()}).
getactivelockbox(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getactivelockbox, ?TIMEOUT).

-spec(getactivewallet(pid()) -> {ok, binary()} | {error, term()}).
getactivewallet(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getactivewallet, ?TIMEOUT).

-spec(getaddrbalance(pid(), binary()) -> {ok, float()} | {error, term()}).
getaddrbalance(Pid, Base58Address) when is_pid(Pid), is_binary(Base58Address) ->
    getaddrbalance(Pid, Base58Address, <<"spendable">>).
    
-spec(getaddrbalance(pid(), binary(), binary()) -> {ok, float()} | {error, term()}).
getaddrbalance(Pid, Base58Address, BalanceType)
    when is_pid(Pid), is_binary(Base58Address), is_binary(BalanceType) ->
    case gen_server:call(Pid, {getaddrbalance, [Base58Address, BalanceType]}, ?TIMEOUT) of
        {ok, -1} -> {error, -1};
        {ok,  X} -> X;
        Other    -> Other
    end.

-spec(getaddressmetadata(pid()) -> {ok, map()} | {error, term()}).
getaddressmetadata(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getaddressmetadata, ?TIMEOUT).

-spec(getarmorydinfo(pid()) -> {ok, map()} | {error, term()}).
getarmorydinfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getarmorydinfo, ?TIMEOUT).

-spec(getbalance(pid()) -> {ok, float()} | {error, term()}).
getbalance(Pid) when is_pid(Pid) ->
    getbalance(Pid, <<"spendable">>).

-spec(getbalance(pid(), binary()) -> {ok, float()} | {error, term()}).
getbalance(Pid, BalanceType) when is_pid(Pid), is_binary(BalanceType) ->
    gen_server:call(Pid, {getbalance, [BalanceType]}, ?TIMEOUT).

-spec(getblock(pid(), binary()) -> {ok, map()} | {error, term()}).
getblock(Pid, BlockHash) when is_pid(Pid), is_binary(BlockHash) ->
    gen_server:call(Pid, {getblock, [BlockHash]}, ?TIMEOUT).

-spec(gethextxtobroadcast(pid(), binary()) -> {ok, binary()} | {error, term()}).
gethextxtobroadcast(Pid, TxASCIIFile) when is_pid(Pid), is_binary(TxASCIIFile) ->
    gen_server:call(Pid, {gethextxtobroadcast, [TxASCIIFile]}, ?TIMEOUT).

%% TODO: Check if this is correct from source code of armoryd
-spec(getledger(pid()) -> {ok, term()} | {error, term()}).
getledger(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getledger, ?TIMEOUT).

-spec(getledgersimple(pid(), binary()) -> {ok, map()} | {error, term()}).
getledgersimple(Pid, Base58WalletOrLockboxId)
    when is_pid(Pid), is_binary(Base58WalletOrLockboxId) ->
    gen_server:call(Pid, {getledgersimple, [Base58WalletOrLockboxId]}, ?TIMEOUT).

-spec(getledgersimple(pid(), binary(), pos_integer()) -> {ok, map()} | {error, term()}).
getledgersimple(Pid, Base58WalletOrLockboxId, TxCount)
    when is_pid(Pid), is_binary(Base58WalletOrLockboxId),
    is_integer(TxCount), TxCount >= 1 ->
    gen_server:call(Pid, {getledgersimple, [Base58WalletOrLockboxId, TxCount]}, ?TIMEOUT).


-spec(getledgersimple(pid(), binary(), pos_integer(), non_neg_integer()) -> {ok, map()} | {error, term()}).
getledgersimple(Pid, Base58WalletOrLockboxId, TxCount, FromTx)
    when is_pid(Pid), is_binary(Base58WalletOrLockboxId),
    is_integer(TxCount), is_integer(FromTx), TxCount >= 1, FromTx >= 0 ->
    gen_server:call(Pid, {getledgersimple, [Base58WalletOrLockboxId, TxCount, FromTx]}, ?TIMEOUT).

-spec(getlockboxinfo(pid()) -> {ok, map()} | {error, term()}).
getlockboxinfo(Pid)
    when is_pid(Pid)->
    gen_server:call(Pid, getlockboxinfo, ?TIMEOUT).

-spec(getlockboxinfo(pid(), binary()) -> {ok, map()} | {error, term()}).
getlockboxinfo(Pid, InLockboxBase58Id)
    when is_pid(Pid), is_binary(InLockboxBase58Id) ->
    gen_server:call(Pid, {getlockboxinfo, [InLockboxBase58Id]}, ?TIMEOUT).

-spec(getlockboxinfo(pid(), binary(), binary()) -> {ok, map()} | {error, term()}).
getlockboxinfo(Pid, InLockboxBase58Id, OutForm)
    when is_pid(Pid), is_binary(InLockboxBase58Id), is_binary(OutForm) ->
    gen_server:call(Pid, {getlockboxinfo, [InLockboxBase58Id, OutForm]}, ?TIMEOUT).

-spec(getnewaddress(pid()) -> {ok, binary()} | {error, term()}).
getnewaddress(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getnewaddress, ?TIMEOUT).

-spec(getrawtransaction(pid(), binary()) -> {ok, map()} | {error, term()}).
getrawtransaction(Pid, TxHash)
    when is_pid(Pid), is_binary(TxHash) ->
    gen_server:call(Pid, {getrawtransaction, [TxHash]}, ?TIMEOUT).

-spec(getrawtransaction(pid(), binary(), non_neg_integer()) -> {ok, map()} | {error, term()}).
getrawtransaction(Pid, TxHash, Verbose)
    when is_pid(Pid), is_binary(TxHash),
    is_integer(Verbose), Verbose >= 0 ->
    gen_server:call(Pid, {getrawtransaction, [TxHash, Verbose]}, ?TIMEOUT).


-spec(getrawtransaction(pid(), binary(), non_neg_integer(), binary()) -> {ok, map()} | {error, term()}).
getrawtransaction(Pid, TxHash, Verbose, Endianness)
    when is_pid(Pid), is_binary(TxHash),
    is_integer(Verbose), Verbose >= 0,
    is_binary(Endianness) ->
    gen_server:call(Pid, {getrawtransaction, [TxHash, Verbose, Endianness]}, ?TIMEOUT).

-spec(getreceivedbyaddress(pid(), binary()) -> {ok, float()} | {error, term()}).
getreceivedbyaddress(Pid, Base58Address) when is_pid(Pid), is_binary(Base58Address) ->
    gen_server:call(Pid, {getreceivedbyaddress, [Base58Address]}, ?TIMEOUT).

-spec(gettransaction(pid(), binary()) -> {ok, map()} | {error, term()}).
gettransaction(Pid, TxHash) when is_pid(Pid), is_binary(TxHash) ->
    gen_server:call(Pid, {gettransaction, [TxHash]}, ?TIMEOUT).

-spec(gettxout(pid(), binary(), non_neg_integer()) -> {ok, map()} | {error, term()}).
gettxout(Pid, TxHash, N)
    when is_pid(Pid), is_binary(TxHash), is_integer(N), N >= 0 ->
    gen_server:call(Pid, {gettxout, [TxHash, N]}, ?TIMEOUT).

-spec(gettxout(pid(), binary(), non_neg_integer(), non_neg_integer()) -> {ok, map()} | {error, term()}).
gettxout(Pid, TxHash, N, Binary)
    when is_pid(Pid), is_binary(TxHash), is_integer(N), is_integer(Binary),
    N >= 0, Binary >= 0 ->
    gen_server:call(Pid, {gettxout, [TxHash, N, Binary]}, ?TIMEOUT).

-spec(getwalletinfo(pid()) -> {ok, map()} | {error, term()}).
getwalletinfo(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getwalletinfo, ?TIMEOUT).

-spec(getwalletinfo(pid(), binary()) -> {ok, map()} | {error, term()}).
getwalletinfo(Pid, Base58WalletId) when is_pid(Pid), is_binary(Base58WalletId) ->
    gen_server:call(Pid, {getwalletinfo, [Base58WalletId]}, ?TIMEOUT).

-spec( help(pid()) -> {ok, binary()} | {error, term()}).
help(Pid)
    when is_pid(Pid) ->
    gen_server:call(Pid, help, ?TIMEOUT).






get_settings_from_armory_conf(ArmoryConfPath) ->
    {ok, Data} = file:read_file(ArmoryConfPath),
    FirstLine = hd(string:tokens(unicode:characters_to_list(Data), "\r\n")),
    [U|ListRest] = string:tokens(FirstLine, ":"),
    P = string:join(ListRest, ":"),
    #ewallet_jsonrpc_config{
        user     = unicode:characters_to_binary(U),
        password = unicode:characters_to_binary(P),
        host     = <<"localhost">>,
        port     = 8225,
        ssl      = false
    }.
