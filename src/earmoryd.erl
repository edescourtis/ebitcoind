%%%-------------------------------------------------------------------
%%% @author forge33
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2015 9:58 PM
%%%-------------------------------------------------------------------
-module(earmoryd).
-author("forge33").


-include("ewalletrpc.hrl").

%% API
-export([start_link/0,
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
