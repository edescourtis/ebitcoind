%%%-------------------------------------------------------------------
%%% @author forge33
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2015 2:37 PM
%%%-------------------------------------------------------------------
-module(ebitcoind_multisigutils).
-author("forge33").

%% API
-export([
%    generatemultisig/3,
]).


%-spec( generatemultisig(pid(), non_neg_integer(), non_neg_integer()) -> map()).
%createmultisig( Pid, RequiredNoOfSigs, NumberOfSigs )
%    when is_pid(Pid),
%         is_integer(RequiredNoOfSigs),
%         RequiredNoOfSigs > 1,
%         is_integer(NumberOfSigs),
%         NumberOfSigs >= RequiredNoOfSigs ->
    %first generate NumberOfSigs private keys
    %then createmultisig for x keys
