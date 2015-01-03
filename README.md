#ebitcoind
=========
> Erlang to Bitcoind API based off API documentation @ https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_calls_list

Linux or Mac is required to run this, Windows is not supported as of yet.

Steps
=====
1. First create a bitcoin configuration in your home directory that contains the following example
<pre>
rpcuser=bitcoinrpc
rpcpassword=changeme
server=1
</pre>
2. Start the daemon by typing
<pre>bitcoind -daemon</pre>
3. Start erlang

Usage
=====
<pre>
1> {ok,Pid} = ebitcoind:start_link().
{ok,<0.177.0>}
2> ebitcoind:getbalance(Pid).
8437.02478294
3> ebitcoind:getinfo(Pid).
#{<<"balance">> => 8437.02478294,
  <<"blocks">> => 260404,
  <<"connections">> => 8,
  <<"difficulty">> => 148819199.80509263,
  <<"errors">> => <<>>,
  <<"keypoololdest">> => 1420307921,
  <<"keypoolsize">> => 102,
  <<"paytxfee">> => 0.0,
  <<"protocolversion">> => 70002,
  <<"proxy">> => <<>>,
  <<"relayfee">> => 1.0e-5,
  <<"testnet">> => false,
  <<"timeoffset">> => -3,
  <<"version">> => 90300,
  <<"walletversion">> => 60000}
4> ebitcoind:setgenerate(Pid,true).
null
</pre>
