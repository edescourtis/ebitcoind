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
1&gt; {ok,Pid} = ebitcoind:start_link().
{ok,&lt;0.177.0&gt;}
2&gt; ebitcoind:getbalance(Pid).
8437.02478294
3&gt; ebitcoind:getinfo(Pid).
{ok, #{&lt;&lt;&quot;balance&quot;&gt;&gt; =&gt; 8437.02478294,
  &lt;&lt;&quot;blocks&quot;&gt;&gt; =&gt; 260404,
  &lt;&lt;&quot;connections&quot;&gt;&gt; =&gt; 8,
  &lt;&lt;&quot;difficulty&quot;&gt;&gt; =&gt; 148819199.80509263,
  &lt;&lt;&quot;errors&quot;&gt;&gt; =&gt; &lt;&lt;&gt;&gt;,
  &lt;&lt;&quot;keypoololdest&quot;&gt;&gt; =&gt; 1420307921,
  &lt;&lt;&quot;keypoolsize&quot;&gt;&gt; =&gt; 102,
  &lt;&lt;&quot;paytxfee&quot;&gt;&gt; =&gt; 0.0,
  &lt;&lt;&quot;protocolversion&quot;&gt;&gt; =&gt; 70002,
  &lt;&lt;&quot;proxy&quot;&gt;&gt; =&gt; &lt;&lt;&gt;&gt;,
  &lt;&lt;&quot;relayfee&quot;&gt;&gt; =&gt; 1.0e-5,
  &lt;&lt;&quot;testnet&quot;&gt;&gt; =&gt; false,
  &lt;&lt;&quot;timeoffset&quot;&gt;&gt; =&gt; -3,
  &lt;&lt;&quot;version&quot;&gt;&gt; =&gt; 90300,
  &lt;&lt;&quot;walletversion&quot;&gt;&gt; =&gt; 60000}}
4&gt; ebitcoind:setgenerate(Pid,true).
{ok, null}
</pre>
OR you can specify connection information as start_link argument
<pre>
1> {ok,Pid} = ebitcoind:start_link(<<"http://rpcuser:rpcpassword@host:port">>).
{ok,<0.177.0>}
</pre>
