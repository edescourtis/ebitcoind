%%%-------------------------------------------------------------------
%%% @author forge33
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2015 10:06 PM
%%%-------------------------------------------------------------------
-author("forge33").

-define(TIMEOUT,                 30000  ).


-record(ewallet_jsonrpc_config, {
    user     = <<"">>          :: binary(),
    password = <<"">>          :: binary(),
    host     = <<"127.0.0.1">> :: binary(),
    port     = 8332            :: pos_integer(),
    ssl      = false           :: boolean()
}).

