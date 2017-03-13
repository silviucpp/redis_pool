-author("silviu.caragea").

-type redis_pool_option() :: erlpool:pool_option() | eredis:option().
-type reason() :: term().
-type return_value() :: eredis:return_value().
-type redis_error() :: binary() | no_connection | tuple().
-type pipeline() :: eredis:pipeline().