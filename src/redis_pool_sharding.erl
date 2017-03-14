-module(redis_pool_sharding).

-export([
    get_command_key/1
]).

%% pipeline query
%%
%% if first command is multi we will get the key from to the next command
%% otherwise we will analyze first command in the pipeline
%%
%% simple query
%%
%% second term will be returned with the following exceptions:
%% - eval and evalsha command we will look at the fourth term.
%% - all commands that are not supported returns undefined
%%

get_command_key([[X|_] = C | Z]) when is_binary(X) orelse is_list(X) ->
    case get_command(X) of
        "multi" ->
            get_command_key(Z);
        _ ->
            get_command_key(C)
    end;
get_command_key([Command, Key | Other]) ->
    case get_command(Command) of
        "eval" ->
            get_eval_key(Other);
        "evalsha" ->
            get_eval_key(Other);
        "multi" ->
            undefined;
        "exec" ->
            undefined;
        "info" ->
            undefined;
        "config" ->
            undefined;
        "shutdown" ->
            undefined;
        "slaveof" ->
            undefined;
        _ ->
            to_binary(Key)
    end;
get_command_key(_) ->
    undefined.

get_eval_key([_, Key|_]) ->
    to_binary(Key);
get_eval_key(_) ->
    undefined.

get_command(X) when is_binary(X) ->
    string:to_lower(binary_to_list(X));
get_command(X) ->
    string:to_lower(X).

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, latin1);
to_binary(_X) ->
    undefined.
