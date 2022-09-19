-module(zddm_adapters).

-export([
    ets/1
]).

-spec ets(ets:tid()) -> zddm_storage_adapter:storage_adapter(term(), term()).
ets(TableRef) ->
    ReadFun = fun(Key) ->
        case ets:lookup(TableRef, Key) of
            [{_, Value}] -> {ok, Value};
            Error -> {error, Error}
        end
    end,
    WriteFun = fun(Key, Value) ->
        ets:insert(TableRef, {Key, Value})
    end,
    zddm_storage_adapter:create(ReadFun, WriteFun).
