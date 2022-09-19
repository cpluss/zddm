%%%-----------------------------------
%%% @doc
%%% StorageAdapters are configured for each storage to manage reads
%%% and writes. They act as wrappers which allow us to abstract the
%%% storage interaction away.
%%%
%%% In order to avoid this getting out of hand, and ending up with
%%% a huge number of storage adapters please aim to implement these
%%% at the lowest common denominator (i.e. as close to the storage
%%% interaction as possible) in order to minimise the surface area
%%% for complexity.
%%% @end
%%%-----------------------------------
-module(zddm_storage_adapter).

%% @doc read_fun should perform a generic read towards
%% the storage backend. Return an OK with the result
%% or an error.
%% -type read_fun().
-type read_fun(K, V) :: fun((K) -> {ok, V} | {error, term()}).

%% @doc write_fun should perform a generic read towards
%% the storage backend. Return an OK with the result
%% or an error.
%% -type write_fun().
-type write_fun(K, V) :: fun((K, V) -> ok | {error, term()}).

%% @doc storage_adapter contains the appropriate read
%% and write methods.
%% -type storage_adapter().
-type storage_adapter(K, V) ::
    #{
        read_fun => read_fun(K, V),
        write_fun => write_fun(K, V)
    }.

-export_type([
    write_fun/2,
    read_fun/2,
    storage_adapter/2
]).

-export([
    read/2,
    write/3,
    create/2
]).

create(ReadFun, WriteFun) ->
    #{
        read_fun => ReadFun,
        write_fun => WriteFun
    }.

%% @doc read grabs the read function from the storage
%% adapter and runs it against K. This acts as a simple
%% wrapper.
read(#{read_fun := ReadFun}, K) ->
    ReadFun(K).

%% @doc write grabs the write function from the storage
%% adapter and runs it against K. This acts as a simple
%% wrapper.
write(#{write_fun := WriteFun}, K, V) ->
    WriteFun(K, V).
