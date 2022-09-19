-module(zddm_memory_migration_test).

-include_lib("eunit/include/eunit.hrl").

in_memory_read_migration_test() ->
    % Create two storage adapters wrapped
    % around ets.
    OldTableRef = ets:new(read_migration_test_old, [set]),
    NewTableRef = ets:new(read_migration_test_new, [set]),

    OldStorageAdapter = zddm_adapters:ets(OldTableRef),
    NewStorageAdapter = zddm_adapters:ets(NewTableRef),

    % Generate a bunch of data into the old storage that we want
    % to eventually migrate over
    NumSamples = 100000,
    [
        zddm_storage_adapter:write(
            OldStorageAdapter,
            N,
            rand:uniform(10000)
        )
     || N <- lists:seq(0, NumSamples)
    ],

    % Use a simple traffic gate which lets everything
    % through
    Gate = zddm_traffic_gate:create(1.0, fun zddm_hashers:sha_hash_rate/1),
    Proxy = zddm_proxy:create(Gate, OldStorageAdapter, NewStorageAdapter),

    % 1. Read every key and make sure it matches what we have in the
    % old storage.
    [
        ?assertEqual(
            zddm_proxy:read(Proxy, N),
            zddm_storage_adapter:read(OldStorageAdapter, N)
        )
     || N <- lists:seq(0, NumSamples)
    ],

    % 2. Once the entire migration has been performed we should have
    % the same data in the new storage
    [
        ?assertEqual(
            zddm_storage_adapter:read(OldStorageAdapter, N),
            zddm_storage_adapter:read(NewStorageAdapter, N)
        )
     || N <- lists:seq(0, NumSamples)
    ].
