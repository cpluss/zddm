# ZDDM - C++ User Guide

Zero Downtime Data Migration [ZDDM] user guide for C++. 

![](https://lucid.app/publicSegments/view/ebd5cda8-882e-4b3c-ad09-306b19e1947d/image.png)

## Before we start

We assume the existence of a key to each of your storage backends. This could be a compounded key that you can collapse to a single type / struct, or a primitive.

## How-To 

In order to use ZDDM you need to do the following

1. Create `StorageAdapters` for each of your storage backends. These acts as simple wrappers so that we can communicate with any storage backend.
   - Note that if you plan on using this in a multi-thread environment these need to be thread-safe.
2. Set-up a traffic-gate based on your input key to the storage backends.
   - Note that if you plan on accessing the storage across platforms this would need to be deterministic across platforms.
   - Note that it's highly beneficial if you can configure this outside of the platform elsewhere, e.g. by using [launchdarkly](https://launchdarkly.com/) or similar tooling, as it acts as a killswitch should things go awry.
   - You could use the supplied hasher in `zddm_hashers` in order to get started quickly locally.
3. Leverage the `Proxy` interface everywhere you read & write data, in order to migrate as we interact with the backend storages respectively.
   - Note that it is up to you to make sure that every callsite is migrated. If you write data outside of using `Proxy` it means that the newer storage will go out of consistency fairly quickly - reading however should be fine.
   - It would probabyl be beneficial to wrap the storage interaction into a module or a `gen_server` in order to minimise the surface area of a migration.
4. Continue to use and run your application as usual, until you notice that almost no reads occur whatsoever to the old storage (use an external counter).

## Examples

Nothing tells a story better than examples.

#### Creating a proxy

```erlang
% Create our storage adapters to interact with both the old
% and the new storage backends.
%
% Note how we're using an ETS adapter which essentially
% just wraps an existing ETS table.
OldAdapter = zddm_adapters:ets(ets:new(my_old_table_name, [set])),
NewAdapter = zddm_adapters:ets(ets:new(my_new_table_name, [set])),

% The traffic gate will control how many requests will
% leverage the new storage as a read-through cache.
Gate = zddm_traffic_gate:create(
    % this is the rate, 1.0 = 100%, fully open
    1.0, 
    % simple hasher which just wraps crypto:hash
    fun zddm_hashers:sha_hash_rate/1
),

% This is the proxy configuration which you'll have to pass around
% everywhere. Probably a good idea to wrap this into a `gen_server`
% where we can persist its state / configuration (or a persistent_term).
Proxy = zddm_proxy:create(Gate, OldAdapter, NewAdapter)
```

#### Using the proxy

Reading data

```erlang
case zddm_proxy:read(Proxy, Key) of
    {ok, Data} -> 
        % do something with my data
    {error, Error} ->
        % something went wrong, the key could not be found
        % or something else happened
end
```

Writing data

```erlang
case zddm_proxy:write(Proxy, Key, Data) of
    ok ->
        % everything went fine, act on it
    {error, Error} ->
        % something went wrong, the key could not be found
        % or something else happened
end
```

## Reading

* [Main Introduction & Idea](https://github.com/cpluss/zddm#zero-downtime-data-migration-zddm)
* [Design doc / RFC](https://github.com/cpluss/zddm/tree/main/design#rfc-design)