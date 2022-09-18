# ZDDM - Go User Guide

Zero Downtime Data Migration [ZDDM] user guide for Go.

![](https://lucid.app/publicSegments/view/ebd5cda8-882e-4b3c-ad09-306b19e1947d/image.png)

## Before we start

We assume the existence of a key to each of your storage backends. This could be a compounded key that you can collapse to a single type / struct, or a primitive.

## How-To

In order to use ZDDM you need to do the following

1. Create `StorageAdapters` for each of your storage backends. These acts as simple wrappers so that we can communicate with any storage backend.
   - Note that if you plan on using this in a multi-thread environment these need to be thread-safe.
2. Set-up a traffic-gate based on your input key to the storage backends.
   - Note that if you plan on accessing the storage across platforms this would need to be deterministic across platforms.
   - Note that this does not need to be cryptographically safe, so you can use something like `Murmur`, `XXHash`, or `MD5`.
   - Note that it's highly beneficial if you can configure this outside of the platform elsewhere, e.g. by using [launchdarkly](https://launchdarkly.com/) or similar tooling, as it acts as a killswitch should things go awry.
3. Leverage the `Proxy` interface everywhere you read & write data, in order to migrate as we interact with the backend storages respectively.
   - Note that it is up to you to make sure that every callsite is migrated. If you write data outside of using `Proxy` it means that the newer storage will go out of consistency fairly quickly - reading however should be fine.
4. Continue to use and run your application as usual, until you notice that almost no reads occur whatsoever to the old storage (use an external counter).

## Examples

Nothing tells a story better than examples.

#### Creating a proxy

Ideally initialise centrally if thread-safe to do so

```go
// Create our storage adapters to interact with both the old
// and new storage backends.
//
// Note how we are using an `InMemoryAdapter` which just 
// wraps a map, please use your own defined adapters.
old_adapter := zddm.NewInMemoryStorageAdapter[string, V]()
new_adapter := zddm.NewInMemoryStorageAdapter[string, V]()

// The traffic gate will control how many requests will
// leverage the new storage as a read-through cache.
gate := zddm.NewTrafficGate[string](
    // rollout rate, 1.0 = 100%, fully open
    1.0,

    // Create a simple hasher, see below for implementation
    &simpleHasher{},
)

// Create our proxy, which are to be used to read & write 
// data from now on in our application.
proxy := zddm.NewProxy[string, V](
    gate,
    old_adapter,
    new_adaper
)
```

#### Using the proxy

Reading data is quite straightforward

```go
data := proxy.Read(key)
if data == nil {
    // Not found in either of the storage
    // backends
}
```

Writing data is quite straightforward

```go
data := ...
proxy.Write(key, data)
```

## Reading

* [Main Introduction & Idea](https://github.com/cpluss/zddm#zero-downtime-data-migration-zddm)
* [Design doc / RFC](https://github.com/cpluss/zddm/tree/main/design#rfc-design)