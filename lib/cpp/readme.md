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
   - Note that this does not need to be cryptographically safe, so you can use something like `Murmur`, `XXHash`, or `MD5`.
   - Note that it's highly beneficial if you can configure this outside of the platform elsewhere, e.g. by using [launchdarkly](https://launchdarkly.com/) or similar tooling, as it acts as a killswitch should things go awry.
3. Leverage the `Proxy` interface everywhere you read & write data, in order to migrate as we interact with the backend storages respectively.
   - Note that it is up to you to make sure that every callsite is migrated. If you write data outside of using `Proxy` it means that the newer storage will go out of consistency fairly quickly - reading however should be fine.
4. Continue to use and run your application as usual, until you notice that almost no reads occur whatsoever to the old storage (use an external counter).

## Examples

Nothing tells a story better than examples.

#### Creating a proxy

Ideally initialise centrally if thread-safe to do so

```C++
// Create our storage adapters to interact with both the
// old and the new storage backends. 
//
// Note how we are using an `InMemoryAdapter` which just wraps
// an unordered_map, please use your own defined adapters.
auto old_adapter = std::make_unique<
    adapters::InMemoryStorageAdapter<std::string, V>>();
auto new_adapter = std::make_unique<
    adapters::InMemoryStorageAdapter<std::string, V>>();

// The traffic gate will control how many requests will
// leverage the new storage as a read-through cache.
auto gate = std::make_unique<zddm::TrafficGate<std::string>>(
  1.0 /* rollout rate 1.0 = 100%, fully open */, 

  // Create a simple hasher (wrapper around std::hash) as
  // the key is a primitive (std::string)
  std::make_unique<zddm::hashers::SimpleHasher<std::string>>()
);

// Create our proxy, which are to be used in order to read &
// write data from now on in our application.
zddm::Proxy<std::string, V> proxy(
  std::move(gate),
  std::move(old_adapter),
  std::move(new_adapter)
);
```

#### Using the proxy

Reading data is quite straightforward

```C++
try {
  auto data = proxy.read(key);
} catch (std::out_of_range const& _) {
  // Not found in either of the storage
  // backends
}
```

Writing data is quite straightforward

```C++
auto data = ...;

// Note how we're passing ownership of data to minimise
// redundant copies. 
proxy.write(key, std::move(data));
```

## Reading

* [Main Introduction & Idea](https://github.com/cpluss/zddm#zero-downtime-data-migration-zddm)
* [Design doc / RFC](https://github.com/cpluss/zddm/tree/main/design#rfc-design)