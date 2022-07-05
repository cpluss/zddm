# [RFC] Design

This is a work in progress, and we probably want to iterate on this over time :warning:. Please make sure to read the [main description of how this works](https://github.com/cpluss/zddm#how-does-it-work) before digging into this. This is primarily implementation focused on how to solve the problem at hand.

**NOTE**: this has not been reviewed / peer reviewed. Feedback appreciated.

![](https://lucid.app/publicSegments/view/086079d1-77b0-4c84-a230-f9c8e1b70283/image.png)

### Goals

1. Enable seamless data migration of most frequently accessed data (by key) without unnecessary complexity.
2. Provide a seamless drop-in replacement in core languages.
3. Provide an easy to follow guide on how to add support with minimal effort (self service) where necessary.

#### Assumptions

* We assume **key presence**, meaning that every backend storage has a key that we can produce a deterministic hash out of. This may or may not hold as some use-cases may not have this readily available, and if so consider adding an artificial / derived key that is not unique.
* We assume that we can get a consistent hashing methodology supported across multiple languages where necessary, as otherwise the rollout will be inconsistent in multi-platform environments.

### Abstractions

Should not be too complicated, and fairly straightforward. If you want to add any missing implementation please follow and mirror an existing version.

![](https://lucid.app/publicSegments/view/ebd5cda8-882e-4b3c-ad09-306b19e1947d/image.png)

* **Proxy** provides the main interface for users. 
  - Interface
    - `Read` - read data.
    - `Write` - write data.
  - Implements double writes by default, and interacts with both or multiple storage backends.
  - Interfaces with `StorageAdapters` for new storage & old storage.
  - Interfaces with `TrafficGate` in order to gate / rollout gradually.
* **StorageAdapter** is an interface you implement as you use the API in order to interact with your storage backend.
  - Interface
    - `Read` - read data.
    - `Write` - write data.
  - Type / storage agnostic interaction wrapper.
* **TrafficGate** is an interface with a default implementation to allow gradual rollout of copy & write logic. Primarily exists in order to avoid overloading any new backend or existing backend with traffic.
  - Interface
    - `shouldPass` - determines whether or not a specific key should pass the gate or not, where pass = go ahead and migrate.
* **DeterministicHasher** is an interface we provide to the `TrafficGate` in order to deterministically hash the storage keys for the TrafficGate itself.
  - Interface
    - `maxSize` - the maximum numerical value of the hash (breadth) space.
    - `hash` - compute the hash (numerical) from a key value.
  - Exposed API
    - `hashRate` - floating point value from `0 - 1`representing the hash value inside the entire hash-space.

### Behaviour

#### Proxy

Currently supported cache strategy in use by the proxy is **read-through** / **write-through**.

**Read-through** always hit the new storage first if the traffic-gate is open, if value found return. If value not found then read from old storage and save into new, then return [always, even on failure].

![](https://lucid.app/publicSegments/view/909176eb-0c0c-452c-9972-f6f2191ad2c1/image.png)

**Write-through** always hits both storages if the traffic-gate is open.

![](https://lucid.app/publicSegments/view/3d6162e4-2a10-4d51-a53c-c4c48b1d5c4b/image.png)

#### Traffic Gate

**TBD**: figure out a way to do consistent hashing across multiple platforms, in a OKish performant way.