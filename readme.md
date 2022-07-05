# Zero Downtime Data Migration [ZDDM]

[![cpluss](https://circleci.com/gh/cpluss/zddm.svg?style=shield&circle-token=ec07e2973d4d542b1d128849483915046b32c6e3)](https://app.circleci.com/pipelines/github/cpluss/zddm?branch=main&filter=all)

Migrating data between two databases, storage solutions, or backends, is hard. It often involves downtime which disrupts your revenue stream, and often end up with data contention and staleness in the end.

ZDDM aims to provide a technique which facilitates data migration without the need for any database magic. Running migrations can take hours, and taking down your production database during that time is simply not feasible for some people & companies.

**ZDDM won't magically solve data migrations, but it will make your life a lot easier.**

**NOTE**: this is storage agnostic by design. There is no magic involved in how we interact with storages / databases, and is therefore unoptimised towards any specific storage solution.

## :warning: WORK IN PROGRESS :warning:

This is a work-in-progress, and is not finished yet. See the "Roadmap" section below for more details on where we are and where we are going.


## How does it work?

ZDDM is a technique which you can leverage in order to reduce the downtime introduced by your data migrations in production. 

You proxy each read, and write, through the ZDDM API for your chosen language [*if supported*] and it does in turn manage the migration for you in real-time. Once the migration is done you remove ZDDM as a middleware and call it a day.

![](https://lucid.app/publicSegments/view/086079d1-77b0-4c84-a230-f9c8e1b70283/image.png)

## Migration strategy

The basic idea is that we proxy every storage operation through ZDDM, and ZDDM turns your new storage solution into a read-through cache.

* **On reads** you would try to read from `New`, and if the data is not found you read it from `Old` and return it. You would also write said data into `New` so that it can be read the next time.

* **On writes** you would write the data to `Old`, and if it exists in `New` you would also write it into `New`.

* **If anything goes wrong** you revert to only interacting with `Old` in order to avoid production downtime or disruptions.

## :wrench: Roadmap for v1 

Aim is to build a storage agnostic core which provides the plumbing necessary to pull this off using a plugin-architecture. We should work on adding ready-to-use functionality to interface with standard storage solutions in future iterations (if we get there).

- [ ] Design
  - [x] Main API & Abstractions
  - [x] Cache Strategies
    - [x] Write-through / Read-through
    - [ ] [STRETCH] Write-back to reduce latency impact
  - [x] Storage adapters
  - [ ] Deterministic Rollout/Gating Strategy
  - [ ] Data consistency checks
- [ ] Language support
    - [ ] Core Language support
       - [x] C++
       - [ ] Go
       - [ ] JavaScript / Node
    - [ ] Self-Service guide to add Language Support
- [ ] Tests & Verification
  - [ ] Integration with in-memory unit-test
    - [x] C++
    - [ ] Go
    - [ ] JavaScript / Node
  - [ ] [STRETCH] Test-suite
- [ ] Documentation
  - [ ] Language Docs (how to use)
    - [ ] C++
    - [ ] Go
    - [ ] Javascript / Node
  - [ ] Rollout guide (how to use the tech)

## FAQ

### Won't this reduce performance?

* **Double the amount of operations** will always incurr a cost in that we can not get away from. This would simply be the cost of doing a live migration, and if it is too expensive it's not worth it.
* **Unfortunately there will always be a latency cost** of doing this, as we would incurr the cost of querying the first storage and later on the second until the migration is fully complete.
  * The latency cost will always be equal to the latency it would take to read from `New`.
  * This could be reduced by changing the strategy to a read-back or a write-back (rather than through) cache, in where each operation with `New` would happen asynchonously and not in a blocking way. *This will be supported long-term*, but not out of the gate.
* **Read-heavy or write-heavy loads can be mitigated** by slowly gating the interaction with `New` with a configurable rate.
  * Assuming that your production solution (`Old`) can handle the load of reads & writes we slowly gate each write & read to the new storage solution (`New`) with a configurable rate.
  * This means that we would only perform the migration for a fraction of the traffic at a time, which you can slowly control and ramp up as we improve the hit-rate & coverage of the new storage solution.

### Why is this not a proxy or a service?

Services introduce unnecessary complexity to the problem.

* It would introduce a scaling bottleneck to all storage operations that we probably can not afford on larger amount of traffic.
* It would mean that we have to support any storage operation out of the box on a single surface area, which will incurr scope-creep over time as we race to keep up with every feature from the storage solution we would need.

Dealing with both of these issues will become way more complex than dealing with multiple implementations in different languages.

### Won't multi-language support cause divergence long-term?

This is definitely a risk, and something to keep an eye out for in the long-run :)

* We can mitigate this by leveraging interopability where we can between languages to minimise the surface area of each implementation. 
* We can mitigate this by providing strict design guidelines & principles that every implementation should adhere to, as well as designating one version as the source-of-truth others should follow.

### What about data not frequently accessed / written?

You'll most likely notice that there are some data that never will be migrated due to the fact thet it's never read nor written. 

**You can run a background migration for data that aren't accessed by your applications (i.e. stale data).** The risk is relatively low as it's a low likelyhood that it'll get accessed, and if it is then the ZDDM proxy will take care of making sure it's copied (and double written) correctly.