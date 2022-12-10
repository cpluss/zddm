use super::traffic_gate::TrafficGate;

/**
 * @brief StorageAdapters are objects which we configure around
 * each storage backend to manage reads & writes. They act as generic
 * and thin wrappers which allows us to abstract the interaction away.
 *
 * The primary purpose is for you to wrap reads & writes to specific
 * entities in either storage backend with storage adapters.
 *
 * In order to avoid this getting out of hand, and ending up with
 * a huge number of storage adapters please aim to implement these
 * at the lowest common denominator (i.e. as close to the storage
 * interaction as possible) in order to minimise the surface area
 * for complexity.
 *
 * Note that this could be a call to virtually anything, even a local
 * API.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 * @tparam T The data-type which we aim to read, this could map
 * to a primitive or something more complex.
 */
pub trait StorageAdapter<K, V> {
    /**
     * @brief Perform a read towards the storage.
     *
     * @param key The key over which to read the data, this
     * would often map to your primary key, or some index.
     */
    fn read(&self, key: &K) -> Option<&V>;

    /**
     * @brief Perform a write towards the storage.
     *
     * @param key The key over which to read the data, this
     * would often map to your primary key, or some index.
     * @param data The data which to write.
     */
    fn write(&self, key: &K, data: V);
}

/**
 * @brief Proxy provides the main interface for ZDDM. It acts
 * as a wrapper to route traffic between two storage adapters, based
 * on a traffic-gate to manage how much of traffic will be diverted
 * to the new storage adapter from the old.
 *
 * NOTE: This is only thread-safe if the underlying
 * storage adapters are thread-safe.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 * @tparam T The data-type which we aim to read, this could map
 * to a primitive or something more complex.
 */
pub struct Proxy<K, V: Clone> {
    pub old_storage: Box<dyn StorageAdapter<K, V>>,
    pub new_storage: Box<dyn StorageAdapter<K, V>>,
    pub gate: TrafficGate<K>,
}

impl<K, V: Clone> Proxy<K, V> {
    /**
     * @brief Perform a proxied read from either of the storage
     * adapters, depending on where the data can be found (
     * if at all).
     *
     * @param key The key over which to read the data, this
     * would often map to your primary key, or some index.
     */
    pub fn read(&self, key: &K) -> Option<&V> {
        if !self.gate.should_pass(key) {
            return self.old_storage.read(key);
        }

        let maybe_new_data = self.new_storage.read(key);
        // On a hit return the value, as `new_` should be the
        // source of truth for anything we find therein.
        if maybe_new_data.is_some() {
            return maybe_new_data;
        }

        // On a miss we return the value from the old storage
        let maybe_old_data = self.old_storage.read(key);
        if maybe_old_data.is_none() {
            // Makes no sense to write an empty
            // value if we have two misses in a row.
            return maybe_old_data;
        }

        // Note that this is currently blocking, which we may want
        // to change in the future as it can have quite a drastic
        // latency impact on large workloads. Both the clone &
        // the write increase latency by quite a large amount
        // if we deal with large enough objects.
        self.new_storage.write(key, maybe_old_data.unwrap().clone());

        return maybe_old_data;
    }

    /**
     * @brief Perform a proxied write to both of the storage
     * adapters, depending on where the data can be found.
     *
     * Note that this only perform writes to the new database if
     * and only if the key is caught by the traffic-gate as configured.
     *
     * @param key The key over which to read the data, this
     * would often map to your primary key, or some index.
     * @param data The actual data to write into each storage
     * adapter.
     */
    pub fn write(&self, key: &K, data: V) {
        // We assume that we would always, even if disabled, want to
        // write to the old storage. It's the de-facto source
        // of truth.
        if !self.gate.should_pass(key) {
            // Note how this is distinctly different from below, as we are
            // passing ownership directly to avoid copying if we do not
            // have to.
            self.old_storage.write(key, data);
        } else {
            // Perform a double-write. If either write fail we'd rather have
            // the old storage failing first and avoid writing at all
            // to the new storage.
            self.old_storage.write(key, data.clone());
            self.new_storage.write(key, data);
        }
    }
}
