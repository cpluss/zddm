#pragma once
#define ZDDM_PROXY_H_

#include <optional>

#include "zddm/StorageAdapter.h"
#include "zddm/TrafficGate.h"

namespace zddm {

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
template <typename K, typename T>
class Proxy {
 public:
  Proxy(std::unique_ptr<TrafficGate<K>> &&gate,
        std::unique_ptr<StorageAdapter<K, T>> &&old_storage,
        std::unique_ptr<StorageAdapter<K, T>> &&new_storage)
      : gate_(std::move(gate)),
        old_(std::move(old_storage)),
        new_(std::move(new_storage)),
        enabled_(true) {}

  /**
   * @brief Perform a proxied read from either of the storage
   * adapters, depending on where the data can be found (
   * if at all).
   *
   * @param key The key over which to read the data, this
   * would often map to your primary key, or some index.
   */
  T read(K const &key) const;

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
  void write(K const &key, T &&data);

  /**
   * @brief Enable the proxy functionality, which will
   * enable traffic being sent to the new storage backend.
   */
  Proxy &enable() {
    enabled_ = true;
    return *this;
  }

  /**
   * @brief Disable the proxy functionality, which will
   * disable traffic being sent to the new storage backend.
   */
  Proxy &disable() {
    enabled_ = false;
    return *this;
  }

  /**
   * @brief Indicator whether or not the proxy is enabled
   */
  bool isEnabled() const { return enabled_; }

 private:
  std::unique_ptr<TrafficGate<K>> gate_;
  std::unique_ptr<StorageAdapter<K, T>> old_;
  std::unique_ptr<StorageAdapter<K, T>> new_;
  bool enabled_;
};

}  // namespace zddm

#include "Proxy-Inl.h"