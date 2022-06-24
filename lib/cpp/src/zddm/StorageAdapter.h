#pragma once

#include <optional>

namespace zddm {

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
template <typename K, typename T>
class StorageAdapter {
 public:
  virtual ~StorageAdapter() = default;

  /**
   * @brief Perform a read towards the storage.
   *
   * @param key The key over which to read the data, this
   * would often map to your primary key, or some index.
   */
  virtual std::optional<T> read(K const &key) = 0;

  /**
   * @brief Perform a write towards the storage.
   *
   * @param key The key over which to read the data, this
   * would often map to your primary key, or some index.
   * @param data The data which to write.
   */
  virtual void write(K const &key, T &&data) = 0;
};

}  // namespace zddm