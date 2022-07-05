#pragma once

#include <zddm/StorageAdapter.h>

#include <memory>
#include <unordered_map>

namespace zddm::adapters {

/**
 * @brief InMemoryStorageAdapter is a simple wrapper around an unordered
 * map.
 *
 * Note that this takes ownership of a container to store the data
 * within. This helps minimise the surface area for a mutex in case
 * you are accessing this in a concurrent environment.
 *
 * Note that this is *not thread safe*.
 */
template <typename K, typename V>
class InMemoryStorageAdapter : public zddm::StorageAdapter<K, V> {
 public:
  InMemoryStorageAdapter() {
    container_ = std::make_unique<std::unordered_map<K, V>>();
  }
  InMemoryStorageAdapter(std::unique_ptr<std::unordered_map<K, V>> container)
      : container_(std::move(container)) {}

  std::optional<V> read(K const& key) override {
    auto it = container_->find(key);
    if (it == container_->end()) {
      return std::nullopt;
    }

    return it->second;
  }

  void write(K const& key, V&& data) override {
    container_->insert_or_assign(key, std::move(data));
  }

  std::unique_ptr<std::unordered_map<K, V>> const& getWeakPointerToContainer()
      const {
    return container_;
  }

 private:
  // We use a smart pointer to hold the container in order to allow
  // ourselves to access the storage container elsewhere
  std::unique_ptr<std::unordered_map<K, V>> container_;
};

}  // namespace zddm::adapters