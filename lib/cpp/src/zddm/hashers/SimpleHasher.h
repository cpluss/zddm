#pragma once

#include <functional>

#include "zddm/TrafficGate.h"

namespace zddm::hashers {

/**
 * @brief SimpleHasher is a wrapper to implement the DeterministicHasher
 * interface over std::hash.
 *
 * Should be used primarily in tests, and when you have simple primitives
 * supported by the standard-library.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 */
template <typename K>
class SimpleHasher : public DeterministicHasher<K> {
 public:
  int64_t hash(K const &key) override { return std::hash<K>()(key); }

  int64_t const maxSize() override {
    return std::numeric_limits<std::size_t>::max();
  }
};

}  // namespace zddm::hashers