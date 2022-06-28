#pragma once

#include <memory>

namespace zddm {

/**
 * @brief DeterministicHasher is an object which takes a
 * generic key K and hashes it, preferably as fast as possible.
 *
 * This is not to be used for crypto purposes, therefore
 * hash collisions should be fine as long as they are partitioned
 * with a good statistical distribution.
 *
 * Do note that this has an assumption built in that every
 * hash function has a known upper-bound with a good distribution
 * therein.
 *
 * Example of implementation backed by a simple Md5 implementation
 * would be something along the lines of:
 *
 *    class MD5StringHasher : public DeterministicHasher<std::string> {
 *     protected:
 *      int64_t hash(std::string const& key) override {
 *        std::string hash = md5(key);
 *        return stoul(hash, nullptr, 16);
 *      }
 *
 *      int64_t maxSize() const override {
 *        // Maximum size of md5 is 32-bits all filled
 *        return 0xFFFFFFFFFFFFFFFF;
 *      }
 *    };
 *
 * Note that the backing hash function you choose depend on the
 * properties of your storage backend and the data you are migrating.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 */
template <typename K>
class DeterministicHasher {
 public:
  virtual ~DeterministicHasher() = default;

  float hashRate(K const &key) { return hash(key) / maxSize(); }

 protected:
  // Hash the input key as fast as possible
  virtual int64_t hash(K const &key) = 0;

  // Denotes the upper-bound of the hash-function. This is
  // needed as the primary function for the deterministic
  // hasher is to be used as a traffic-gate.
  virtual int64_t const maxSize() = 0;
};

/**
 * @brief TrafficGates are configured to let specific keys
 * through [deterministically] over time. This allows us to
 * control how much of the read & write traffic we're introducing
 * the double-write overhead into.
 *
 * This allows us to start initially small, negating most of the
 * performance impact a large amount of reads & writes would cause,
 * and slowly ramp-up over time in order to capture the full extent
 * of the data.
 *
 * The better hit-rate we have when reading & writing from the new
 * storage backend, the better.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 */
template <typename K>
class TrafficGate {
 public:
  TrafficGate(std::unique_ptr<DeterministicHasher<K>> &&hasher)
      : rate_(0.0), hasher_(std::move(hasher)) {}
  TrafficGate(float rate, std::unique_ptr<DeterministicHasher<K>> &&hasher)
      : rate_(rate), hasher_(std::move(hasher)) {}

  bool shouldPass(K const &key) {
    // No need to hash and do all the work
    // if we can avoid it
    if (rate_ == 0.0) {
      return false;
    }
    if (rate_ == 1.0) {
      return true;
    }

    float hashRate = hasher_->hashRate(key);
    return hashRate < rate_;
  }

  void setRate(float rate) { rate_ = rate; }
  float getRate() { return rate_; }

 private:
  float rate_;
  std::unique_ptr<DeterministicHasher<K>> hasher_;
};

}  // namespace zddm