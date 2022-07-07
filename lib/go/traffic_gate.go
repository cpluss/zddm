package zddm

// DeterministicHasher is an object which takes a
// generic key K and hashes it, preferably as fast as possible.
//
// This is not to be used for crypto purposes, therefore
// hash collisions should be fine as long as they are partitioned
// with a good statistical distribution.
//
// Do note that this has an assumption built in that every
// hash function has a known upper-bound with a good distribution
// therein.
//
// Note that the backing hash function you choose depend on the
// properties of your storage backend and the data you are migrating.
type DeterministicHasher[K comparable] interface {
	Hash(key K) uint64
	MaxSize() uint64
}

// TrafficGates are configured to let specific keys
// through [deterministically] over time. This allows us to
// control how much of the read & write traffic we're introducing
// the double-write overhead into.
//
// This allows us to start initially small, negating most of the
// performance impact a large amount of reads & writes would cause,
// and slowly ramp-up over time in order to capture the full extent
// of the data.
//
// The better hit-rate we have when reading & writing from the new
// storage backend, the better.
type TrafficGate[K comparable] struct {
	hasher_ DeterministicHasher[K]
	rate_   float64
}

func NewTrafficGate[K comparable](rate float64, hasher DeterministicHasher[K]) *TrafficGate[K] {
	return &TrafficGate[K]{
		hasher_: hasher,
		rate_:   rate,
	}
}

func (self *TrafficGate[K]) ShouldPass(key K) bool {
	// No need to hash and do work if we can avoid it
	if self.rate_ == 0.0 {
		return false
	}
	if self.rate_ >= 1.0 {
		return true
	}

	hashRate := self.hashRate(key)
	return hashRate < self.rate_
}

func (self *TrafficGate[K]) hashRate(key K) float64 {
	hash := self.hasher_.Hash(key)
	maxSize := self.hasher_.MaxSize()
	return float64(hash) / float64(maxSize)
}

func (self *TrafficGate[K]) GetRate() float64 {
	return self.rate_
}

func (self *TrafficGate[K]) SetRate(rate float64) {
	self.rate_ = rate
}
