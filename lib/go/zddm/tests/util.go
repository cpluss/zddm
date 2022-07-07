package tests

import (
	"hash/fnv"
	"math"

	"github.com/cpluss/zddm/lib/go/zddm"
)

type simpleHasher struct{}

func (*simpleHasher) Hash(key string) uint64 {
	h := fnv.New64a()
	h.Write([]byte(key))
	return h.Sum64()
}
func (*simpleHasher) MaxSize() uint64 {
	return math.MaxUint64
}

// This is a hack as we're not really supposed
// to store primitives, but pointers to complex objects
func newString(s string) *string {
	return &s
}

func createAlwaysOnGate() *zddm.TrafficGate[string] {
	return zddm.NewTrafficGate[string](
		1.0, /* rate = 1.0 means always pass */
		&simpleHasher{},
	)
}

func createTestStorageAdapter() *zddm.InMemoryStorageAdapter[string, string] {
	return zddm.NewInMemoryStorageAdapter[string, string]()
}
