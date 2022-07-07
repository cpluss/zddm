package zddm

// StorageAdapters are objects which we configure around
// each storage backend to manage reads & writes. They act as generic
// and thin wrappers which allows us to abstract the interaction away.
//
// The primary purpose is for you to wrap reads & writes to specific
// entities in either storage backend with storage adapters.
//
// In order to avoid this getting out of hand, and ending up with
// a huge number of storage adapters please aim to implement these
// at the lowest common denominator (i.e. as close to the storage
// interaction as possible) in order to minimise the surface area
// for complexity.
//
// Note that this could be a call to virtually anything, even a local
// API.
type StorageAdapter[K any, T any] interface {
	// Read performs a read towards the storage backend.
	Read(key K) *T
	// Write performs a write towards the storage backend.
	Write(key K, data *T)
}
