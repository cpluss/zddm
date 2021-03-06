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
type StorageAdapter[K comparable, T any] interface {
	// Read performs a read towards the storage backend.
	Read(key K) *T
	// Write performs a write towards the storage backend.
	Write(key K, data *T)
}

// InMemoryStorageAdapter is a simple wrapper around a map.
//
// Note that this takes ownership of a container to store the data
// within. This helps minimise the surface area for a mutex in case
// you are accessing this in a concurrent environment.
//
// Note that this is *not thread safe*.
type InMemoryStorageAdapter[K comparable, T any] struct {
	container_ map[K]*T
}

func NewInMemoryStorageAdapter[K comparable, T any]() *InMemoryStorageAdapter[K, T] {
	return &InMemoryStorageAdapter[K, T]{
		container_: make(map[K]*T),
	}
}

func (self *InMemoryStorageAdapter[K, T]) Read(key K) *T {
	return self.container_[key]
}

func (self *InMemoryStorageAdapter[K, T]) Write(key K, data *T) {
	self.container_[key] = data
}
