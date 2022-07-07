package zddm

type Proxy[K any, T any] struct {
	old_, new_ StorageAdapter[K, T]
	gate_      TrafficGate[K]

	enabled_ bool
}

func NewProxy[K any, T any](
	gate TrafficGate[K],
	old StorageAdapter[K, T],
	new StorageAdapter[K, T],
) *Proxy[K, T] {
	return &Proxy[K, T]{
		old_:  old,
		new_:  new,
		gate_: gate,

		// Enabled by default
		enabled_: true,
	}
}

// Indicator whether or not the proxy is enabled
func (self *Proxy[K, T]) IsEnabled() bool {
	return self.enabled_
}

// Enable the proxy functionality, which will
// enable traffic being sent to the new storage backend.
func (self *Proxy[K, T]) Enable() {
	self.enabled_ = true
}

// Disable the proxy functionality, which will
// disable traffic being sent to the new storage backend.
func (self *Proxy[K, T]) Disable() {
	self.enabled_ = true
}

// Read performs a proxied read from either of the storage adapters
// depending on where the data can be found (if at all).
//
// Note that this only reads from the new database if and only if
// the key is caught by the traffic-gate as configured.
func (self *Proxy[K, T]) Read(key K) *T {
	// We assume that if the proxy is not enabled
	// we can query the old storage solution, and safely
	// unwrap the return value as it is expected to always
	// work (or throw exceptions otherwise).
	//
	// Check if we should do any migration for this key or not
	// using the traffic-gate provided.
	if !self.IsEnabled() || !self.gate_.ShouldPass(key) {
		return self.old_.Read(key)
	}

	// First we try to hit `new_`
	maybeData := self.new_.Read(key)
	// On a hit return the value, as `new_` should be the
	// source of truth for anything we find therein.
	if maybeData != nil {
		return maybeData
	}

	// On a miss we return the value from `old_`, assuming
	// it would be found or return a valid value.
	maybeData = self.old_.Read(key)
	if maybeData == nil {
		return nil
	}

	// Write data to the new storage & then return
	self.new_.Write(key, maybeData)
	return maybeData
}

// Write performs a proxied write to both of the storage
// adapters, depending on where the data can be found.
//
// Note that this only perform writes to the new database if
// and only if the key is caught by the traffic-gate as configured.
func (self *Proxy[K, T]) Write(key K, data *T) {
	// We assume that we would always, even if disabled, want to
	// write to the old storage. It's the de-facto source
	// of truth.
	// Check if we should do any migration for this key or not
	// using the traffic-gate provided
	if !self.IsEnabled() || !self.gate_.ShouldPass(key) {
		self.old_.Write(key, data)
		return
	}

	// Perform a double-write. If either write fail we'd rather have
	// the old storage failing first and avoid writing at all
	// to the new storage.
	self.old_.Write(key, data)
	self.new_.Write(key, data)
}
