package zddm

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// This is a hack as we're not really supposed
// to store primitives, but pointers to complex objects
func newString(s string) *string {
	return &s
}

func createAlwaysOnGate() *TrafficGate[string] {
	return NewTrafficGate[string](
		1.0, /* rate = 1.0 means always pass */
		&simpleHasher{},
	)
}

func createTestStorageAdapter() *InMemoryStorageAdapter[string, string] {
	return &InMemoryStorageAdapter[string, string]{
		container_: make(map[string]*string),
	}
}

func TestProxyIsEnabled(t *testing.T) {
	assert := assert.New(t)
	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		/* old */ createTestStorageAdapter(),
		/* new */ createTestStorageAdapter(),
	)

	assert.True(proxy.IsEnabled(), "Proxy should be enabled by default")
	proxy.Disable()
	assert.False(proxy.IsEnabled(), "On disable, should be disabled")
	proxy.Enable()
	assert.True(proxy.IsEnabled(), "On enable, should be enabled")
}

func TestProxyShouldAlwaysReadOldOnDisabled(t *testing.T) {
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// Make sure the same key exists for both storage solutions, with
	// different data [for testing purposes].
	old_adapter.Write("foo", newString("old_bar"))
	new_adapter.Write("foo", newString("bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	proxy.Disable()
	assert.Equal(t, *proxy.Read("foo"), "old_bar", "We should have read from old storage")
}

func TestProxyShouldAlwaysWriteOldOnDisabled(t *testing.T) {
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// Make sure the same key exists for both storage solutions, with
	// different data [for testing purposes].
	old_adapter.Write("foo", newString("old_bar"))
	new_adapter.Write("foo", newString("bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	proxy.Disable()
	proxy.Write("foo", newString("old"))
	assert.Equal(t, *proxy.Read("foo"), "old", "We should have updated the value correctly")

	// Make sure we did not write to new
	assert.Equal(t, *new_adapter.Read("foo"), "bar", "We should have retained the value in the new storage")
}

func TestProxyShouldReadWithPriorityForNew(t *testing.T) {
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// Make sure the same key exists for both storage solutions, with
	// different data [for testing purposes].
	old_adapter.Write("foo", newString("old_bar"))
	new_adapter.Write("foo", newString("bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	assert.Equal(t, *proxy.Read("foo"), "bar", "We should read from the new storage if the key exists")
}

func TestProxyShouldReadOldOnMiss(t *testing.T) {
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// We should only have one value in the old storage
	old_adapter.Write("foo", newString("old_bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	assert.Equal(t, *proxy.Read("foo"), "old_bar", "We should have read from old storage")
}

func TestProxyShouldReadOldAndCopyOnMiss(t *testing.T) {
	assert := assert.New(t)
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// We should only have one value in the old storage
	old_adapter.Write("foo", newString("bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	// First read should hit old storage successfully
	assert.Equal(*proxy.Read("foo"), "bar", "We should have read from old storage")
	// Second read should hit new storage successfully
	assert.Equal(*proxy.Read("foo"), "bar", "We should have read from new storage [again]")

	// Make sure the value has been pushed into the new storage
	newData := new_adapter.Read("foo")
	assert.NotNil(newData, "We should have the value in new storage")
	assert.Equal(*newData, "bar", "We should have written a copy to new storage")
	// Adjust the value to make them diverge, doubling down on that
	// we prioritise the new storage
	new_adapter.Write("foo", newString("new_bar"))
	assert.Equal(*proxy.Read("foo"), "new_bar", "We should have read from new storage")
}

func TestProxyShouldDoubleWrite(t *testing.T) {
	assert := assert.New(t)
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// We should only have one value in the old storage
	old_adapter.Write("foo", newString("bar"))

	proxy := NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	// Updating "foo" should write the update to both storage solutions
	proxy.Write("foo", newString("new_bar"))

	// Both values should be updated
	assert.Equal(*old_adapter.Read("foo"), "new_bar", "Should have updated old storage")
	assert.Equal(*new_adapter.Read("foo"), "new_bar", "Should have updated new storage")
}
