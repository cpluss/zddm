package tests

import (
	"math/rand"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/cpluss/zddm/lib/go/zddm"
)

func TestReadMigration(t *testing.T) {
	assert := assert.New(t)
	old_adapter := createTestStorageAdapter()
	new_adapter := createTestStorageAdapter()

	// Generate a bunch of data into the old storage
	// that we want to migrate over to the new one
	N := 100000
	for i := 0; i < N; i++ {
		i_as_string := strconv.Itoa(i)
		old_adapter.Write(
			i_as_string,
			newString(strconv.Itoa(rand.Intn(10000))),
		)
	}

	proxy := zddm.NewProxy[string, string](
		createAlwaysOnGate(),
		old_adapter,
		new_adapter,
	)

	// Read every key and make sure it matches what we have in
	// the old storage
	for i := 0; i < N; i++ {
		i_as_string := strconv.Itoa(i)
		assert.Equal(proxy.Read(i_as_string), old_adapter.Read(i_as_string))
	}

	// Once the entire migration is over we should have the same
	// data in new storage
	for i := 0; i < N; i++ {
		i_as_string := strconv.Itoa(i)
		assert.Equal(new_adapter.Read(i_as_string), old_adapter.Read(i_as_string))
	}
}
