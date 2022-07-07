package tests

import (
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/cpluss/zddm/lib/go/zddm"
)

func TestTrafficGateSetRate(t *testing.T) {
	assert := assert.New(t)
	gate := zddm.NewTrafficGate[string](0.0 /* rate */, &simpleHasher{})

	assert.Equal(gate.GetRate(), 0.0, "We should start closed by default")
	for r := 0.1; r < 0.9; r += 0.1 {
		gate.SetRate(r)
		assert.Equalf(gate.GetRate(), r, "Should have set rate successfully to %.2f", r)
	}
}

func TestTrafficGatePassRates(t *testing.T) {
	assert := assert.New(t)
	// Do 100k samples
	N := 100000
	// 50%
	rate := 0.5

	gate := zddm.NewTrafficGate[string](rate /* rate */, &simpleHasher{})
	passes := 0
	blocks := 0
	for i := 0; i < N; i++ {
		if gate.ShouldPass(strconv.Itoa(i)) {
			passes++
		} else {
			blocks++
		}
	}

	precision := 0.001 * float64(N)
	expected_passes := rate * float64(N)
	assert.InDelta(
		float64(passes),
		expected_passes,
		precision, "We should be within the expected passes",
	)

	expected_blocks := (1.0 - rate) * float64(N)
	assert.InDelta(
		float64(blocks),
		expected_blocks,
		precision, "We should be within the expected blocks",
	)
}
