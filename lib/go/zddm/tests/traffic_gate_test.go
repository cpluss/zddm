package tests

import (
	"fmt"
	"hash/fnv"
	"math"
	"strconv"
	"testing"

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

func assertNear(t *testing.T, a float64, b float64, precision float64, msg string) {
	delta := math.Abs(a - b)
	if delta > precision {
		t.Error(msg)
	}
}

func TestTrafficGateSetRate(t *testing.T) {
	gate := zddm.NewTrafficGate[string](0.0 /* rate */, &simpleHasher{})

	if gate.GetRate() != 0.0 {
		t.Error("We should start closed by default")
	}

	for r := 0.1; r < 0.9; r += 0.1 {
		gate.SetRate(r)
		if gate.GetRate() != r {
			t.Errorf("Should have set rate successfully to %f", r)
		}
	}
}

func TestTrafficGatePassRates(t *testing.T) {
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
	assertNear(
		t,
		float64(passes),
		expected_passes,
		precision,
		fmt.Sprintf("Expected %.2f passes out of %d attempts, but got %d.", expected_passes, N, passes))

	expected_blocks := (1.0 - rate) * float64(N)
	assertNear(
		t,
		float64(blocks),
		expected_blocks,
		precision,
		fmt.Sprintf("Expected %.2f blocks out of %d attempts, but got %d.", expected_blocks, N, blocks))
}
