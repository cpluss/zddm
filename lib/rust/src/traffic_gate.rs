use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

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
 * Note that the backing hash function you choose depend on the
 * properties of your storage backend and the data you are migrating.
 *
 * @tparam K The key over which to read the data, this
 * would often map to your primary key, or some index.
 */
pub trait DeterministicHasher<K> {
    // Hash the input key as fast as possible
    fn hash(&self, key: &K) -> u64;

    // Denotes the upper-bound of the hash-function. This is
    // needed as the primary function for the deterministic
    // hasher is to be used as a traffic-gate.
    fn max_size(&self) -> u64;

    fn hash_rate(&self, key: &K) -> f64 {
        return (self.hash(key) as f64) / (self.max_size() as f64);
    }
}

pub struct StandardHasher {}

impl<K: Hash> DeterministicHasher<K> for StandardHasher {
    fn hash(&self, key: &K) -> u64 {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish()
    }

    fn max_size(&self) -> u64 {
        u64::MAX
    }
}

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
pub struct TrafficGate<K> {
    pub rate: f64,
    pub hasher: Box<dyn DeterministicHasher<K>>,
}

impl<K> TrafficGate<K> {
    pub fn should_pass(&self, key: &K) -> bool {
        if self.rate == 0.0 {
            return false;
        }
        if self.rate == 1.0 {
            return true;
        }

        let hash_rate = self.hasher.hash_rate(key);
        return hash_rate < self.rate;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_near {
        ($expected:expr, $actual:expr, $error_margin:expr, $msg:expr) => {{
            let delta = ((($expected as i64) - ($actual as i64)) as i64).abs();
            assert!(delta <= ($error_margin), "{}", $msg);
        }};
    }

    #[test]
    fn pass_rates_test() {
        const TOTAL: i64 = 100000;
        const RATE: f64 = 0.5;
        const PRECISION: f64 = 0.005;
        const ERROR_MARGIN: i64 = (TOTAL as f64 * PRECISION) as i64;

        let gate = TrafficGate {
            rate: RATE,
            hasher: Box::new(StandardHasher {}),
        };

        let mut passes = 0;
        let mut blocks = 0;
        for i in 0..TOTAL {
            if gate.should_pass(&i.to_string()) {
                passes = passes + 1
            } else {
                blocks = blocks + 1
            }
        }

        let expected_passes = (TOTAL as f64) * RATE;
        assert_near!(
            expected_passes,
            passes,
            ERROR_MARGIN,
            format!("Expected {} passes, got {}", expected_passes, passes)
        );

        let expected_blocks = (TOTAL as f64) * (1.0 - RATE);
        assert_near!(
            expected_blocks,
            blocks,
            ERROR_MARGIN,
            format!("Expected {} blocks, got {}", expected_blocks, blocks)
        );
    }
}
