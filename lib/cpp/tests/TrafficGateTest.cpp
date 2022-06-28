#include <gtest/gtest.h>
#include <zddm/TrafficGate.h>
#include <zddm/hashers/SimpleHasher.h>

namespace zddm::tests {

TEST(TrafficGateTest, TestSetRate) {
  TrafficGate<std::string> gate(
      std::make_unique<hashers::SimpleHasher<std::string>>());

  // We should start closed by default
  EXPECT_EQ(gate.getRate(), 0.0);

  for (float r = 0.1; r < 0.9; r += 0.1) {
    gate.setRate(r);
    EXPECT_EQ(gate.getRate(), r);
  }
}

TEST(TrafficGateTest, TestPassRates) {
  // Do 100k samples
  int64_t const N = 100000;
  // 50%
  float rate = 0.5;

  TrafficGate<std::string> gate(
      rate, std::make_unique<hashers::SimpleHasher<std::string>>());
  int passes = 0;
  int blocks = 0;
  for (int64_t i = 0; i < N; i++) {
    if (gate.shouldPass(std::to_string(i))) {
      passes++;
    } else {
      blocks++;
    }
  }

  // We should be within 0.1% precision
  EXPECT_NEAR(passes, rate * N, 0.001 * N);
  EXPECT_NEAR(blocks, rate * N, 0.001 * N);
};

}  // namespace zddm::tests