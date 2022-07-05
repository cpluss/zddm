#include <gtest/gtest.h>
#include <zddm/Proxy.h>
#include <zddm/adapters/InMemoryStorageAdapter.h>
#include <zddm/hashers/SimpleHasher.h>

#include <optional>
#include <unordered_map>

namespace {

// Always open traffic-gate to make the tests easier
std::unique_ptr<zddm::TrafficGate<std::string>> createAlwaysOnGate() {
  auto hasher = std::make_unique<zddm::hashers::SimpleHasher<std::string>>();
  return std::make_unique<zddm::TrafficGate<std::string>>(
      1.0 /* rate = 1.0 means always pass */, std::move(hasher));
}

std::unique_ptr<
    zddm::adapters::InMemoryStorageAdapter<std::string, std::string>>
createTestStorageAdapter() {
  return std::make_unique<
      zddm::adapters::InMemoryStorageAdapter<std::string, std::string>>();
}

}  // namespace

namespace zddm::tests {

TEST(ProxyTests, TestIsEnabled) {
  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(),
      /* old */ createTestStorageAdapter(),
      /* new */ createTestStorageAdapter());

  // Enabled by default
  ASSERT_TRUE(proxy.isEnabled());
  // On disable, should be disabled
  ASSERT_FALSE(proxy.disable().isEnabled());
  // On enable, should be disabled
  ASSERT_TRUE(proxy.enable().isEnabled());
}

TEST(ProxyTests, TestShouldAlwaysReadOldOnDisabled) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  proxy.disable();
  ASSERT_EQ(proxy.read("foo"), "old_bar");
}

TEST(ProxyTests, TestShouldAlwaysWriteOldOnDisabled) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  proxy.disable();
  proxy.write("foo", "old");
  ASSERT_EQ(proxy.read("foo"), "old");
}

TEST(ProxyTests, TestShouldReadWithPriorityForNew) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  // As we access "foo" we should hit the new storage first by default
  // and get the value stored in the new adapter rather than in the
  // old.
  EXPECT_EQ(proxy.read("foo"), "bar");
}

TEST(ProxyTests, TestShouldReadOldOnMiss) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_foo");

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  // As we access "foo" we should hit the new storage first by default
  // and get the value stored in the new adapter rather than in the
  // old.
  EXPECT_EQ(proxy.read("foo"), "old_foo");
}

TEST(ProxyTests, TestShouldReadOldOnAndCopyOnMiss) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure we only have one value that produces a hit but in the
  // old storage
  old_adapter->write("foo", "bar");

  // Grab a reference so we can check if it exists in storage
  // after handing over ownership of `new_adapter`.
  auto const& new_storage = new_adapter->getWeakPointerToContainer();

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  // First read should hit the old storage successfully
  EXPECT_EQ(proxy.read("foo"), "bar");
  // New read should hit the new storage successfully
  EXPECT_EQ(proxy.read("foo"), "bar");

  // Make sure we have the value in new storage, as expected
  EXPECT_TRUE(new_storage->find("foo") != new_storage->end());
  // Adjust the value to make them diverge, doubling down on that
  // we prioritise the new storage
  new_storage->insert_or_assign("foo", "new_bar");
  // New read should hit the new storage successfully
  EXPECT_EQ(proxy.read("foo"), "new_bar");
}

TEST(ProxyTests, TestShouldDoubleWrite) {
  auto old_adapter = createTestStorageAdapter();
  auto new_adapter = createTestStorageAdapter();

  // Make sure we only have one value that produces a hit but in the
  // old storage
  old_adapter->write("foo", "bar");

  // Grab references so we can check if it exists in storage
  // after handing over ownership of `new_adapter`.
  auto const& old_storage = new_adapter->getWeakPointerToContainer();
  auto const& new_storage = new_adapter->getWeakPointerToContainer();

  zddm::Proxy<std::string, std::string> proxy(
      createAlwaysOnGate(), std::move(old_adapter), std::move(new_adapter));

  // Updating "foo" should write the update to both storage solutions
  proxy.write("foo", "new_bar");

  // Both values should be present in both storages
  EXPECT_EQ(old_storage->at("foo"), "new_bar");
  EXPECT_EQ(new_storage->at("foo"), "new_bar");
}

}  // namespace zddm::tests