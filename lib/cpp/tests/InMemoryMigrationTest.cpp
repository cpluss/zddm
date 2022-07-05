#include <gtest/gtest.h>
#include <zddm/Proxy.h>
#include <zddm/adapters/InMemoryStorageAdapter.h>
#include <zddm/hashers/SimpleHasher.h>

namespace {

// Always open traffic-gate to make the tests easier
template <typename K>
std::unique_ptr<zddm::TrafficGate<K>> createAlwaysOnGate() {
  auto hasher = std::make_unique<zddm::hashers::SimpleHasher<K>>();
  return std::make_unique<zddm::TrafficGate<K>>(
      1.0 /* rate = 1.0 means always pass */, std::move(hasher));
}

}  // namespace

namespace zddm::tests {

TEST(InMemoryMigrationTest, TestReadMigration) {
  auto old_adapter = std::make_unique<
      adapters::InMemoryStorageAdapter<std::string, std::string>>();
  auto new_adapter = std::make_unique<
      adapters::InMemoryStorageAdapter<std::string, std::string>>();

  // Grab references to the underlying in memory storages
  auto const& old_storage = old_adapter->getWeakPointerToContainer();
  auto const& new_storage = new_adapter->getWeakPointerToContainer();

  // Generate a bunch of data into the old storage that we want to
  // migrate into the new storage.
  int const N = 100000;
  for (auto i = 0; i < N; i++) {
    old_adapter->write(std::to_string(i), std::to_string(rand() % 10000));
  }

  Proxy<std::string, std::string> proxy(createAlwaysOnGate<std::string>(),
                                        std::move(old_adapter),
                                        std::move(new_adapter));

  // Read every key and make sure it matches what we have in
  // the old storage
  for (auto i = 0; i < N; i++) {
    auto k = std::to_string(i);
    EXPECT_EQ(proxy.read(k), old_storage->at(k));
  }

  // Once the entire migration has been done we should also
  // have the same data in the new storage
  for (auto i = 0; i < N; i++) {
    auto k = std::to_string(i);
    EXPECT_EQ(new_storage->at(k), old_storage->at(k));
  }
}

}  // namespace zddm::tests