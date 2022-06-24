#include <gtest/gtest.h>
#include <zddm/Proxy.h>

#include <optional>
#include <unordered_map>

namespace {

class TestStorageAdapter
    : public zddm::StorageAdapter<std::string, std::string> {
 public:
  TestStorageAdapter() {
    storage_ = std::make_unique<std::unordered_map<std::string, std::string>>();
  }

  std::optional<std::string> read(std::string const& key) override {
    auto it = storage_->find(key);
    if (it == storage_->end()) {
      return std::nullopt;
    }

    return it->second;
  }

  void write(std::string const& key, std::string&& data) override {
    storage_->insert_or_assign(key, std::move(data));
  }

  // Hacky way to get access to underlying storage after move
  std::unordered_map<std::string, std::string>* getStorage() {
    return storage_.get();
  }

 private:
  std::unique_ptr<std::unordered_map<std::string, std::string>> storage_;
};

}  // namespace

namespace zddm::tests {

TEST(ProxyTests, TestIsEnabled) {
  zddm::Proxy<std::string, std::string> proxy(
      /* old */ std::make_unique<TestStorageAdapter>(),
      /* new */ std::make_unique<TestStorageAdapter>());

  // Enabled by default
  ASSERT_TRUE(proxy.isEnabled());
  // On disable, should be disabled
  ASSERT_FALSE(proxy.disable().isEnabled());
  // On enable, should be disabled
  ASSERT_TRUE(proxy.enable().isEnabled());
}

TEST(ProxyTests, TestShouldAlwaysReadOldOnDisabled) {
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

  proxy.disable();
  ASSERT_EQ(proxy.read("foo"), "old_bar");
}

TEST(ProxyTests, TestShouldAlwaysWriteOldOnDisabled) {
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

  proxy.disable();
  proxy.write("foo", "old");
  ASSERT_EQ(proxy.read("foo"), "old");
}

TEST(ProxyTests, TestShouldReadWithPriorityForNew) {
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_bar");
  new_adapter->write("foo", "bar");

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

  // As we access "foo" we should hit the new storage first by default
  // and get the value stored in the new adapter rather than in the
  // old.
  EXPECT_EQ(proxy.read("foo"), "bar");
}

TEST(ProxyTests, TestShouldReadOldOnMiss) {
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure the same key exists for both storage solutions, with
  // different data [for testing purposes].
  old_adapter->write("foo", "old_foo");

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

  // As we access "foo" we should hit the new storage first by default
  // and get the value stored in the new adapter rather than in the
  // old.
  EXPECT_EQ(proxy.read("foo"), "old_foo");
}

TEST(ProxyTests, TestShouldReadOldOnAndCopyOnMiss) {
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure we only have one value that produces a hit but in the
  // old storage
  old_adapter->write("foo", "bar");

  // Grab a reference so we can check if it exists in storage
  // after handing over ownership of `new_adapter`.
  auto new_storage = new_adapter->getStorage();

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

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
  auto old_adapter = std::make_unique<TestStorageAdapter>();
  auto new_adapter = std::make_unique<TestStorageAdapter>();

  // Make sure we only have one value that produces a hit but in the
  // old storage
  old_adapter->write("foo", "bar");

  // Grab references so we can check if it exists in storage
  // after handing over ownership of `new_adapter`.
  auto old_storage = old_adapter->getStorage();
  auto new_storage = new_adapter->getStorage();

  zddm::Proxy<std::string, std::string> proxy(std::move(old_adapter),
                                              std::move(new_adapter));

  // Updating "foo" should write the update to both storage solutions
  proxy.write("foo", "new_bar");

  // Both values should be present in both storages
  EXPECT_EQ(old_storage->at("foo"), "new_bar");
  EXPECT_EQ(new_storage->at("foo"), "new_bar");
}

}  // namespace zddm::tests