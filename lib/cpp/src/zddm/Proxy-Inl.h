#ifndef ZDDM_PROXY_H_
#error "This file may only be included by zddm.h"
#endif

namespace zddm {

namespace detail {

template <typename T>
T copy(T t) {
  return t;
}

}  // namespace detail

template <typename K, typename T>
T Proxy<K, T>::read(K const& key) const {
  // We assume that if the proxy is not enabled
  // we can query the old storage solution, and safely
  // unwrap the return value as it is expected to always
  // work (or throw exceptions otherwise).
  //
  // Check if we should do any migration for this key or not
  // using the traffic-gate provided.
  if (!isEnabled() || !gate_->shouldPass(key)) {
    return std::move(old_->read(key).value());
  }

  // First we try to hit `new_`
  auto maybeData = new_->read(key);
  // On a hit return the value, as `new_` should be the
  // source of truth for anything we find therein.
  if (maybeData.has_value()) {
    return std::move(maybeData.value());
  }

  // On a miss we return the value from `old_`, assuming
  // it would be found or return a valid value.
  maybeData = old_->read(key);
  if (!maybeData) {
    throw new std::out_of_range(
        "[ZDDM][Proxy] could not find key in old storage");
  }

  // Write data to the new storage & then return
  auto data = std::move(maybeData.value());

  // Note that this is currently blocking, which we may want
  // to change in the future as it can have quite a drastic
  // latency impact on large workloads.
  new_->write(key, detail::copy(data));
  return std::move(data);
}

template <typename K, typename T>
void Proxy<K, T>::write(K const& key, T&& data) {
  // We assume that we would always, even if disabled, want to
  // write to the old storage. It's the de-facto source
  // of truth.
  // Check if we should do any migration for this key or not
  // using the traffic-gate provided
  if (!isEnabled() || !gate_->shouldPass(key)) {
    // Note how this is distinctly different from below, as we are
    // passing ownership directly to avoid copying if we do not
    // have to.
    old_->write(key, std::move(data));
    return;
  }

  // Perform a double-write. If either write fail we'd rather have
  // the old storage failing first and avoid writing at all
  // to the new storage.
  old_->write(key, detail::copy(data));
  new_->write(key, std::move(data));
}

}  // namespace zddm