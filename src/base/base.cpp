#include "hpp/Base.hpp"

#include <dbg.h>

namespace watson {
  /*static*/ Base& Base::i() {
    return *base::detail_::baseInstance;
  }

  Base::Base(ISmmAPI* mmAPI,
             PluginId plguinId,
             SourceHook::ISourceHook* srcHook,
             ISmmPlugin* mmPluginBase) :
    mmAPI_(mmAPI),
    pluginId_(plguinId),
    srcHook_(srcHook),
    mmPluginBase_(mmPluginBase),

    loaded_(true),
    loadedLate_(false),
    allPluginsLoaded_(false),

    listeners_(),
    listenersNeedingPluginCbs_(),

    pluginCbs_(nullptr)
  {}

  // ISmmPlugin
  SourceHook::ISourceHook* Base::srcHook() const {
    return srcHook_;
  }
  ISmmAPI* Base::mmAPI() const {
    return mmAPI_;
  }
  ISmmPlugin* Base::mmPluginBase() const {
    return mmPluginBase_;
  }
  PluginId Base::pluginId() const {
    return pluginId_;
  }
  bool Base::loadedLate() const {
    return loadedLate_;
  }

  bool Base::load_(char* err, size_t errLen, bool late) {
    loadedLate_ = late;

    for (base::listener::load l : listeners_.load)
      l(err, errLen);

    if (pluginCbs() != nullptr)
      for (base::listener::load l : listenersNeedingPluginCbs_.load)
        l(err, errLen);

    return true;
  }
  bool Base::unload_(char* err, size_t errLen) {
    for (base::listener::unload l : listeners_.unload)
      l(err, errLen);

    if (pluginCbs() != nullptr)
      for (base::listener::unload l : listenersNeedingPluginCbs_.unload)
        l(err, errLen);

    return true;
  }

  bool Base::pause_(char* err, size_t errLen) {
    for (base::listener::pause l : listeners_.pause)
      l(err, errLen);

    if (pluginCbs() != nullptr)
      for (base::listener::pause l : listenersNeedingPluginCbs_.pause)
        l(err, errLen);

    return true;
  }
  bool Base::unpause_(char* err, size_t errLen) {
    for (base::listener::unpause l : listeners_.unpause)
      l(err, errLen);

    if (pluginCbs() != nullptr)
      for (base::listener::unpause l : listenersNeedingPluginCbs_.unpause)
        l(err, errLen);

    return true;
  }

  void Base::allPluginsLoad_() {
    for (base::listener::allPluginsLoad l : listeners_.allPluginsLoad)
      l();

    if (pluginCbs() != nullptr)
      for (base::listener::allPluginsLoad l : listenersNeedingPluginCbs_.allPluginsLoad)
        l();
  }

  // IMetamodListener
  IServerPluginCallbacks* Base::pluginCbs() const {
    return pluginCbs_;
  }

  void Base::acquiredPluginCbs_() {
    if (loaded_)
      for (base::listener::load l : listenersNeedingPluginCbs_.load)
        l(nullptr, 0);
    if (allPluginsLoaded_)
      for (base::listener::allPluginsLoad l : listenersNeedingPluginCbs_.allPluginsLoad)
        l();
  }
}
