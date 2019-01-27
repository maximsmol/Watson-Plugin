#include "hpp/Base.hpp"

#include <dbg.h>

namespace watson {
  /*static*/ Base& Base::i() {
    return *detail_::baseInstance;
  }

  Base::Base(ISmmAPI* mmAPI,
             PluginId plguinId,
             SourceHook::ISourceHook* srcHook,
             ISmmPlugin* mmPluginBase) :
    mmAPI_(mmAPI),
    pluginId_(plguinId),
    srcHook_(srcHook),
    mmPluginBase_(mmPluginBase)
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

  bool Base::load_(char* err, size_t errLen, bool late) {
    mmAPI()->ConPrintf("Watson Base loaded. late = %d\n", late);

    return true;
  }
  bool Base::unload_(char* err, size_t errLen) {
    return true;
  }

  bool Base::pause_(char* err, size_t errLen) {
    return true;
  }
  bool Base::unpause_(char* err, size_t errLen) {
    return true;
  }

  void Base::allPluginsLoaded_() {}

  // IMetamodListener
  IServerPluginCallbacks* Base::pluginCbs() const {
    return pluginCbs_;
  }

  void Base::acquiredPluginCbs_() {
    mmAPI()->ConPrintf("Acquired IServerPluginCallbacks\n");
  }
}
