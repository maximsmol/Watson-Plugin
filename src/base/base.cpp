#include "hpp/base.hpp"

#include <dbg.h>

namespace watson {
  /*static*/ Base& Base::i() {
    return *detail_::baseInstance;
  }

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

  Base::Base(ISmmAPI* mmAPI,
             PluginId plguinId,
             SourceHook::ISourceHook* srcHook,
             ISmmPlugin* mmPluginBase) :
    mmAPI_(mmAPI),
    pluginId_(plguinId),
    srcHook_(srcHook),
    mmPluginBase_(mmPluginBase)
  {}

  bool Base::load(char* err, size_t errLen, bool late) {
    mmAPI()->ConPrintf("Watson Base loaded. late = %d", late);

    return true;
  }
  bool Base::unload(char* err, size_t errLen) {
    return true;
  }

  bool Base::pause(char* err, size_t errLen) {
    return true;
  }
  bool Base::unpause(char* err, size_t errLen) {
    return true;
  }

  void Base::allPluginsLoaded() {}
}
