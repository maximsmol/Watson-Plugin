#pragma once

#include <ISmmPlugin.h>
#include <sourcehook.h>

namespace watson {
  struct Base {
    public:
      static Base& i();

      SourceHook::ISourceHook* srcHook() const;
      ISmmAPI* mmAPI() const;
      ISmmPlugin* mmPluginBase() const;
      PluginId pluginId() const;

      IServerPluginCallbacks* pluginCbs() const;
    private:
      friend struct MMPlugin;

      Base(ISmmAPI* mmAPI,
           PluginId plguinId,
           SourceHook::ISourceHook* srcHook,
           ISmmPlugin* mmPluginBase);

      // ISmmPlugin
      ISmmAPI* mmAPI_;
      PluginId pluginId_;
      SourceHook::ISourceHook* srcHook_;
      ISmmPlugin* mmPluginBase_;

      bool load_(char* err, size_t errLen, bool late);
      bool unload_(char* err, size_t errLen);

      bool pause_(char* err, size_t errLen);
      bool unpause_(char* err, size_t errLen);

      void allPluginsLoaded_();

      // IMetamodListener
      IServerPluginCallbacks* pluginCbs_;
      void acquiredPluginCbs_();
  };
  namespace detail_ {
    extern Base* baseInstance;
  }
}
