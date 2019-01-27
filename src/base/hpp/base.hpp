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

    private:
      friend struct MMPlugin;

      Base(ISmmAPI* mmAPI,
           PluginId plguinId,
           SourceHook::ISourceHook* srcHook,
           ISmmPlugin* mmPluginBase);

      ISmmAPI* mmAPI_;
      PluginId pluginId_;
      SourceHook::ISourceHook* srcHook_;
      ISmmPlugin* mmPluginBase_;

      bool load(char* err, size_t errLen, bool late);
      bool unload(char* err, size_t errLen);

      bool pause(char* err, size_t errLen);
      bool unpause(char* err, size_t errLen);

      void allPluginsLoaded();
  };
  namespace detail_ {
    extern Base* baseInstance;
  }
}
