#pragma once

#include <vector>

#include "ISmmPlugin.hpp"
#include <sourcehook.h>

namespace watson {
  namespace base {
    namespace listener {
      using load = bool (*)(char* err, size_t errLen);
      using unload = bool (*)(char* err, size_t errLen);

      using pause = bool (*)(char* err, size_t errLen);
      using unpause = bool (*)(char* err, size_t errLen);

      using allPluginsLoad = void (*)();
    }
    namespace detail_ {
      struct Listeners {
        public:
          std::vector<base::listener::load> load;
          std::vector<base::listener::unload> unload;

          std::vector<base::listener::pause> pause;
          std::vector<base::listener::unpause> unpause;

          std::vector<base::listener::allPluginsLoad> allPluginsLoad;
      };
    }
  }

  struct Base {
    public:
      static Base& i();

      SourceHook::ISourceHook* srcHook() const;
      ISmmAPI* mmAPI() const;
      ISmmPlugin* mmPluginBase() const;
      PluginId pluginId() const;
      bool loadedLate() const;

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

      void allPluginsLoad_();

      bool loaded_;
      bool loadedLate_;
      bool allPluginsLoaded_;

      base::detail_::Listeners listeners_;
      base::detail_::Listeners listenersNeedingPluginCbs_;

      // IMetamodListener
      IServerPluginCallbacks* pluginCbs_;
      void acquiredPluginCbs_();
  };
  namespace base::detail_ {
    extern Base* baseInstance;
  }
}
