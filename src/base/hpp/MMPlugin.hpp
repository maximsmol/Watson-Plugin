#pragma once

#include "ISmmPlugin.hpp"

namespace watson {
  struct MMPlugin : public ISmmPlugin, public IMetamodListener
  {
    public:
      // ISmmPlugin
      bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late);
      bool Unload(char *error, size_t maxlen);

      bool Pause(char *error, size_t maxlen);
      bool Unpause(char *error, size_t maxlen);

      void AllPluginsLoaded();

      const char *GetAuthor();
      const char *GetName();
      const char *GetDescription();
      const char *GetURL();
      const char *GetLicense();
      const char *GetVersion();
      const char *GetDate();
      const char *GetLogTag();

      // IMetamodListener
      void OnPluginLoad(PluginId id);
      void OnPluginUnload(PluginId id);

      void OnPluginPause(PluginId id);
      void OnPluginUnpause(PluginId id);

      void OnLevelInit(char const* map,
                      char const* entities,
                      char const* oldMap,
                      char const* landmarkName,
                      bool loadGame,
                      bool background);
      void OnLevelShutdown();

      // void* OnEngineQuery(const char* iface, int* ret);
      // void* OnPhysicsQuery(const char* iface, int* ret);
      // void* OnFileSystemQuery(const char* iface, int* ret);
      // void* OnGameDLLQuery(const char* iface, int* ret);
      // void* OnMetamodQuery(const char* iface, int* ret);

      void OnVSPListening(IServerPluginCallbacks* iface);
      void OnUnlinkConCommandBase(PluginId id, ConCommandBase* pCommand);
  };
}
