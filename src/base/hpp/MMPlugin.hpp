#pragma once

#include <ISmmPlugin.h>

namespace watson {
  struct MMPlugin : public ISmmPlugin
  {
    public:
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
  };
}
