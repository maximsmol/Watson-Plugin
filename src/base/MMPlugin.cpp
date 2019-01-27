#include "hpp/MMPlugin.hpp"

#include "hpp/base.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
static watson::MMPlugin pluginInstance;
#pragma clang diagnostic pop

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
PL_EXPOSURE_FUNC(watson::MMPlugin, pluginInstance);
#pragma clang diagnostic pop

namespace watson {
  Base* detail_::baseInstance = nullptr;

  bool MMPlugin::Load(PluginId id, ISmmAPI* ismm, char* err, size_t errLen, bool late) {
    detail_::baseInstance = new Base(
      ismm,
      id,
      static_cast<SourceHook::ISourceHook*>(ismm->MetaFactory(MMIFACE_SOURCEHOOK, nullptr, nullptr)),
      static_cast<ISmmPlugin*>(this)
    );

    return Base::i().load(err, errLen, late);
  }
  bool MMPlugin::Unload(char* err, size_t errLen) {
    bool res = Base::i().unload(err, errLen);

    delete detail_::baseInstance;
    return res;
  }

  bool MMPlugin::Pause(char* err, size_t errLen) {
    return Base::i().pause(err, errLen);
  }
  bool MMPlugin::Unpause(char* err, size_t errLen) {
    return Base::i().unpause(err, errLen);
  }

  void MMPlugin::AllPluginsLoaded() {
    Base::i().allPluginsLoaded();
  }

  const char* MMPlugin::GetLicense() {
    return "MIT";
  }
  const char* MMPlugin::GetVersion() {
    return "0.1.0";
  }
  const char* MMPlugin::GetDate() {
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wdate-time"
    return __DATE__;
    #pragma clang diagnostic pop
  }
  const char* MMPlugin::GetLogTag() {
    return "WTSN";
  }
  const char* MMPlugin::GetAuthor() {
    return "Maksim Smolin";
  }
  const char* MMPlugin::GetDescription() {
    return "Watson base";
  }
  const char* MMPlugin::GetName() {
    return "Watson base";
  }
  const char* MMPlugin::GetURL() {
    return "";
  }
}
