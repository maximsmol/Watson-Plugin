#include "hpp/MMPlugin.hpp"

#include "hpp/Base.hpp"

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
  // ISmmPlugin
  Base* detail_::baseInstance = nullptr;

  bool MMPlugin::Load(PluginId id, ISmmAPI* ismm, char* err, size_t errLen, bool late) {
    detail_::baseInstance = new Base(
      ismm,
      id,
      static_cast<SourceHook::ISourceHook*>(ismm->MetaFactory(MMIFACE_SOURCEHOOK, nullptr, nullptr)),
      static_cast<ISmmPlugin*>(this)
    );

    IServerPluginCallbacks* tmp = nullptr;
    if ((tmp = ismm->GetVSPInfo(nullptr)) == nullptr) {
      ismm->AddListener(this, this);
      ismm->EnableVSPListener();
    }
    else
      Base::i().acquiredPluginCbs_();

    return Base::i().load_(err, errLen, late);
  }
  bool MMPlugin::Unload(char* err, size_t errLen) {
    bool res = Base::i().unload_(err, errLen);

    delete detail_::baseInstance;
    return res;
  }

  bool MMPlugin::Pause(char* err, size_t errLen) {
    return Base::i().pause_(err, errLen);
  }
  bool MMPlugin::Unpause(char* err, size_t errLen) {
    return Base::i().unpause_(err, errLen);
  }

  void MMPlugin::AllPluginsLoaded() {
    Base::i().allPluginsLoaded_();
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

  // IMetamodListener
  void MMPlugin::OnPluginLoad(PluginId id) {
  }
  void MMPlugin::OnPluginUnload(PluginId id) {
  }

  void MMPlugin::OnPluginPause(PluginId id) {
  }
  void MMPlugin::OnPluginUnpause(PluginId id) {
  }

  void MMPlugin::OnLevelInit(char const* map,
                  char const* entities,
                  char const* oldMap,
                  char const* landmarkName,
                  bool loadGame,
                  bool background)
  {
  }
  void MMPlugin::OnLevelShutdown() {
  }

  // void* MMPlugin::OnEngineQuery(const char* iface, int* ret) {

  // }
  // void* MMPlugin::OnPhysicsQuery(const char* iface, int* ret) {

  // }
  // void* MMPlugin::OnFileSystemQuery(const char* iface, int* ret) {

  // }
  // void* MMPlugin::OnGameDLLQuery(const char* iface, int* ret) {

  // }
  // void* MMPlugin::OnMetamodQuery(const char* iface, int* ret) {

  // }

  void MMPlugin::OnVSPListening(IServerPluginCallbacks* x) {
    Base::i().pluginCbs_ = x;
    Base::i().acquiredPluginCbs_();
  }
  void MMPlugin::OnUnlinkConCommandBase(PluginId id, ConCommandBase* pCommand) {
  }
}
