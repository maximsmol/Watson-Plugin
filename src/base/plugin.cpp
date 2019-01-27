#include <stdio.h>
#include "watson.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
PLUGIN_EXPOSE(watson::Base, watson::g_instance);
#pragma clang diagnostic pop

namespace watson {
	#pragma clang diagnostic push
	#pragma clang diagnostic ignored "-Wglobal-constructors"
	#pragma clang diagnostic ignored "-Wexit-time-destructors"
	Base g_instance;
	#pragma clang diagnostic pop

	bool Base::Load(PluginId id [[maybe_unused]], ISmmAPI* ismm, char* err [[maybe_unused]], size_t errLen [[maybe_unused]], bool late [[maybe_unused]])
	{
		PLUGIN_SAVEVARS();
		return true;
	}
	bool Base::Unload(char* err [[maybe_unused]], size_t errLen [[maybe_unused]])
	{
		return true;
	}

	bool Base::Pause(char* err [[maybe_unused]], size_t errLen [[maybe_unused]]) {
		return true;
	}
	bool Base::Unpause(char* err [[maybe_unused]], size_t errLen [[maybe_unused]]) {
		return true;
	}

	void Base::AllPluginsLoaded() {}

	const char* Base::GetLicense() {
		return "MIT";
	}
	const char* Base::GetVersion() {
		return "0.1.0";
	}
	const char* Base::GetDate() {
		#pragma clang diagnostic push
		#pragma clang diagnostic ignored "-Wdate-time"
		return __DATE__;
		#pragma clang diagnostic pop
	}
	const char* Base::GetLogTag() {
		return "WTSN";
	}
	const char* Base::GetAuthor() {
		return "Maksim Smolin";
	}
	const char* Base::GetDescription() {
		return "Watson base";
	}
	const char* Base::GetName() {
		return "Watson base";
	}
	const char* Base::GetURL() {
		return "";
	}
}
