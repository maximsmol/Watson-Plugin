#include "Base.hpp"
#include "pluginI.hpp"

using namespace watson;

void onBaseAvailable() {
  Base::i().mmAPI()->ConPrint("Watson/test: base available");
}
