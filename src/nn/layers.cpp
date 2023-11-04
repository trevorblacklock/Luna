#include "layers.h"

namespace Luna::NeuralNet {

#define INCBIN_STYLE INCBIN_STYLE_CAMEL
#include "../incbin/incbin.h"
INCBIN(LunaNet, EVALFILE);

alignas(BYTE_ALIGNMENT) int16_t inputWeights[NB_FEATURES * NB_HIDDEN];
alignas(BYTE_ALIGNMENT) int16_t inputBias[NB_HIDDEN];

alignas(BYTE_ALIGNMENT) int16_t hiddenWeights[2 * NB_HIDDEN];
alignas(BYTE_ALIGNMENT) int32_t hiddenBias[NB_OUTPUT];

void init() {
  size_t idx = 0;

  std::memcpy(inputWeights, &gLunaNetData[idx], sizeof(int16_t) * NB_FEATURES * NB_HIDDEN);
  idx += sizeof(int16_t) * NB_FEATURES * NB_HIDDEN;
  std::memcpy(inputBias, &gLunaNetData[idx], sizeof(int16_t) * NB_HIDDEN);
  idx += sizeof(int16_t) * NB_HIDDEN;

  std::memcpy(hiddenWeights, &gLunaNetData[idx], sizeof(int16_t) * 2 * NB_HIDDEN);
  idx += sizeof(int16_t) * 2 * NB_HIDDEN;
  std::memcpy(hiddenBias, &gLunaNetData[idx], sizeof(int32_t));
  idx += sizeof(hiddenBias);
}
}
