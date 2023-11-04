#pragma once

#include "common.h"

namespace Luna::NeuralNet {

  extern int16_t inputWeights[NB_FEATURES * NB_HIDDEN];
  extern int16_t inputBias[NB_HIDDEN];

  extern int16_t hiddenWeights[2 * NB_HIDDEN];
  extern int32_t hiddenBias[NB_OUTPUT];

  void init();
}
