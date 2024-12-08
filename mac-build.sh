#!/bin/bash

# Ensures we have all the correct C-libs in scope
stack  build --fast --extra-include-dirs "$(brew --prefix)/include" --extra-lib-dirs "$(brew --prefix)/lib"
