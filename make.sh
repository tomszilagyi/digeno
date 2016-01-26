#!/bin/bash

# '-pa ebin' is needed to compile modules with -behaviour(digeno_callback).
erl -pa ebin -make
