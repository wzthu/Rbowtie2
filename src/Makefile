#include ${R_HOME}/etc${R_ARCH}/Makeconf
ADRM_DIR = adapterremoval
BT2_DIR = bowtie2

CXX = g++

.PHONY: all clean

all: 
	mkdir -p ../inst
	(cd	$(ADRM_DIR) && ($(MAKE) CXX="$(CXX) -std=gnu++11" -f Makefile))
	(cd $(BT2_DIR) && ($(MAKE) CXX="$(CXX)" -f Makefile))


clean:
	(cd	$(ADRM_DIR) && ($(MAKE) clean -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) clean -f Makefile))




