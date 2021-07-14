ADRM_DIR = adapterremoval
BT2_DIR = bowtie2-2.4.4

CXX = g++

.PHONY: all clean

all:
	mkdir -p ../inst
	(cd	$(ADRM_DIR) && ($(MAKE) CXX="$(CXX) -std=gnu++11" -f Makefile))
	(cd $(BT2_DIR) && ($(MAKE) CXX="$(CXX)" -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) move -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) clean_dSYM -f Makefile))
	$(CXX) -shared -fPIC -o Rbowtie2.so  version_info.cpp


clean:
	(cd	$(ADRM_DIR) && ($(MAKE) clean -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) clean -f Makefile))
	rm -f *.so
	rm -f *.o




