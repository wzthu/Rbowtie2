ADRM_DIR = adapterremoval
BT2_DIR = bowtie2
SAM_DIR = samtools-1.12

CXX = g++

.PHONY: all clean

all: 
	mkdir -p ../inst
	(cd $(SAM_DIR) && ($(MAKE) CXX="$(CXX)" -f Makefile))
	mv ./samtools-1.12/samtools ../inst/
	(cd	$(ADRM_DIR) && ($(MAKE) CXX="$(CXX) -std=gnu++11" -f Makefile))
	(cd $(BT2_DIR) && ($(MAKE) CXX="$(CXX)" -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) move -f Makefile))
	$(CXX) -shared -fPIC -o Rbowtie2.so  version_info.cpp


clean:
	(cd	$(ADRM_DIR) && ($(MAKE) clean -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) clean -f Makefile))
	rm -f *.so
	rm -f *.o
	rm -f ../inst/samtools




