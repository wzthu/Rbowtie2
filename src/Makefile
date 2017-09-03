#include ${R_HOME}/etc$(R_ARCH)/Makeconf
ADRM_DIR = adapterremoval
BT2_DIR = bowtie2


.PHONY: createinst mkadapterremoval mkbowtie2 all clean

all: createinst mkadapterremoval mkbowtie2 

createinst:
	mkdir -p ../inst

mkadapterremoval:
	(cd	$(ADRM_DIR) && ($(MAKE) CXX="$(CXX) -std=gnu++11" -f Makefile))

mkbowtie2:
	(cd $(BT2_DIR) && ($(MAKE) CXX="$(CXX) -std=gnu++11" -f Makefile))


clean:
	(cd	$(ADRM_DIR) && ($(MAKE) clean -f Makefile))
	(cd	$(BT2_DIR) && ($(MAKE) clean -f Makefile))




