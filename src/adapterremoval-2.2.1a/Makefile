

PKG_CXXFLAGS = -std=c++11 -O3
PKG_CPPFLAGS = -DR_ENV_FLAG -pthread 

include $(R_HOME)/etc${R_ARCH}/Makeconf

SRCS = adapterset.cc \
alignment.cc \
argparse.cc \
debug.cc \
demultiplex.cc \
fastq.cc \
fastq_enc.cc \
fastq_io.cc \
linereader.cc \
linereader_joined.cc \
main.cc \
main_adapter_id.cc \
main_adapter_rm.cc \
main_demultiplex.cc \
scheduler.cc \
strutils.cc \
threads.cc \
timer.cc \
trimmed_reads.cc \
userconfig.cc \

ADRM_LIBRARY = adrm



all: lib$(ADRM_LIBRARY).a

OBJS = $(SRCS:.cc=.o)

lib$(ADRM_LIBRARY).a:$(OBJS)
	$(AR) cq $@ $^