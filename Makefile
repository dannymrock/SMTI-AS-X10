# Makefile
.PRECIOUS: %.cc %.h %.d
SUFFIXES += .d .cc .x10

#We don't need to clean up when we're making these targets
NODEPS=rsync clean

#RT=sockets
RT=mpi


PROJECT=smti

SOURCES_X10=$(shell find $(PROJECT)/ -name "*.x10")
SOURCES_CPP=$(SOURCES_X10:.x10=.cc)
DEPFILES=$(SOURCES_CPP:.cc=.d)
OBJS=$(SOURCES_CPP:.cc=.o)



X10_FLAGS=-NO_CHECKS -O -x10rt $(RT)

ifeq ($(RT),sockets)
  CXX=g++
  CXXFLAGS_RT=-DTRANSPORT=sockets
else
  CXX=mpicxx
  CXXFLAGS_RT=-DNO_TRACING 
endif


CXX_X10_INCLUDE=-I${X10_HOME}/include -I${X10_HOME}/stdlib/include

CXXFLAGS_DEFS=-Wno-long-long -Wno-unused-parameter -DHOMOGENEOUS -DX10_USE_BDWGC -pthread $(CXX_X10_INCLUDE) -I. $(CXXFLAGS_RT)


CXXFLAGS_OPT=-O2 -DNO_CHECKS -finline-functions 
#CXXFLAGS_OPT=-O3 -DNO_CHECKS -finline-functions -fomit-frame-pointer 
#CXXFLAGS_OPT=-g -Wno-unused-variable -Wno-unused-but-set-variable -Wall
CXXFLAGS=$(CXXFLAGS_DEFS) $(CXXFLAGS_OPT)

LDFLAGS=-Wl,--no-as-needed -pthread

LIBS=-L${X10_HOME}/stdlib/lib -lx10 -lgc -lm -lpthread -lrt -ldl -L${X10_HOME}/lib -lx10rt_$(RT) -Wl,--rpath -Wl,${X10_HOME}/stdlib/lib -Wl,--rpath -Wl,${X10_HOME}/lib -Wl,-export-dynamic


inc: rsync Main

full: rsync compile Main

rsync:
	rsync -a danny@cri-hpc1.univ-paris1.fr:workspace/SMTI-files/src/$(PROJECT) .

compile: 
	echo sources $(SOURCES)
	@rm -rf src_tmp
	x10c++ $(X10_FLAGS) -commandlineonly -c -d src_tmp $(SOURCES)
	rsync -ca src_tmp/$(PROJECT) .



#Don't create dependencies when we're cleaning, for instance
ifeq (0, $(words $(findstring $(MAKECMDGOALS), $(NODEPS))))
    #Chances are, these files don't exist.  GMake will create them and
    #clean up automatically afterwards
    -include $(DEPFILES)
endif

# some .x10 files do not produce .cc output (type defs). 
# (in that case src_tmp is not created). Create an empty .cc file
%.cc: %.x10
	@rm -rf src_tmp
	x10c++ $(X10_FLAGS) -commandlineonly -c -d src_tmp $<
	@if [ -d src_tmp ]; then rsync -ca src_tmp/$(PROJECT) .; else touch $@; fi

# do not pass -I for x10 includes else they are added by -MM in the .d files
%.d: %.cc
	g++ -I. -MM -MF $@ $<

%.o: %.cc %.d
	$(CXX) $(CXXFLAGS) -c $< -o $@


xxx_main_xxx.cc:
	@echo '#include <smti/solver/Main.h>' > xxx_main_xxx.cc
	@echo '#include <x10aux/bootstrap.h>' >>xxx_main_xxx.cc
	@echo 'extern "C" { int main(int ac, char **av) { return ::x10aux::template_main< ::smti::solver::Main>(ac,av); } }' >>xxx_main_xxx.cc


Main: $(OBJS) xxx_main_xxx.o
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)



clean:
	rm -rf Main xxx_main_xxx.cc *.inc *~ src_tmp
	find $(PROJECT) \( -name '*.cc' -o -name '*.h' -o -name '*.o' -o -name '*.d' \) -exec rm \{} \;
