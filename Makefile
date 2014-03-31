# Makefile
.PRECIOUS: %.cc %.h

#RT=sockets
RT=mpi


PROJECT=smti

SOURCES=smti/util/Unit.x10 \
smti/util/Utils.x10 \
smti/util/Monitor.x10 \
smti/util/RandomTools.x10 \
smti/util/Logger.x10 \
smti/solver/Main.x10 \
smti/solver/CSPStats.x10 \
smti/solver/PairAS.x10 \
smti/solver/ElitePool.x10 \
smti/solver/ASSolverParameters.x10 \
smti/solver/SMTIModel.x10 \
smti/solver/ParallelSolverI.x10 \
smti/solver/ASSolverPermut.x10 \
smti/solver/CommManager.x10 \
smti/solver/CSPSharedUnit.x10 \
smti/solver/Maybe.x10 \
smti/solver/PlacesMultiWalks.x10

OBJS=$(SOURCES:.x10=.o)


X10_FLAGS=-NO_CHECKS -O -x10rt $(RT)

ifeq ($(RT),sockets)
  CXX=g++
  CFLAGS_RT=-DTRANSPORT=sockets
else
  CXX=mpicxx
  CFLAGS_RT=-DNO_TRACING 
endif



CFLAGS_DEFS=-Wno-long-long -Wno-unused-parameter -DHOMOGENEOUS -DX10_USE_BDWGC -pthread -I. -I${X10_HOME}/include -I${X10_HOME}/stdlib/include -I. $(CFLAGS_RT)


CFLAGS_OPT=-O2 -DNO_CHECKS -finline-functions 
#CFLAGS_OPT=-O3 -DNO_CHECKS -finline-functions -fomit-frame-pointer 
#CFLAGS_OPT=-g -Wno-unused-variable -Wno-unused-but-set-variable -Wall
CFLAGS=$(CFLAGS_DEFS) $(CFLAGS_OPT)

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


%.cc: %.x10
	@rm -rf src_tmp
	x10c++ $(X10_FLAGS) -commandlineonly -c -d src_tmp $<
	rsync -ca src_tmp/$(PROJECT) .

%.o: %.cc
	$(CXX) $(CFLAGS) -c $< -o $@


xxx_main_xxx.cc:
	@echo '#include <smti/solver/Main.h>' > xxx_main_xxx.cc
	@echo '#include <x10aux/bootstrap.h>' >>xxx_main_xxx.cc
	@echo 'extern "C" { int main(int ac, char **av) { return ::x10aux::template_main< ::smti::solver::Main>(ac,av); } }' >>xxx_main_xxx.cc


Main: $(OBJS) xxx_main_xxx.o
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)



clean:
	rm -rf Main *.o *.h *.cc *.inc *~ src_tmp
	find $(PROJECT) \( -name '*.cc' -o -name '*.h' -o -name '*.o' \) -exec rm \{} \;

smti/solver/Main.x10: smti/util/Logger.x10 smti/solver/ParallelSolverI.x10 smti/solver/PlacesMultiWalks.x10 smti/solver/SMTIModel.x10 smti/solver/Valuation.x10
smti/solver/ParallelSolverI.x10: smti/solver/SMTIModel.x10 smti/solver/Valuation.x10 smti/solver/CSPSharedUnit.x10 smti/solver/CSPStats.x10
smti/solver/PlacesMultiWalks.x10: smti/util/Logger.x10 smti/solver/ParallelSolverI.x10 smti/solver/SMTIModel.x10 smti/solver/ASSolverPermut.x10 smti/solver/CSPStats.x10 smti/solver/CommManager.x10 smti/solver/Maybe.x10  smti/solver/CSPSharedUnit.x10 smti/solver/Valuation.x10
smti/solver/SMTIModel.x10: smti/util/Logger.x10 smti/util/RandomTools.x10 smti/solver/ASSolverParameters.x10 smti/solver/Valuation.x10
smti/solver/ASSolverPermut.x10: smti/util/Logger.x10 smti/solver/ASSolverParameters.x10 smti/solver/SMTIModel.x10 smti/solver/ParallelSolverI.x10 smti/solver/Valuation.x10
smti/solver/CommManager.x10: smti/util/Logger.x10 smti/solver/ElitePool.x10 smti/solver/ParallelSolverI.x10
smti/solver/CSPStats.x10: smti/util/Monitor.x10
smti/solver/ElitePool.x10: smti/util/Logger.x10 smti/util/Monitor.x10 smti/util/Unit.x10 smti/util/Utils.x10 smti/solver/CSPSharedUnit.x10 smti/solver/Maybe.x10 

