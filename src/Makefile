## list phony targets alphabetically
.PHONY: clean default depend distclean

APP=tc

LLVMCONFIG=llvm-config-14

CXX=clang++
CXXFLAGS=`$(LLVMCONFIG) --cxxflags` -std=c++17 -Wall -Werror -g
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all` -lfmt

HDRS := $(wildcard *.hpp)
SRCS := $(wildcard *.cpp)
OBJS := $(patsubst %.cpp,%.o,$(SRCS))

default: $(APP)

$(APP): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

depend: .depend

.depend: $(SRCS)
	@$(RM) .depend
	@$(CXX) $(CXXFLAGS) -MM $^ >> .depend

lint: $(HDRS) $(SRCS)
	@cpplint --quiet --linelength=120 --filter=-legal/copyright,-runtime/references,-build/c++11 $^

clean:
	@$(RM) $(OBJS) *~

distclean: clean
	@$(RM) $(APP) .depend

include .depend
