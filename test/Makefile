GCC_MACHINE=$(shell gcc -dumpmachine)
$(info "-- GCC_MACHINE = ${GCC_MACHINE}")
ifeq ($(shell echo $(GCC_MACHINE) | sed 's/.*-\(.*\)-.*/\1/'), linux)
  OS=Linux
else ifeq ($(shell echo $(GCC_MACHINE) | sed 's/.*-.*-\([a-zA-Z]*\).*/\1/'), darwin)
  OS=Darwin
else ifeq ($(shell echo $(GCC_MACHINE) | sed 's/.*-.*-\(.*\)/\1/'), cygwin)
  OS=Cygwin
endif
$(info "--      OS = ${OS}")
ifeq ($(OS),Linux)
 export MAKEFILE=Makefile.Linux
endif
ifeq ($(OS),CYGWIN)
 export MAKEFILE=Makefile.Cygwin
endif
ifeq ($(OS),Darwin)
 export MAKEFILE=Makefile.Darwin
endif

$(info "-- MAKEFILE = ${MAKEFILE}")

# set EUSDIR if not defined
export EUSDIR?=$(CURDIR)/..
$(info "--  EUSDIR = ${EUSDIR}")

include $(MAKEFILE)

# test_foreign.c's call_ifunc()/call_ffunc() end with "return g(...)" /
# "return gf(...)", calling back into a defun-c-callable Lisp function
# through a raw function pointer. With -O2, GCC turns that into a sibling
# (tail) call: it pops call_ifunc's own frame and branches to the target
# with `b`/`br` instead of `bl`/`blr`. On aarch64 this breaks unwinding
# back through the pod-code trampoline once the calling frame has also
# called a PLT-bound external function (e.g. printf) earlier -- so keep
# these as real calls.
CFLAGS += -fno-optimize-sibling-calls

SRC=test_foreign.c
OBJ=$(basename $(SRC)).o
LIB=$(basename $(SRC)).$(LSFX)

$(LIB): $(OBJ)
	$(LD) $(SOFLAGS) $(OUTOPT)$(LIB) $(OBJ) $(LDFLAGS)
	@echo "Try make test"

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) -DCOMPILE_LIB -c $(SRC) $(OBJOPT)$(OBJ)

clean:
	rm -f $(LIB) $(OBJ)

test: $(LIB)
	teusgl eus64-test.l

clean :
	\rm -f *.o *.so

