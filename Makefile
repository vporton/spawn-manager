PREFIX ?= $(HOME)/ada

SRCDIR = src
LIBDIR = lib
OBJDIR = obj
COVDIR = $(OBJDIR)/cov

GPR_FILE = gnat/spawn.gpr

BIT = $(shell getconf LONG_BIT)

all: spawn_lib spawn_manager

spawn_tests:
	@gnatmake -P$@ -p -XBIT=$(BIT)

tests: spawn_tests spawn_manager_debug
	@$(OBJDIR)/spawn_manager /tmp/spawn_manager_0 &
	@$(OBJDIR)/test_runner

spawn_manager:
	@gnatmake -P$@ -p -XBIT=$(BIT)

spawn_manager_debug:
	@gnatmake -Pspawn_manager -p -XBIT=$(BIT) -XBUILD="debug"

spawn_performance:
	@gnatmake -P$@ -p -XBIT=$(BIT)

spawn_lib:
	@gnatmake -P$@ -p -XBIT=$(BIT)

perf: spawn_performance spawn_manager
	@$(OBJDIR)/perf/performance

install: install_lib install_manager

install_lib: spawn_lib
	install -d $(PREFIX)/include/spawn
	install -d $(PREFIX)/lib/spawn
	install -d $(PREFIX)/lib/gnat
	install -m 644 $(SRCDIR)/*.ad[bs] $(PREFIX)/include/spawn
	install -m 644 thin/*.ads $(PREFIX)/include/spawn
	install -m 644 thin/$(BIT)/*.ads $(PREFIX)/include/spawn
	install -m 444 $(LIBDIR)/*.ali $(PREFIX)/lib/spawn
	install -m 444 $(OBJDIR)/thin/*.ali $(PREFIX)/lib/spawn
	install -m 444 $(LIBDIR)/libspawn.a $(PREFIX)/lib/spawn
	install -m 644 $(GPR_FILE) $(PREFIX)/lib/gnat

install_manager: spawn_manager
	install -m 755 $(OBJDIR)/spawn_manager $(PREFIX)

cov:
	@rm -f $(COVDIR)/*.gcda
	@gnatmake -Pspawn_tests.gpr -p -XBUILD="coverage" -XBIT=$(BIT)
	@$(COVDIR)/test_runner || true
	@lcov -c -d $(COVDIR) -o $(COVDIR)/cov.info
	@lcov -e $(COVDIR)/cov.info "$(PWD)/src/*.adb" -o $(COVDIR)/cov.info
	@genhtml --no-branch-coverage $(COVDIR)/cov.info -o $(COVDIR)

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)

PHONY: clean cov build install install_lib install_manager perf spawn_lib \
	spawn_performance spawn_manager spawn_manager_debug spawn_tests tests
