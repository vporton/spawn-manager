OBJDIR = obj
COVDIR = $(OBJDIR)/cov

all: spawn_tests

spawn_tests:
	@gnatmake -P$@ -p

tests: spawn_tests spawn_manager
	@$(OBJDIR)/test_runner

spawn_manager:
	@gnatmake -P$@ -p

spawn_performance:
	@gnatmake -P$@ -p

perf: spawn_performance spawn_manager
	@$(OBJDIR)/perf/performance

cov:
	@rm -f $(COVDIR)/*.gcda
	@gnatmake -Pspawn_tests.gpr -p -XBUILD="coverage"
	@$(COVDIR)/test_runner || true
	@lcov -c -d $(COVDIR) -o $(COVDIR)/cov.info
	@lcov -e $(COVDIR)/cov.info "$(PWD)/src/*.adb" -o $(COVDIR)/cov.info
	@genhtml --no-branch-coverage $(COVDIR)/cov.info -o $(COVDIR)

clean:
	@rm -rf $(OBJDIR)

PHONY: clean cov build perf spawn_performance spawn_manager spawn_tests tests
