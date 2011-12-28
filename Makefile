OBJDIR = obj
COVDIR = $(OBJDIR)/cov

all: spawn_tests

spawn_tests:
	@gnatmake -P$@ -p

tests: spawn_tests
	@$(OBJDIR)/test_runner

spawn_performance:
	@gnatmake -P$@ -p

perf: spawn_performance
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

PHONY: clean cov build perf spawn_performance spawn_tests tests
