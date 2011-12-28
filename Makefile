all: spawn_performance

spawn_performance:
	@gnatmake -P$@ -p

clean:
	@rm -rf obj

PHONY: clean build spawn_performance
