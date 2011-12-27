all: spawn_manager

spawn_manager:
	@gnatmake -P$@ -p

clean:
	@rm -rf obj

PHONY: clean build spawn_manager
