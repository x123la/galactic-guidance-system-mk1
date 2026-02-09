PROJECT := galactic_ins.gpr
BIN_DIR := bin
DRIVER_SO := $(BIN_DIR)/libdriver.so
DRIVER_SRC := src/c_driver/driver.c

.PHONY: all ada driver clean run demo viz

all: ada driver

ada:
	gprbuild -P $(PROJECT)

driver:
	mkdir -p $(BIN_DIR)
	gcc -std=c11 -O2 -g -fPIC -shared -pthread $(DRIVER_SRC) -o $(DRIVER_SO) -lm

clean:
	gprclean -P $(PROJECT)
	rm -f $(DRIVER_SO)

run: all
	./bin/main_ins

demo: all
	python3 -u ins_demo.py

viz: all
	python3 -u viz/visualizer.py
