build: build/Makefile *.h *.cpp
	make -C build

clean:
	rm -rf build

build/Makefile: CMakeLists.txt
	cmake -B build -DEXERCISM_RUN_ALL_TESTS=1 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
