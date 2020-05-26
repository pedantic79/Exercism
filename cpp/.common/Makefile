.PHONY: build clean check update-cmakelists

build: build/Makefile *.h *.cpp
	make -C build

clean:
	rm -rf build

build/Makefile: CMakeLists.txt
	cmake -B build -DEXERCISM_RUN_ALL_TESTS=1 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

check: build
	clang-tidy -p build $(filter-out $(wildcard *test.cpp), $(wildcard *.cpp *.h)) -header-filter=.* --checks cppcoreguidelines-*,-cppcoreguidelines-avoid-magic-numbers

update-cmakelists:
	perl -i -pe 's/CXX_STANDARD 14/CXX_STANDARD 17/' CMakeLists.txt
