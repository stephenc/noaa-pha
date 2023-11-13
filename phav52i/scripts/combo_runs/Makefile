# build used for installing Release Branch x/y/z operational codes ONLY

.PHONY: build

objects = run_test.sh min_neigh.awk filter_a_with_b.ghcn.awk gen_meta.awk \
  convert_mv2_d2m.awk

build:
	mkdir -p ../../build/code/scripts
	cp -v $(objects) ../../build/code/scripts
	cp -v testv52i-pha.sh ../../build
