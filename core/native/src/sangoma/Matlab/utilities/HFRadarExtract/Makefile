OCTAVE=octave
MATLAB=matlab
TEST_SCRIPT=test_hfradar_extractf
SANGOMA_DIR=$(HOME)/src/sangoma/tools/tags/V0/utilities/HFRadarExtract

test: test-octave

test-octave:
	$(OCTAVE) --quiet --no-init-file --eval $(TEST_SCRIPT)

test-matlab:
	echo $(TEST_SCRIPT) | $(MATLAB) -nodisplay

update:
	cp --dereference * $(SANGOMA_DIR)