OCTAVE=octave
MATLAB=matlab
TEST_SCRIPT=wce_demo
SANGOMA_DIR=$(HOME)/src/sangoma/tools/trunk/perturbations/WCE

test: test-octave

test-octave:
	$(OCTAVE) --quiet --no-init-file --path ../examples --eval $(TEST_SCRIPT)

test-matlab:
	echo $(TEST_SCRIPT) | $(MATLAB) -nodisplay

update:
	cp --dereference * $(SANGOMA_DIR)

diff:
	diff . $(SANGOMA_DIR)