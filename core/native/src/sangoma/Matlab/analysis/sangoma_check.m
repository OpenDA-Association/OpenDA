function sangoma_check(val1,val2,message,tol,expected_failure)

if nargin == 4
  expected_failure = false;
end

rmsd = rms(val1,val2);
fmt = '%40s: %s (RMS = %g)\n';

if rmsd < tol
  fprintf(fmt,message,' OK ',rmsd);
else
  if expected_failure
    fprintf(fmt,message,'expected failure',rmsd);
  else
    fprintf(fmt,message,'FAIL',rmsd);    
    assert(false);
  end
end
