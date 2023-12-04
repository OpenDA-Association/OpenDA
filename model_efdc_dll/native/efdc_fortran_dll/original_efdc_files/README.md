# Merge notes

For the following files there was no clear, distinct approach to merge
the diffs that were present between the version of EFDC provided by NIER
(start of 2022) and the version present in Openda around the same time.
Each file corresponds with a single commit that introduced the patch for
that file. Note, this might not have been the right way to resolve the
conflicts...

* `CALAVBOLD_mpi.for`: It is noted that `CALAVBOLD_mpi` performs
  calculations using `SQRT` while the corresponding non-MPI
  implementation uses multiplication with `0.5`... It is unclear why
  this difference exists. No action has been taken to unify these
  computations.
* `CGATEFLX.for`: The NIER source misses the fix introducing boolean
  `HUPG_HDWG_INITIALIZED` that was added in 2016 in OpenDA.
  Additionally, the array GKMULT seems not to have been initialized in
  all possible situations and could have been used uninitialized in
  some. These patches were not brought back to OpenDA.
* `HDMT2T.for`: The NIER source does not invoke `CALEXP2TO` any longer
  and shows slightly modified if-statements to consider which subroutine
  to call. This subroutine is also no longer present in the provided
  sources. It has been decided to cherry-pick these line diffs and
  remove the remaining source file corresponding to `CALEXP2TO` all
  together.
* `RWQBEN2.for`: There seemed to be a patch missing in OpenDA. The NIER
  version is taken for this file where a slightly different (possibly
  renamed) set of variables are used to extract properties from cards.
  These seem to be reassigned at previously used variables elsewhere in
  the source.
* `VARINIT.for`: The comparison for `NQCTYPM` has been changed from
  `.EQ.` to `.GE`. This seems mostly used. Inspection of the input decks
  does not give more hints to the proper use of these values. Also,
  `LCMWQ` setting is updated to match NIER.
* `WQ3D.for`: The version in OpenDA also considered `TASER` values in
  the condition for the various while-loops and other statements. The
  decision was made that the version in OpenDA is accurate and the
  patches are to be propagated back to NIER.
* `WQSKE3.for`: This includes the missing loops (2 chunks) regarding
  "green algae salinity tox" from NIER towards OpenDA. Additionally,
  this converts all comparisons in OpenDA of the form
  `IF(LMASKDRY(L).AND.IWQM.GE.1)THEN` from `.AND.` to `.OR.` to be
  consistent with all other comparisons done like this. Also, the
  comparison `IF(IWQBEN.EQ.1)THEN` now compares to zero instead.
