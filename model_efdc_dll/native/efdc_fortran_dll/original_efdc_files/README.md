# Merge notes

For the following files there was no clear, distinct approach to merge
the diffs that were present between the version of EFDC provided by NIER
(start of 2022) and the version present in Openda around the same time.
Each file corresponds with a single commit that introduced the patch for
that file. Note, this might not have been the right way to resolve the
conflicts...

* `CGATEFLX.for`: The NIER source misses the fix introducing boolean
  `HUPG_HDWG_INITIALIZED` that was added in 2016 in OpenDA.
  Additionally, the array GKMULT seems not to have been initialized in
  all possible situations and could have been used uninitialized in
  some. These patches were not brought back to OpenDA.
