cmake_minimum_required(VERSION 3.9.1)
enable_language(Fortran)

set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)
set(SOURCES
caxpy.f  cher.f    csyr2k.f  dcabs1.f  dspmv.f   dtpmv.f   lsame.f   srot.f   ssyr2k.f  zcopy.f   zher.f    zsyrk.f
ccopy.f  cher2.f   csyrk.f   dcopy.f   dspr.f    dtpsv.f   sasum.f   srotg.f  ssyrk.f   zdotc.f   zher2.f   ztbmv.f
cdotc.f  cher2k.f  ctbmv.f   ddot.f    dspr2.f   dtrmm.f   saxpy.f   ssbmv.f  stbmv.f   zdotu.f   zher2k.f  ztbsv.f
cdotu.f  cherk.f   ctbsv.f   dgbmv.f   dswap.f   dtrmv.f   scasum.f  sscal.f  stbsv.f   zdscal.f  zherk.f   ztpmv.f
cgbmv.f  chpmv.f   ctpmv.f   dgemm.f   dsymm.f   dtrsm.f   scnrm2.f  sspmv.f  stpmv.f   zgbmv.f   zhpmv.f   ztpsv.f
cgemm.f  chpr.f    ctpsv.f   dgemv.f   dsymv.f   dtrsv.f   scopy.f   sspr.f   stpsv.f   zgemm.f   zhpr.f    ztrmm.f
cgemv.f  chpr2.f   ctrmm.f   dger.f    dsyr.f    dzasum.f  sdot.f    sspr2.f  strmm.f   zgemv.f   zhpr2.f   ztrmv.f
cgerc.f  crotg.f   ctrmv.f   dnrm2.f   dsyr2.f   dznrm2.f  sgbmv.f   sswap.f  strmv.f   zgerc.f   zrotg.f   ztrsm.f
cgeru.f  cscal.f   ctrsm.f   drot.f    dsyr2k.f  icamax.f  sgemm.f   ssymm.f  strsm.f   zgeru.f   zscal.f   ztrsv.f
chbmv.f  csscal.f  ctrsv.f   drotg.f   dsyrk.f   idamax.f  sgemv.f   ssymv.f  strsv.f   zhbmv.f   zswap.f
chemm.f  cswap.f   dasum.f   dsbmv.f   dtbmv.f   isamax.f  sger.f    ssyr.f   xerbla.f  zhemm.f   zsymm.f
chemv.f  csymm.f   daxpy.f   dscal.f   dtbsv.f   izamax.f  snrm2.f   ssyr2.f  zaxpy.f   zhemv.f   zsyr2k.f
)

add_library(blas SHARED ${SOURCES})

