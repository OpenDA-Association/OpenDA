cmake_minimum_required(VERSION 3.9.1)
enable_language(Fortran)
# add_compile_options(-Wall -Weffc++ -pedantic -std=c++0x)

set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)
set(SOURCES
cbdsqr.f  chetrd.f  clatbs.f  ctrtrs.f  dggsvp.f  dlasd1.f  dpttrs.f  sdisna.f  slaev2.f  slatrs.f  ssteqr.f  zgerq2.f  zlahef.f  zptsv.f
cgbbrd.f  chetrf.f  clatdf.f  ctzrqf.f  dgtcon.f  dlasd2.f  dptts2.f  second.f  slaexc.f  slatrz.f  ssterf.f  zgerqf.f  zlahqr.f  zptsvx.f
cgbcon.f  chetri.f  clatps.f  ctzrzf.f  dgtrfs.f  dlasd3.f  drscl.f   sgbbrd.f  slag2.f   slatzm.f  sstev.f   zgesc2.f  zlahrd.f  zpttrf.f
cgbequ.f  chetrs.f  clatrd.f  cung2l.f  dgtsv.f   dlasd4.f  dsbev.f   sgbcon.f  slags2.f  slauu2.f  sstevd.f  zgesdd.f  zlaic1.f  zpttrs.f
cgbrfs.f  chgeqz.f  clatrs.f  cung2r.f  dgtsvx.f  dlasd5.f  dsbevd.f  sgbequ.f  slagtf.f  slauum.f  sstevr.f  zgesv.f   zlals0.f  zptts2.f
cgbsv.f   chpcon.f  clatrz.f  cungbr.f  dgttrf.f  dlasd6.f  dsbevx.f  sgbrfs.f  slagtm.f  sopgtr.f  sstevx.f  zgesvd.f  zlalsa.f  zrot.f
cgbsvx.f  chpev.f   clatzm.f  cunghr.f  dgttrs.f  dlasd7.f  dsbgst.f  sgbsv.f   slagts.f  sopmtr.f  ssycon.f  zgesvx.f  zlalsd.f  zspcon.f
cgbtf2.f  chpevd.f  clauu2.f  cungl2.f  dgtts2.f  dlasd8.f  dsbgv.f   sgbsvx.f  slagv2.f  sorg2l.f  ssyev.f   zgetc2.f  zlangb.f  zspmv.f
cgbtrf.f  chpevx.f  clauum.f  cunglq.f  dhgeqz.f  dlasd9.f  dsbgvd.f  sgbtf2.f  slahqr.f  sorg2r.f  ssyevd.f  zgetf2.f  zlange.f  zspr.f
cgbtrs.f  chpgst.f  cpbcon.f  cungql.f  dhsein.f  dlasda.f  dsbgvx.f  sgbtrf.f  slahrd.f  sorgbr.f  ssyevr.f  zgetrf.f  zlangt.f  zsprfs.f
cgebak.f  chpgv.f   cpbequ.f  cungqr.f  dhseqr.f  dlasdq.f  dsbtrd.f  sgbtrs.f  slaic1.f  sorghr.f  ssyevx.f  zgetri.f  zlanhb.f  zspsv.f
cgebal.f  chpgvd.f  cpbrfs.f  cungr2.f  dlabad.f  dlasdt.f  dsecnd.f  sgebak.f  slaln2.f  sorgl2.f  ssygs2.f  zgetrs.f  zlanhe.f  zspsvx.f
cgebd2.f  chpgvx.f  cpbstf.f  cungrq.f  dlabrd.f  dlaset.f  dspcon.f  sgebal.f  slals0.f  sorglq.f  ssygst.f  zggbak.f  zlanhp.f  zsptrf.f
cgebrd.f  chprfs.f  cpbsv.f   cungtr.f  dlacon.f  dlasq1.f  dspev.f   sgebd2.f  slalsa.f  sorgql.f  ssygv.f   zggbal.f  zlanhs.f  zsptri.f
cgecon.f  chpsv.f   cpbsvx.f  cunm2l.f  dlacpy.f  dlasq2.f  dspevd.f  sgebrd.f  slalsd.f  sorgqr.f  ssygvd.f  zgges.f   zlanht.f  zsptrs.f
cgeequ.f  chpsvx.f  cpbtf2.f  cunm2r.f  dladiv.f  dlasq3.f  dspevx.f  sgecon.f  slamch.f  sorgr2.f  ssygvx.f  zggesx.f  zlansb.f  zstedc.f
cgees.f   chptrd.f  cpbtrf.f  cunmbr.f  dlae2.f   dlasq4.f  dspgst.f  sgeequ.f  slamrg.f  sorgrq.f  ssyrfs.f  zggev.f   zlansp.f  zstegr.f
cgeesx.f  chptrf.f  cpbtrs.f  cunmhr.f  dlaebz.f  dlasq5.f  dspgv.f   sgees.f   slangb.f  sorgtr.f  ssysv.f   zggevx.f  zlansy.f  zstein.f
cgeev.f   chptri.f  cpocon.f  cunml2.f  dlaed0.f  dlasq6.f  dspgvd.f  sgeesx.f  slange.f  sorm2l.f  ssysvx.f  zggglm.f  zlantb.f  zsteqr.f
cgeevx.f  chptrs.f  cpoequ.f  cunmlq.f  dlaed1.f  dlasr.f   dspgvx.f  sgeev.f   slangt.f  sorm2r.f  ssytd2.f  zgghrd.f  zlantp.f  zsycon.f
cgegs.f   chsein.f  cporfs.f  cunmql.f  dlaed2.f  dlasrt.f  dsprfs.f  sgeevx.f  slanhs.f  sormbr.f  ssytf2.f  zgglse.f  zlantr.f  zsymv.f
cgegv.f   chseqr.f  cposv.f   cunmqr.f  dlaed3.f  dlassq.f  dspsv.f   sgegs.f   slansb.f  sormhr.f  ssytrd.f  zggqrf.f  zlapll.f  zsyr.f
cgehd2.f  clabrd.f  cposvx.f  cunmr2.f  dlaed4.f  dlasv2.f  dspsvx.f  sgegv.f   slansp.f  sorml2.f  ssytrf.f  zggrqf.f  zlapmt.f  zsyrfs.f
cgehrd.f  clacgv.f  cpotf2.f  cunmr3.f  dlaed5.f  dlaswp.f  dsptrd.f  sgehd2.f  slanst.f  sormlq.f  ssytri.f  zggsvd.f  zlaqgb.f  zsysv.f
cgelq2.f  clacon.f  cpotrf.f  cunmrq.f  dlaed6.f  dlasy2.f  dsptrf.f  sgehrd.f  slansy.f  sormql.f  ssytrs.f  zggsvp.f  zlaqge.f  zsysvx.f
cgelqf.f  clacp2.f  cpotri.f  cunmrz.f  dlaed7.f  dlasyf.f  dsptri.f  sgelq2.f  slantb.f  sormqr.f  stbcon.f  zgtcon.f  zlaqhb.f  zsytf2.f
cgels.f   clacpy.f  cpotrs.f  cunmtr.f  dlaed8.f  dlatbs.f  dsptrs.f  sgelqf.f  slantp.f  sormr2.f  stbrfs.f  zgtrfs.f  zlaqhe.f  zsytrf.f
cgelsd.f  clacrm.f  cppcon.f  cupgtr.f  dlaed9.f  dlatdf.f  dstebz.f  sgels.f   slantr.f  sormr3.f  stbtrs.f  zgtsv.f   zlaqhp.f  zsytri.f
cgelss.f  clacrt.f  cppequ.f  cupmtr.f  dlaeda.f  dlatps.f  dstedc.f  sgelsd.f  slanv2.f  sormrq.f  stgevc.f  zgtsvx.f  zlaqp2.f  zsytrs.f
cgelsx.f  cladiv.f  cpprfs.f  dbdsdc.f  dlaein.f  dlatrd.f  dstegr.f  sgelss.f  slapll.f  sormrz.f  stgex2.f  zgttrf.f  zlaqps.f  ztbcon.f
cgelsy.f  claed0.f  cppsv.f   dbdsqr.f  dlaev2.f  dlatrs.f  dstein.f  sgelsx.f  slapmt.f  sormtr.f  stgexc.f  zgttrs.f  zlaqsb.f  ztbrfs.f
cgeql2.f  claed7.f  cppsvx.f  ddisna.f  dlaexc.f  dlatrz.f  dsteqr.f  sgelsy.f  slapy2.f  spbcon.f  stgsen.f  zgtts2.f  zlaqsp.f  ztbtrs.f
cgeqlf.f  claed8.f  cpptrf.f  dgbbrd.f  dlag2.f   dlatzm.f  dsterf.f  sgeql2.f  slapy3.f  spbequ.f  stgsja.f  zhbev.f   zlaqsy.f  ztgevc.f
cgeqp3.f  claein.f  cpptri.f  dgbcon.f  dlags2.f  dlauu2.f  dstev.f   sgeqlf.f  slaqgb.f  spbrfs.f  stgsna.f  zhbevd.f  zlar1v.f  ztgex2.f
cgeqpf.f  claesy.f  cpptrs.f  dgbequ.f  dlagtf.f  dlauum.f  dstevd.f  sgeqp3.f  slaqge.f  spbstf.f  stgsy2.f  zhbevx.f  zlar2v.f  ztgexc.f
cgeqr2.f  claev2.f  cptcon.f  dgbrfs.f  dlagtm.f  dopgtr.f  dstevr.f  sgeqpf.f  slaqp2.f  spbsv.f   stgsyl.f  zhbgst.f  zlarcm.f  ztgsen.f
cgeqrf.f  clags2.f  cpteqr.f  dgbsv.f   dlagts.f  dopmtr.f  dstevx.f  sgeqr2.f  slaqps.f  spbsvx.f  stpcon.f  zhbgv.f   zlarf.f   ztgsja.f
cgerfs.f  clagtm.f  cptrfs.f  dgbsvx.f  dlagv2.f  dorg2l.f  dsycon.f  sgeqrf.f  slaqsb.f  spbtf2.f  stprfs.f  zhbgvd.f  zlarfb.f  ztgsna.f
cgerq2.f  clahef.f  cptsv.f   dgbtf2.f  dlahqr.f  dorg2r.f  dsyev.f   sgerfs.f  slaqsp.f  spbtrf.f  stptri.f  zhbgvx.f  zlarfg.f  ztgsy2.f
cgerqf.f  clahqr.f  cptsvx.f  dgbtrf.f  dlahrd.f  dorgbr.f  dsyevd.f  sgerq2.f  slaqsy.f  spbtrs.f  stptrs.f  zhbtrd.f  zlarft.f  ztgsyl.f
cgesc2.f  clahrd.f  cpttrf.f  dgbtrs.f  dlaic1.f  dorghr.f  dsyevr.f  sgerqf.f  slaqtr.f  spocon.f  strcon.f  zhecon.f  zlarfx.f  ztpcon.f
cgesdd.f  claic1.f  cpttrs.f  dgebak.f  dlaln2.f  dorgl2.f  dsyevx.f  sgesc2.f  slar1v.f  spoequ.f  strevc.f  zheev.f   zlargv.f  ztprfs.f
cgesv.f   clals0.f  cptts2.f  dgebal.f  dlals0.f  dorglq.f  dsygs2.f  sgesdd.f  slar2v.f  sporfs.f  strexc.f  zheevd.f  zlarnv.f  ztptri.f
cgesvd.f  clalsa.f  crot.f    dgebd2.f  dlalsa.f  dorgql.f  dsygst.f  sgesv.f   slarf.f   sposv.f   strrfs.f  zheevr.f  zlarrv.f  ztptrs.f
cgesvx.f  clalsd.f  cspcon.f  dgebrd.f  dlalsd.f  dorgqr.f  dsygv.f   sgesvd.f  slarfb.f  sposvx.f  strsen.f  zheevx.f  zlartg.f  ztrcon.f
cgetc2.f  clangb.f  cspmv.f   dgecon.f  dlamch.f  dorgr2.f  dsygvd.f  sgesvx.f  slarfg.f  spotf2.f  strsna.f  zhegs2.f  zlartv.f  ztrevc.f
cgetf2.f  clange.f  cspr.f    dgeequ.f  dlamrg.f  dorgrq.f  dsygvx.f  sgetc2.f  slarft.f  spotrf.f  strsyl.f  zhegst.f  zlarz.f   ztrexc.f
cgetrf.f  clangt.f  csprfs.f  dgees.f   dlangb.f  dorgtr.f  dsyrfs.f  sgetf2.f  slarfx.f  spotri.f  strti2.f  zhegv.f   zlarzb.f  ztrrfs.f
cgetri.f  clanhb.f  cspsv.f   dgeesx.f  dlange.f  dorm2l.f  dsysv.f   sgetrf.f  slargv.f  spotrs.f  strtri.f  zhegvd.f  zlarzt.f  ztrsen.f
cgetrs.f  clanhe.f  cspsvx.f  dgeev.f   dlangt.f  dorm2r.f  dsysvx.f  sgetri.f  slarnv.f  sppcon.f  strtrs.f  zhegvx.f  zlascl.f  ztrsna.f
cggbak.f  clanhp.f  csptrf.f  dgeevx.f  dlanhs.f  dormbr.f  dsytd2.f  sgetrs.f  slarrb.f  sppequ.f  stzrqf.f  zherfs.f  zlaset.f  ztrsyl.f
cggbal.f  clanhs.f  csptri.f  dgegs.f   dlansb.f  dormhr.f  dsytf2.f  sggbak.f  slarre.f  spprfs.f  stzrzf.f  zhesv.f   zlasr.f   ztrti2.f
cgges.f   clanht.f  csptrs.f  dgegv.f   dlansp.f  dorml2.f  dsytrd.f  sggbal.f  slarrf.f  sppsv.f   xerbla.f  zhesvx.f  zlassq.f  ztrtri.f
cggesx.f  clansb.f  csrot.f   dgehd2.f  dlanst.f  dormlq.f  dsytrf.f  sgges.f   slarrv.f  sppsvx.f  zbdsqr.f  zhetd2.f  zlaswp.f  ztrtrs.f
cggev.f   clansp.f  csrscl.f  dgehrd.f  dlansy.f  dormql.f  dsytri.f  sggesx.f  slartg.f  spptrf.f  zdrot.f   zhetf2.f  zlasyf.f  ztzrqf.f
cggevx.f  clansy.f  cstedc.f  dgelq2.f  dlantb.f  dormqr.f  dsytrs.f  sggev.f   slartv.f  spptri.f  zdrscl.f  zhetrd.f  zlatbs.f  ztzrzf.f
cggglm.f  clantb.f  cstegr.f  dgelqf.f  dlantp.f  dormr2.f  dtbcon.f  sggevx.f  slaruv.f  spptrs.f  zgbbrd.f  zhetrf.f  zlatdf.f  zung2l.f
cgghrd.f  clantp.f  cstein.f  dgels.f   dlantr.f  dormr3.f  dtbrfs.f  sggglm.f  slarz.f   sptcon.f  zgbcon.f  zhetri.f  zlatps.f  zung2r.f
cgglse.f  clantr.f  csteqr.f  dgelsd.f  dlanv2.f  dormrq.f  dtbtrs.f  sgghrd.f  slarzb.f  spteqr.f  zgbequ.f  zhetrs.f  zlatrd.f  zungbr.f
cggqrf.f  clapll.f  csycon.f  dgelss.f  dlapll.f  dormrz.f  dtgevc.f  sgglse.f  slarzt.f  sptrfs.f  zgbrfs.f  zhgeqz.f  zlatrs.f  zunghr.f
cggrqf.f  clapmt.f  csymv.f   dgelsx.f  dlapmt.f  dormtr.f  dtgex2.f  sggqrf.f  slas2.f   sptsv.f   zgbsv.f   zhpcon.f  zlatrz.f  zungl2.f
cggsvd.f  claqgb.f  csyr.f    dgelsy.f  dlapy2.f  dpbcon.f  dtgexc.f  sggrqf.f  slascl.f  sptsvx.f  zgbsvx.f  zhpev.f   zlatzm.f  zunglq.f
cggsvp.f  claqge.f  csyrfs.f  dgeql2.f  dlapy3.f  dpbequ.f  dtgsen.f  sggsvd.f  slasd0.f  spttrf.f  zgbtf2.f  zhpevd.f  zlauu2.f  zungql.f
cgtcon.f  claqhb.f  csysv.f   dgeqlf.f  dlaqgb.f  dpbrfs.f  dtgsja.f  sggsvp.f  slasd1.f  spttrs.f  zgbtrf.f  zhpevx.f  zlauum.f  zungqr.f
cgtrfs.f  claqhe.f  csysvx.f  dgeqp3.f  dlaqge.f  dpbstf.f  dtgsna.f  sgtcon.f  slasd2.f  sptts2.f  zgbtrs.f  zhpgst.f  zpbcon.f  zungr2.f
cgtsv.f   claqhp.f  csytf2.f  dgeqpf.f  dlaqp2.f  dpbsv.f   dtgsy2.f  sgtrfs.f  slasd3.f  srscl.f   zgebak.f  zhpgv.f   zpbequ.f  zungrq.f
cgtsvx.f  claqp2.f  csytrf.f  dgeqr2.f  dlaqps.f  dpbsvx.f  dtgsyl.f  sgtsv.f   slasd4.f  ssbev.f   zgebal.f  zhpgvd.f  zpbrfs.f  zungtr.f
cgttrf.f  claqps.f  csytri.f  dgeqrf.f  dlaqsb.f  dpbtf2.f  dtpcon.f  sgtsvx.f  slasd5.f  ssbevd.f  zgebd2.f  zhpgvx.f  zpbstf.f  zunm2l.f
cgttrs.f  claqsb.f  csytrs.f  dgerfs.f  dlaqsp.f  dpbtrf.f  dtprfs.f  sgttrf.f  slasd6.f  ssbevx.f  zgebrd.f  zhprfs.f  zpbsv.f   zunm2r.f
cgtts2.f  claqsp.f  ctbcon.f  dgerq2.f  dlaqsy.f  dpbtrs.f  dtptri.f  sgttrs.f  slasd7.f  ssbgst.f  zgecon.f  zhpsv.f   zpbsvx.f  zunmbr.f
chbev.f   claqsy.f  ctbrfs.f  dgerqf.f  dlaqtr.f  dpocon.f  dtptrs.f  sgtts2.f  slasd8.f  ssbgv.f   zgeequ.f  zhpsvx.f  zpbtf2.f  zunmhr.f
chbevd.f  clar1v.f  ctbtrs.f  dgesc2.f  dlar1v.f  dpoequ.f  dtrcon.f  shgeqz.f  slasd9.f  ssbgvd.f  zgees.f   zhptrd.f  zpbtrf.f  zunml2.f
chbevx.f  clar2v.f  ctgevc.f  dgesdd.f  dlar2v.f  dporfs.f  dtrevc.f  shsein.f  slasda.f  ssbgvx.f  zgeesx.f  zhptrf.f  zpbtrs.f  zunmlq.f
chbgst.f  clarcm.f  ctgex2.f  dgesv.f   dlarf.f   dposv.f   dtrexc.f  shseqr.f  slasdq.f  ssbtrd.f  zgeev.f   zhptri.f  zpocon.f  zunmql.f
chbgv.f   clarf.f   ctgexc.f  dgesvd.f  dlarfb.f  dposvx.f  dtrrfs.f  slabad.f  slasdt.f  sspcon.f  zgeevx.f  zhptrs.f  zpoequ.f  zunmqr.f
chbgvd.f  clarfb.f  ctgsen.f  dgesvx.f  dlarfg.f  dpotf2.f  dtrsen.f  slabrd.f  slaset.f  sspev.f   zgegs.f   zhsein.f  zporfs.f  zunmr2.f
chbgvx.f  clarfg.f  ctgsja.f  dgetc2.f  dlarft.f  dpotrf.f  dtrsna.f  slacon.f  slasq1.f  sspevd.f  zgegv.f   zhseqr.f  zposv.f   zunmr3.f
chbtrd.f  clarft.f  ctgsna.f  dgetf2.f  dlarfx.f  dpotri.f  dtrsyl.f  slacpy.f  slasq2.f  sspevx.f  zgehd2.f  zlabrd.f  zposvx.f  zunmrq.f
checon.f  clarfx.f  ctgsy2.f  dgetrf.f  dlargv.f  dpotrs.f  dtrti2.f  sladiv.f  slasq3.f  sspgst.f  zgehrd.f  zlacgv.f  zpotf2.f  zunmrz.f
cheev.f   clargv.f  ctgsyl.f  dgetri.f  dlarnv.f  dppcon.f  dtrtri.f  slae2.f   slasq4.f  sspgv.f   zgelq2.f  zlacon.f  zpotrf.f  zunmtr.f
cheevd.f  clarnv.f  ctpcon.f  dgetrs.f  dlarrb.f  dppequ.f  dtrtrs.f  slaebz.f  slasq5.f  sspgvd.f  zgelqf.f  zlacp2.f  zpotri.f  zupgtr.f
cheevr.f  clarrv.f  ctprfs.f  dggbak.f  dlarre.f  dpprfs.f  dtzrqf.f  slaed0.f  slasq6.f  sspgvx.f  zgels.f   zlacpy.f  zpotrs.f  zupmtr.f
cheevx.f  clartg.f  ctptri.f  dggbal.f  dlarrf.f  dppsv.f   dtzrzf.f  slaed1.f  slasr.f   ssprfs.f  zgelsd.f  zlacrm.f  zppcon.f
chegs2.f  clartv.f  ctptrs.f  dgges.f   dlarrv.f  dppsvx.f  dzsum1.f  slaed2.f  slasrt.f  sspsv.f   zgelss.f  zlacrt.f  zppequ.f
chegst.f  clarz.f   ctrcon.f  dggesx.f  dlartg.f  dpptrf.f  icmax1.f  slaed3.f  slassq.f  sspsvx.f  zgelsx.f  zladiv.f  zpprfs.f
chegv.f   clarzb.f  ctrevc.f  dggev.f   dlartv.f  dpptri.f  ieeeck.f  slaed4.f  slasv2.f  ssptrd.f  zgelsy.f  zlaed0.f  zppsv.f
chegvd.f  clarzt.f  ctrexc.f  dggevx.f  dlaruv.f  dpptrs.f  ilaenv.f  slaed5.f  slaswp.f  ssptrf.f  zgeql2.f  zlaed7.f  zppsvx.f
chegvx.f  clascl.f  ctrrfs.f  dggglm.f  dlarz.f   dptcon.f  izmax1.f  slaed6.f  slasy2.f  ssptri.f  zgeqlf.f  zlaed8.f  zpptrf.f
cherfs.f  claset.f  ctrsen.f  dgghrd.f  dlarzb.f  dpteqr.f  lsame.f   slaed7.f  slasyf.f  ssptrs.f  zgeqp3.f  zlaein.f  zpptri.f
chesv.f   clasr.f   ctrsna.f  dgglse.f  dlarzt.f  dptrfs.f  lsamen.f  slaed8.f  slatbs.f  sstebz.f  zgeqpf.f  zlaesy.f  zpptrs.f
chesvx.f  classq.f  ctrsyl.f  dggqrf.f  dlas2.f   dptsv.f   sbdsdc.f  slaed9.f  slatdf.f  sstedc.f  zgeqr2.f  zlaev2.f  zptcon.f
chetd2.f  claswp.f  ctrti2.f  dggrqf.f  dlascl.f  dptsvx.f  sbdsqr.f  slaeda.f  slatps.f  sstegr.f  zgeqrf.f  zlags2.f  zpteqr.f
chetf2.f  clasyf.f  ctrtri.f  dggsvd.f  dlasd0.f  dpttrf.f  scsum1.f  slaein.f  slatrd.f  sstein.f  zgerfs.f  zlagtm.f  zptrfs.f
)

set_source_files_properties(slamch.f PROPERTIES COMPILE_FLAGS -O0)
set_source_files_properties(dlamch.f PROPERTIES COMPILE_FLAGS -O0)

add_library(lapack SHARED ${SOURCES})

