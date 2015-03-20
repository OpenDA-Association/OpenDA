lib_LTLIBRARIES = libsqlite3.la
libsqlite3_la_SOURCES =  \
alter.c     delete.c           legacy.c   parse.c      tokenize.c  \
analyze.c   expr.c             loadext.c  pragma.c     trigger.c   \
attach.c    fts1.c             main.c     prepare.c    update.c    \
auth.c      fts1_hash.c        opcodes.c  printf.c     utf.c       \
btree.c     fts1_porter.c      os.c       random.c     util.c      \
build.c     fts1_tokenizer1.c  os_os2.c   select.c     vacuum.c    \
callback.c  func.c             os_unix.c  vdbe.c      \
complete.c  hash.c             os_win.c   table.c      vdbeapi.c   \
date.c      insert.c           pager.c    tclsqlite.c  vdbeaux.c   \
vdbefifo.c  vdbemem.c          vtab.c     where.c
#shell.c      
libsqlite3_la_LIBADD = @LTLIBOBJS@
libsqlite3_la_LDFLAGS = -no-undefined -version-info 0:0:0
AM_CPPFLAGS=-DNO_TCL

