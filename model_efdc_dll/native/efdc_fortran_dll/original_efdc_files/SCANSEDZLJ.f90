SUBROUTINE SCANSEDZLJ  
!
!  REVISION DATE :  May 24, 2006
!  Craig Jones and Scott James
!***************************************************************
	USE GLOBAL
    USE MPI
	IMPLICIT NONE
	INTEGER::IDUMMY,ERROR
!
    IF(MYRANK.EQ.0)WRITE(*,'(A)')'SCANNING INPUT FILE: BED.SDF'  
	OPEN(1,FILE='BED.SDF',STATUS='OLD')  
	READ(1,*,IOSTAT=ERROR) !SKIP THIS LINE
	IF(ERROR==1)THEN
        IF(MYRANK.EQ.0)WRITE(*,'("READ ERROR IN SEDZLJ INPUT FILE")')  
        IF(MYRANK.EQ.0)WRITE(8,'("READ ERROR IN SEDZLJ INPUT FILE")')  
		STOP
	ENDIF
	READ(1,*,IOSTAT=ERROR) IDUMMY,IDUMMY,IDUMMY,KB
	IF(ERROR==1)THEN
        IF(MYRANK.EQ.0)WRITE(*,'("READ ERROR IN SEDZLJ INPUT FILE")')  
        IF(MYRANK.EQ.0)WRITE(8,'("READ ERROR IN SEDZLJ INPUT FILE")')  
		STOP
	ENDIF
	READ(1,*,IOSTAT=ERROR) !SKIP THIS LINE
	IF(ERROR==1)THEN
        IF(MYRANK.EQ.0)WRITE(*,'("READ ERROR IN SEDZLJ INPUT FILE")')  
        IF(MYRANK.EQ.0)WRITE(8,'("READ ERROR IN SEDZLJ INPUT FILE")')  
		STOP
	ENDIF
	READ(1,*,IOSTAT=ERROR) ITBM,NSICM 
	IF(ERROR==1)THEN
        IF(MYRANK.EQ.0)WRITE(*,'("READ ERROR IN SEDZLJ INPUT FILE")')  
        IF(MYRANK.EQ.0)WRITE(8,'("READ ERROR IN SEDZLJ INPUT FILE")')  
		STOP
	ENDIF
	CLOSE(1)  
	RETURN
END SUBROUTINE SCANSEDZLJ

