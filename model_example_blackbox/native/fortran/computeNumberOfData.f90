subroutine computeNumberOfData(line,nData)

  implicit none

  character(len=*) line
  character(len=len(line)) cLine
  integer nData
  integer indComma

  cLine=line
  nData=0
  indComma=1
  do while ( indComma.ne.0 )
    indComma=index(cline,',')
    nData=nData+1
    cLine=cLine(indComma+1:)
  end do
  
end subroutine
