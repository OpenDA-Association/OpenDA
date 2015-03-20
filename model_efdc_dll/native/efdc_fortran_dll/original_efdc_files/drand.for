      function drand(n)
      if (n == 1) then
        drand=0.5
        CALL RANDOM_SEED
      elseif (n == 0) then
        CALL RANDOM_NUMBER(drand)
      else
        write(6,*) 'CALL DRAND w/o SEED of 0 or 1 ==> STOP'
        stop 99
      endif
      return
      end
