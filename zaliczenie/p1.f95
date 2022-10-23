PROGRAM sum_odd_even
  IMPLICIT NONE
  INTEGER :: i, current, sum_odd = 0, sum_even = 0
  INTEGER, PARAMETER :: numberCount = 15
  
  OPEN(UNIT = 1, FILE = "liczby.txt", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
  DO i=1, numberCount
    READ(UNIT = 1, FMT = *) current
    IF (mod(current, 2) == 0) THEN
      sum_even = sum_even + current
    ELSE
      sum_odd = sum_odd + current
    END IF
  END DO
  CLOSE(1)

  PRINT "(A,1I0)", "Suma nieparzystych: ", sum_odd
  PRINT "(A,1I0)", "Suma parzystych: ", sum_even
  
  STOP
END PROGRAM
