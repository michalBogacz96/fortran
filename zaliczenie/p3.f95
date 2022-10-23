PROGRAM sum_recursive
  IMPLICIT NONE
  INTEGER, PARAMETER :: idp = selected_int_kind(10)
  INTEGER(KIND = idp) :: N1 = 2, N2 = 5, SUM = 0

  CALL recursive_sum(N1, N2, SUM)  
  PRINT "(A,1I0,A,1I0,A,1I0)", "Suma liczb naturalnych od ", N1, " do ", N2, " wynosi ", SUM
STOP

CONTAINS
  RECURSIVE SUBROUTINE recursive_sum(N1, N2, SUM)
    IMPLICIT NONE
    INTEGER(KIND = idp), INTENT(IN) :: N1, N2
    INTEGER(KIND = idp), INTENT(OUT) :: SUM
    INTEGER(KIND = idp) :: TEMP_SUM = 0
    
    IF (N2 == N1) THEN
      SUM = N1
    ELSE
      CALL recursive_sum(N1, N2-1, TEMP_SUM)
      SUM = N2 + TEMP_SUM
    ENDIF
  END SUBROUTINE
 
END PROGRAM sum_recursive
