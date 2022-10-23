PROGRAM investment
  IMPLICIT NONE
  INTEGER :: i, years = 30
  REAL :: amount = 1000.0, interest = 0.02

  DO i=1, years
    amount = amount * (1 + interest)
  END DO
  PRINT "(A,1F0.2,A)", "Kwota po 30 latach: ", amount, " zl"
  STOP
END PROGRAM
