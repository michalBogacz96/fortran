PROGRAM prog_28
    IMPLICIT NONE
    CHARACTER(LEN=6) :: a, b, c, d
    PRINT *, 'Type string 123456789'
    READ '(A8,T1,A4,T7, A6, T4, A)', a, b, c, d

    PRINT '(5X,A8,5X,A4,A,5X,A6)', a, b, c, d
    PRINT '(5X,A,5X,A,5X,A,5X,A1)', a, b, c, d
STOP
END PROGRAM prog_28
