PROGRAM prog_30
    IMPLICIT NONE
    
    REAL :: a
    DOUBLE PRECISION :: b
    INTEGER :: c
    LOGICAL :: d
    CHARACTER :: e

    REAL(KIND=SELECTED_REAL_KIND(P=6, R=30)) :: f
    REAL(KIND=SELECTED_REAL_KIND(P=15, R=30)) :: g
    REAL(KIND=SELECTED_REAL_KIND(P=15, R=1000)) :: h
    REAL(KIND=SELECTED_REAL_KIND(P=18, R=1000)) :: i
    REAL(KIND=SELECTED_REAL_KIND(P=18, R=2000)) :: j
    REAL(KIND=SELECTED_REAL_KIND(P=18, R=4000)) :: k

    PRINT *, 'TYPE REAL                 KIND=', KIND(a)
    PRINT *, 'TYPE DOUBLE PRECISION     KIND=', KIND(b)
    PRINT *, 'TYPE INTEGER              KIND=', KIND(c)
    PRINT *, 'TYPE LOGICAL              KIND=', KIND(d)
    PRINT *, 'TYPE CHARACTER            KIND=', KIND(e)

    PRINT *, 'TYPE P=6, R=30        KIND=', KIND(f)
    PRINT *, 'TYPE P=15, R=30       KIND=', KIND(g)
    PRINT *, 'TYPE P=15, R=1000     KIND=', KIND(h)
    PRINT *, 'TYPE P=18, R=1000     KIND=', KIND(i)
    PRINT *, 'TYPE P=18, R=2000     KIND=', KIND(j)
    PRINT *, 'TYPE P=18, R=4000     KIND=', KIND(k)
STOP
END PROGRAM prog_30