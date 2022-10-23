MODULE zawiera_funkcje_Large
    IMPLICIT NONE
    PUBLIC :: Large

    CONTAINS
    FUNCTION Large(list, k) result(large_R)
        INTEGER, INTENT(IN), DIMENSION(:) :: list
        INTEGER, INTENT(IN) :: k
        INTEGER :: large_R

        IF(ANY(list <= k)) THEN
            large_R = MAXVAL(list, MASK=(list <= k))
        ELSE
            large_R = k
        END IF
        RETURN
    END FUNCTION Large
END MODULE zawiera_funkcje_Large

PROGRAM prog_37
    USE zawiera_funkcje_Large
    IMPLICIT NONE
    INTEGER :: n, k
    INTEGER, ALLOCATABLE, DIMENSION(:) :: list

    DO
        PRINT *, 'podaj rozmiar macierzy "list" '
        READ(UNIT=*, FMT=*) n
        IF (n <= 0) THEN
            EXIT
        END IF
        ALLOCATE(list(n))
        PRINT *, "podaj wartosci n-elementowej macierzy oddzielajac &"
        READ(UNIT=*, FMT=*) list
        PRINT *, "podaj liczbe 'k' "
        READ (UNIT=*, FMT=*) k
        PRINT *, "najwiekszy element macierzy ""list"" sposrod mniejszych od liczby ""k"" "
        WRITE(UNIT=*, FMT=*) Large(list, k)
        DEALLOCATE(list)
    END DO
STOP
END PROGRAM prog_37
