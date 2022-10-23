PROGRAM monte_carlo
  IMPLICIT NONE

  INTEGER, PARAMETER :: rdp = SELECTED_REAL_KIND(15)
  REAL(KIND = rdp), PARAMETER :: pi = 3.14159265358979326433824338327950288_rdp
  
  INTEGER :: i, maxPoints = 1000000, counter = 0
  REAL(KIND = rdp) :: xMin = 0.0, xMax, yMin = 0.0, yMax = 1.0
  REAL(KIND = rdp) :: x, y, sinValue, result
  xMax = pi

  CALL init_random_seed()

  DO i = 1, maxPoints
    x = randomFromRange(xMin, xMax)
    y = randomFromRange(yMin, yMax)
    sinValue = sinRdp(x)
    IF (y < sinValue) THEN
      counter = counter + 1
    END IF
  END DO

  result = (xMax - xMin) * (yMax - yMin) * (REAL(counter) / REAL(maxPoints))
  PRINT "(A,1F0.5,A,1F0.5,A,1F0.5)", "Calka oznaczona w zakresie od ", xMin, " do ", xMax, " z funkcji sin(x) wynosi ", result
CONTAINS
  FUNCTION sinRdp(x) RESULT(y)
    REAL(KIND = rdp), INTENT(IN) :: x
    REAL(KIND = rdp) :: y
    y = SIN(x)
  END FUNCTION

  FUNCTION randomFromRange(rangeMin, rangeMax) RESULT(number)
    REAL(KIND = RDP) :: number
    REAL(KIND = RDP), INTENT(IN) :: rangeMin, rangeMax
    CALL random_number(number)
    number = number * (rangeMax - rangeMin) + rangeMin
  END FUNCTION

  SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    CALL RANDOM_SEED(SIZE = n)
    ALLOCATE(seed(n))
    CALL SYSTEM_CLOCK(COUNT = clock)
    seed = clock + 37 * (/(i - 1, i = 1, n)/)
    CALL RANDOM_SEED(PUT = seed)
    DEALLOCATE(seed)
  END SUBROUTINE
END PROGRAM
