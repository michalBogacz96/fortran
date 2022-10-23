PROGRAM zero_function
  IMPLICIT NONE
  INTEGER :: i = 0
  REAL :: x1 = -3.0, x2 = 4.0, step = 0.001, res = 0.0, currX
  REAL :: leftV, rightV

  currX = x1

  PRINT "(A, 1F0.2, A, 1F0.2, A)", "Funkcja f(x) = x^3 - 3x^2 - 4x + 12 w zakresie od ", x1, " do ", x2, " ma miejsca zerowe w:"
  DO WHILE (currX < x2)
    leftV = calcFunction(currX)
    rightV = calcFunction(currX + step)
    IF (isDiffSign(leftV, rightV)) THEN
      PRINT "(A,1I0,A,1F0.2)", "x", i, " = ", currX
      i = i + 1
    END IF

    currX = currX + step
  END DO
  STOP


  CONTAINS
  FUNCTION calcFunction(x) RESULT(y)
    REAL :: y
    REAL, INTENT(IN) :: x

    y = (x**3) - (3 * x**2) - (4 * x) + 12
  END FUNCTION

  FUNCTION isDiffSign(v1, v2) RESULT(diffSign)
    REAL, INTENT(IN) :: v1, v2
    LOGICAL diffSign

    IF ((v1 < 0 .and. v2 > 0) .or. (v1 > 0 .and. v2 < 0)) THEN
      diffSign = .TRUE.
    ELSE
      diffSign = .FALSE.
    END IF
  END FUNCTION

END PROGRAM
