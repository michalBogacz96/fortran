MODULE word_utils
  IMPLICIT NONE
  PUBLIC :: isVowel
  
  CONTAINS
  FUNCTION toUpperCase(strIn) RESULT(strOut)
    CHARACTER(LEN=*), INTENT(IN) :: strIn
    CHARACTER(LEN=LEN(strIn)) :: strOut
    INTEGER :: i,j

    DO i = 1, LEN(strIn)
         j = IACHAR(strIn(i:i))
         IF (j>= IACHAR("a") .and. j<=IACHAR("z") ) THEN
              strOut(i:i) = ACHAR(IACHAR(strIn(i:i))-32)
         ELSE
              strOut(i:i) = strIn(i:i)
         END IF
    END DO

  END FUNCTION

  FUNCTION isVowel(c) RESULT(res)
    CHARACTER(1), INTENT(IN) :: c
    CHARACTER(1) upperC
    LOGICAL res

    upperC = toUpperCase(c)

    SELECT CASE (upperC)
      CASE ('A','E','I','O','U')
        res = .TRUE.
      CASE DEFAULT
        res = .FALSE.
    END SELECT
  END FUNCTION
END MODULE


PROGRAM vowel_counter
  USE WORD_UTILS
  IMPLICIT NONE

  CHARACTER(99) word
  INTEGER :: i, wordLength, vowelCounter = 0

  OPEN(1, FILE='slowo.txt')
  READ(1, *) word
  wordLength = LEN_TRIM(word)

  DO i=1,wordLength
    IF (isVowel(word(i:i))) THEN
      vowelCounter = vowelCounter + 1
    END IF
  END DO

  PRINT "(A,1I0,A,1I0,A)", "Wczytane slowo ma ", wordLength, " liter w tym ", vowelCounter, " samoglosek"
  CLOSE(1)
END PROGRAM
