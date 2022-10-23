MODULE ProgramUtils
    IMPLICIT NONE
    PUBLIC :: initProgram, resetProgram, baduserInput
    CHARACTER (256) :: USER_APP_PATH, USER_TASKS_PATH

    CONTAINS
    SUBROUTINE initProgram()
      CHARACTER (256) :: command
      CHARACTER (256) :: user
      CALL GETENV("USER", user)
      USER_APP_PATH = "/home/" // trim(user)

      command = "mkdir " // trim(USER_APP_PATH) // "/.time_tracker/tasks -p"
      CALL SYSTEM(command)

      USER_APP_PATH = trim(USER_APP_PATH) // "/.time_tracker"
      USER_TASKS_PATH = trim(USER_APP_PATH) // "/tasks"
    END SUBROUTINE

    SUBROUTINE resetProgram()
      CHARACTER (256) :: command

      command = "rm -rf " // trim(USER_APP_PATH) // "/tasks"
      CALL SYSTEM(command)

      command = "mkdir " // trim(USER_APP_PATH) // "/tasks -p"
      CALL SYSTEM(command)
    END SUBROUTINE

    SUBROUTINE clear()
      CHARACTER (256) :: command
      command = "rm -rf ./.sys_call_temp"
      CALL SYSTEM(command)
    END SUBROUTINE

    SUBROUTINE badUserInput()
      PRINT *, "Nie rozpoznano polecenia"
    END SUBROUTINE

END MODULE ProgramUtils

MODULE TaskModule
  USE ProgramUtils
  IMPLICIT NONE

  PUBLIC :: addTask, deleteTask, editTaskName, editTaskDescription, listTasks

  CONTAINS
  SUBROUTINE addTask(name)
    IMPLICIT NONE
    CHARACTER (*), INTENT(IN) :: name
    CHARACTER (256) :: taskPath, command

    IF (taskExist(name)) THEN
      PRINT *, "Zadanie o tej nazwie juz istnieje"
      RETURN
    END IF
    
    taskPath = trim(USER_TASKS_PATH) // "/" // trim(name)
    command = "mkdir " // trim(taskPath) // " -p"
    CALL SYSTEM(command)
    command = "touch " // trim(taskPath) // "/description"
    CALL SYSTEM(command)
    command = "echo > " // trim(taskPath) // "/description"
    CALL SYSTEM(command)
    command = "touch " // trim(taskPath) // "/tracking"
    CALL SYSTEM(command)
    PRINT "(A, A)", "Dodano nowe zadanie ", name
  END SUBROUTINE

  SUBROUTINE deleteTask(name)
    IMPLICIT NONE
    CHARACTER (*), INTENT(IN) :: name
    CHARACTER(256) :: command

    IF (taskExist(name)) THEN
      command =  "rm -rf " // trim(USER_TASKS_PATH) // "/" // trim(name)
      CALL SYSTEM(command)
      PRINT "(A, A)", "Usunieto zadanie o nazwie ", name
      RETURN
    END IF
    PRINT "(A,A)", "Nie znaleziono zadania o nazwie ", name

  END SUBROUTINE

  SUBROUTINE editTaskName(name, newName)
    IMPLICIT NONE
    CHARACTER (*), INTENT(IN) :: name, newName
    CHARACTER (256) :: taskPath, newTaskPath, command
    IF (taskExist(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie o tej nazwie nie istnieje"
      RETURN
    END IF

    taskPath = trim(USER_TASKS_PATH) // "/" // trim(name)
    newTaskPath = trim(USER_TASKS_PATH) // "/" // trim(newName)
    command = "mv " // trim(taskPath) // " " // trim(newTaskPath)
    CALL SYSTEM(command)
    PRINT "(A, A, A, A)", "Nazwa zadania ", trim(name), " zostala zmieniona na ", trim(newName)
  END SUBROUTINE

  SUBROUTINE editTaskDescription(name, newDescription)
    IMPLICIT NONE
    
    CHARACTER (256) :: taskDescPath, command
    CHARACTER (*), INTENT(IN) :: name, newDescription
    IF (taskExist(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie o tej nazwie nie istnieje"
      RETURN
    END IF

    taskDescPath = trim(USER_TASKS_PATH) // "/" // trim(name) // "/description"
    OPEN(1, FILE=trim(taskDescPath), STATUS="OLD", ACTION="WRITE", POSITION="REWIND")
    WRITE(1, "(A)") trim(newDescription)
    CLOSE(1)
    PRINT "(A, A, A)", "Opis dla zadania ", trim(name), " zostal zmieniony."
  END SUBROUTINE

  FUNCTION getTasksSize() RESULT(size)
    INTEGER :: size
    CHARACTER(256) :: command
    command = "ls " // trim(USER_TASKS_PATH) // " | wc -l > .sys_call_temp"
    CALL SYSTEM(command)
    OPEN(UNIT = 1, FILE = ".sys_call_temp", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
    READ(1, *) size
    CLOSE(1)
  END FUNCTION

  FUNCTION getTasks() RESULT(tasks)
    IMPLICIT NONE
    CHARACTER(256) :: tasks(100), command, current
    INTEGER :: i, taskSize

    taskSize = getTasksSize()

    command = "ls " // trim(USER_TASKS_PATH) // " > .sys_call_temp"
    CALL SYSTEM(command)

    OPEN(UNIT = 1, FILE = ".sys_call_temp", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
    DO i=1, taskSize
      READ(UNIT = 1, FMT = *) current
      tasks(i) = trim(current)
    END DO
    CLOSE(1)
  END FUNCTION

  FUNCTION taskExist(name) RESULT(exist)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: name
    LOGICAL :: exist
    CHARACTER(256) :: tasks(100)
    INTEGER :: taskSize, j

    taskSize = getTasksSize()
    tasks = getTasks()
    DO j = 1, taskSize
      IF (trim(tasks(j)) == trim(name)) THEN
        exist = .TRUE.
        RETURN
      END IF
    END DO
    exist = .FALSE.
    RETURN
  END FUNCTION

  SUBROUTINE listTasks()
    IMPLICIT NONE
    CHARACTER(256) :: tasks(100), taskDescPath, desc
    INTEGER :: taskSize, j

    taskSize = getTasksSize()
    tasks = getTasks()
    PRINT *, "Lista zadan:"
    DO j = 1, taskSize
      taskDescPath = trim(USER_TASKS_PATH) // "/" // trim(tasks(j)) // "/description"
      OPEN(1, FILE=trim(taskDescPath), STATUS="OLD", ACTION="READ", POSITION="REWIND")
      READ(1, "(A)") desc
      CLOSE(1)
      PRINT "(A, A, A, A)", "Nazwa: ", trim(tasks(j)), ", opis: ", trim(desc)
    END DO
  END SUBROUTINE

END MODULE TaskModule


MODULE TimeTrackingModule
  USE ProgramUtils
  USE TaskModule
  IMPLICIT NONE

  PUBLIC :: startTrack, endTrack, getTaskSummary, getAllTasksSummary
  
  CONTAINS
  SUBROUTINE startTrack(name)
    INTEGER :: epochNow    
    CHARACTER (*), INTENT(IN) :: name
    CHARACTER (256) :: taskTrackingPath, command

    IF (taskExist(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie o tej nazwie nie istnieje"
      RETURN
    END IF

 
    IF(isActive(name)) THEN
      PRINT *, "Zadanie jest juz w trakcie pomiaru!"
      RETURN
    END IF

    taskTrackingPath = trim(USER_TASKS_PATH) // "/" // trim(name) // "/tracking"

    OPEN(1, FILE=trim(taskTrackingPath), STATUS="OLD", ACTION="WRITE", POSITION="APPEND")
    epochNow = time()
    WRITE(1, "(1I0)", ADVANCE="no") epochNow ! advance="no" not working :(
    command = "touch " // trim(USER_TASKS_PATH) // "/" // trim(name) // "/.active"
    CALL SYSTEM(command)
    CLOSE(1)
  END SUBROUTINE
  
  SUBROUTINE endTrack(name)
    INTEGER :: epochNow    
    CHARACTER (*), INTENT(IN) :: name
    CHARACTER (256) :: taskTrackingPath, command

    IF (taskExist(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie o tej nazwie nie istnieje"
      RETURN
    END IF

 
    IF(isActive(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie czeka na start pomiaru."
      RETURN
    END IF

    taskTrackingPath = trim(USER_TASKS_PATH) // "/" // trim(name) // "/tracking"
    command = "truncate -s -1 " // trim(taskTrackingPath) ! delete newline character
    CALL SYSTEM(command)
    OPEN(1, FILE=trim(taskTrackingPath), STATUS="OLD", ACTION="WRITE", POSITION="APPEND")
    epochNow = time()
    WRITE(1, "(A, 1I0)") " ", epochNow
    command = "rm -rf " // trim(USER_TASKS_PATH) // "/" // trim(name) // "/.active"
    CALL SYSTEM(command)
    CLOSE(1)
  END SUBROUTINE
  
  SUBROUTINE getTaskSummary(name)
    CHARACTER (*), INTENT(IN) :: name
    INTEGER :: EoF, startTime, endTime, diffTime, sumTime
    CHARACTER(30) :: startDateTime, endDateTime
    CHARACTER (256) :: taskTrackingPath

    IF (taskExist(name).EQV..FALSE.) THEN
      PRINT *, "Zadanie o tej nazwie nie istnieje"
      RETURN
    END IF

 
    IF(isActive(name)) THEN
      PRINT *, "Zadanie jest w trakcie pomiaru!"
      RETURN
    END IF

    sumTime = 0
    taskTrackingPath = trim(USER_TASKS_PATH) // "/" // trim(name) // "/tracking"
    OPEN(1, FILE=trim(taskTrackingPath), STATUS="OLD", ACTION="READ", POSITION="REWIND")
    DO
      READ(1, "(1I11,1I11)", IOSTAT=EoF) startTime, endTime
      IF(EoF/=0) EXIT
      diffTime = endTime - startTime
      sumTime = sumTime + diffTime
      CALL CTIME(startTime, startDateTime)
      CALL CTIME(endTime, endDateTime)
      WRITE(*,"(A, A, A, A)", ADVANCE="no") trim(startDateTime), " - ", trim(endDateTime), " | "
      CALL printTimeSummary(diffTime)
    END DO

    WRITE(*, "(/A)", ADVANCE="no") "Lacznie spedzony czas nad zadaniem: "
    CALL printTimeSummary(sumTime)
    CLOSE(1)

  END SUBROUTINE

  SUBROUTINE printTimeSummary(timeS)
    INTEGER, INTENT(IN) :: timeS
    INTEGER :: sec = 0, min = 0, hours = 0
    IF (timeS < 60) THEN
      sec = timeS
      PRINT "(1I0, A)", sec, " sek"
    ELSE IF (timeS < 3600) THEN
      min = timeS / 60
      PRINT "(1I0, A, 1I0, A)", min, " min, ", sec, " sek"
    ELSE
      hours = timeS / 3600
      min = (timeS - (hours * 3600)) / 60
      sec = timeS - (hours * 3600) - (min * 60)
      PRINT "(1I0, A, 1I0, A, 1I0, A)", hours, " godz, ", min, " min, ", sec, " sek"
    END IF
  END SUBROUTINE

  
  SUBROUTINE getAllTasksSummary()
    IMPLICIT NONE
    CHARACTER(256) :: tasks(100), taskDescPath, desc
    INTEGER :: taskSize, j

    taskSize = getTasksSize()
    tasks = getTasks()
    PRINT *, "Lista zadan:"
    DO j = 1, taskSize
      taskDescPath = trim(USER_TASKS_PATH) // "/" // trim(tasks(j)) // "/description"
      OPEN(1, FILE=trim(taskDescPath), STATUS="OLD", ACTION="READ", POSITION="REWIND")
      READ(1, "(A)") desc
      CLOSE(1)
      PRINT "(A, A, A, A)", "Nazwa: ", trim(tasks(j)), ", opis: ", trim(desc)
      PRINT "(/A)", "Time tracking:"
      CALL getTaskSummary(trim(tasks(j)))
      PRINT "(A)", "------------------------------------------------------"
    END DO
  END SUBROUTINE

  FUNCTION isActive(name) RESULT(active)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(256) :: activeFilePath
    LOGICAL :: active

    activeFilePath = trim(USER_TASKS_PATH) // "/" // trim(name) // "/.active"
    INQUIRE(FILE = activeFilePath, EXIST=active)
    RETURN

  END FUNCTION
END MODULE TimeTrackingModule


PROGRAM TimeTracker
  USE ProgramUtils
  USE TaskModule
  USE TimeTrackingModule

  IMPLICIT NONE
  INTEGER :: userInput
  
  CALL initProgram()
  
  PRINT *, "Menu: "
  PRINT *, "(1) Zadania"
  PRINT *, "(2) Time Tracking"
  PRINT *, "(3) Resetuj dane"
  READ *, userInput

  SELECT CASE (userInput)
    CASE (1)
      CALL TaskMenu()
    CASE (2)
      CALL TimeTrackingMenu()
    CASE (3)
      CALL resetProgram()
    CASE DEFAULT
      CALL baduserInput()
  END SELECT

  CALL clear()

  CONTAINS
  SUBROUTINE TaskMenu()
    CHARACTER(256) :: taskInput, taskInput2
    INTEGER :: userInput
    PRINT *, "(1) Dodaj zadanie"
    PRINT *, "(2) Usun zadanie"
    PRINT *, "(3) Dodaj opis do zadania"
    PRINT *, "(4) Zmien nazwe zadania"
    PRINT *, "(5) Pokaz wszystkie zadania"
    READ *, userInput

    SELECT CASE (userInput)
      CASE (1)
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        CALL addTask(trim(taskInput))
      CASE (2) 
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        CALL deleteTask(trim(taskInput))
      CASE (3)
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        PRINT *, "Wpisz opis: "
        READ(*, "(A)") taskInput2
        CALL editTaskDescription(trim(taskInput), trim(taskInput2))
      CASE (4)
        PRINT *, "Wpisz nazwe zadania: "  
        READ *, taskInput
        PRINT *, "Wpisz nowa nazwe: "
        READ(*, "(A)") taskInput2
        CALL editTaskName(trim(taskInput), trim(taskInput2))
      CASE (5)
        CALL listTasks()
      CASE DEFAULT
        CALL badUserInput()
    END SELECT

  END SUBROUTINE


  SUBROUTINE TimeTrackingMenu()
    CHARACTER(256) :: taskInput, taskInput2
    INTEGER :: userInput
    PRINT *, "(1) Zacznij mierzyc"
    PRINT *, "(2) Zakoncz mierzyc"
    PRINT *, "(3) Szczegoly zadania"
    PRINT *, "(4) Szczegoly wszystkich zadan"
    READ *, userInput
    SELECT CASE (userInput)
      CASE (1)
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        CALL startTrack(trim(taskInput))
      CASE (2)
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        CALL endTrack(trim(taskInput))
      CASE (3)
        PRINT *, "Wpisz nazwe zadania: "
        READ *, taskInput
        CALL getTaskSummary(trim(taskInput))
      CASE (4)
        CALL getAllTasksSummary()
      CASE DEFAULT
        CALL badUserInput()
    END SELECT


  END SUBROUTINE
END PROGRAM TimeTracker
