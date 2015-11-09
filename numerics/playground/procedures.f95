MODULE mytype

  IMPLICIT NONE

  TYPE obj
     INTEGER :: var
     complex, allocatable :: gla (:)
   CONTAINS
     PROCEDURE, PASS :: get_var
     PROCEDURE, PASS :: set_var
     PROCEDURE, PASS :: init_var
     PROCEDURE, PASS :: get_gla     
  END TYPE obj

CONTAINS

  SUBROUTINE set_var(this,an_int)
    class(obj), INTENT(inout) :: this
    INTEGER, INTENT(in) :: an_int
    this%var = an_int
  END SUBROUTINE set_var

  INTEGER FUNCTION get_var(this)
    CLASS(obj), INTENT(inout) :: this
    get_var = this%var
  END FUNCTION get_var

  subroutine init_var(this,n)
    class(obj) :: this
    integer :: n
    allocate(this%gla(n))
    this%gla=0
  end subroutine init_var

  function get_gla(this,j)
    class(obj):: this
    integer :: j
    real:: get_gla
    get_gla=this%gla(j)
  end function get_gla
END MODULE mytype


PROGRAM test

  USE mytype
  IMPLICIT NONE 

  integer :: j
  complex :: debugArray(5)
  TYPE (obj) :: myObj

  call myObj%init_var(5)

  
  myObj%gla=(/ (j*0.1,j=1,5) /)
  write(*,*) "What I set", myObj%gla
  debugArray = (/ (myObj%get_gla(j),j=1,5) /)
  write(*,*) "What I get", debugArray

  myObj%gla=(/ (j*0.5,j=1,5) /)
  write(*,*) "What I set", myObj%gla
  debugArray = (/ (myObj%get_gla(j),j=1,5) /)
  write(*,*) "What I get", debugArray
  
  myObj%gla=(/ (j*1.5,j=1,5) /)
  write(*,*) "What I set", myObj%gla
  debugArray = (/ (myObj%get_gla(j),j=1,5) /)
  write(*,*) "What I get", debugArray

  myObj%gla=(/ (j*2.0,j=1,5) /)
  write(*,*) "What I set", myObj%gla
  debugArray = (/ (myObj%get_gla(j),j=1,5) /)
  write(*,*) "What I get", debugArray


  myObj%var=10
  write(*,*) "What I set", myObj%var
  write(*,*) "what I get", myObj%get_var()

  myObj%var=15
  write(*,*) "What I set", myObj%var
  write(*,*) "what I get", myObj%get_var()

  myObj%var=20
  write(*,*) "What I set", myObj%var
  write(*,*) "what I get", myObj%get_var()
  
  CALL myobj%set_var(12)
  PRINT*, myObj%get_var()

END PROGRAM test
