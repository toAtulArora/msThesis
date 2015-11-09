module classContVar
  !for interpolating
  use spline_fortran
  implicit none
  

  !private

  public :: contVar, contVarInit, contVarInterp, contVarAllocate

  type contVar
     !the data points f(x)
     complex, allocatable :: f(:)
     !the corresponding (x)
     real, allocatable :: x(:)
     !the computed spline parameters
     complex, allocatable :: b(:),c(:),d(:)
  end type contVar

contains
  subroutine contVarAllocate(this,Ndefined)
    type(contVar):: this
    integer :: Ndefined,N
    N=Ndefined
    allocate(this%f(N))
    this%f = 0
    allocate(this%x(N))
    this%x = 0
    allocate(this%b(N))
    this%b =0
    allocate(this%c(N))
    this%c = 0
    allocate(this%d(N))
    this%d =0
  end subroutine contVarAllocate
  
  subroutine contVarInit(this)
    type(contVar) :: this
    !These are for holding the spline parameters temporarily
    real, dimension(size(this%x)) :: rb,rc,rd,ib,ic,id
    call spline(this%x,real(this%f),rb,rc,rd,size(this%x))
    call spline(this%x,real((0,-1)*this%f),ib,ic,id,size(this%x)) !this funky thing is just to send the

    !combine the cofficients to a complex # array
    this%b=rb + (0,1)*ib
    this%c=rc + (0,1)*ic
    this%d=rd + (0,1)*id
  end subroutine contVarInit

  function contVarInterp(this,q)
    type(contVar),intent(in) :: this
    !the value at which you want the value of f
    real :: q
    real :: contVarInterp
    contVarInterp=this%f(int((q + 10)/(0.1))) !ispline(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(this%x)) + (0,1)*ispline(q,this%x,real((0,-1)*this%f),real((0,-1)*this%b),real((0,-1)*this%c),real((0,-1)*this%d),size(this%x))
  end function contVarInterp

end module classContVar
