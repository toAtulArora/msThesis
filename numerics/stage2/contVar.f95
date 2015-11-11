module classContVar
  !for interpolating
  use spline_fortran
  implicit none
  

  !private

  !public :: contVar, contVarInit, contVarInterp, contVarAllocate

  type contVar
     !the data points f(x)
     complex, public, allocatable :: f(:)
     !the corresponding (x)
     real, public,  allocatable :: x(:)
     !the computed spline parameters
     complex, public, allocatable :: b(:),c(:),d(:)
   contains
     procedure, pass :: contVarAllocate
     procedure, pass :: contVarInit
     procedure, pass :: contVarInterp
     procedure, pass :: contVarDel2
  end type contVar

contains
  subroutine contVarAllocate(this,Ndefined)
    class(contVar),intent(inout):: this
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
    class(contVar),intent(inout) :: this
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
    class(contVar),intent(inout) :: this
    !the value at which you want the value of f
    real :: q
    !integer :: qStep
    complex :: contVarInterp
    !contVarInterp=this%f(qStep)
    !contVarInterp=this%f(int((q + 10)/(0.1))) !
    contVarInterp=ispline(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(this%x)) + (0,1)*ispline(q,this%x,real((0,-1)*this%f),real((0,-1)*this%b),real((0,-1)*this%c),real((0,-1)*this%d),size(this%x))
  end function contVarInterp

  function contVarDel2(this,q)
    class(contVar),intent(inout) :: this
    !the value at which you want the value of f
    real :: q
    !integer :: qStep
    complex :: contVarDel2
    !contVarDel2=this%f(qStep)
    !contVarDel2=this%f(int((q + 10)/(0.1))) !
    contVarDel2=iDel2(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(this%x)) + (0,1)*iDel2(q,this%x,real((0,-1)*this%f),real((0,-1)*this%b),real((0,-1)*this%c),real((0,-1)*this%d),size(this%x))
  end function contVarDel2

  function contVarDel(this,q)
    class(contVar),intent(inout) :: this
    !the value at which you want the value of f
    real :: q
    !integer :: qStep
    complex :: contVarDel
    !contVarDel=this%f(qStep)
    !contVarDel=this%f(int((q + 10)/(0.1))) !
    contVarDel=iDel(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(this%x)) + (0,1)*iDel(q,this%x,real((0,-1)*this%f),real((0,-1)*this%b),real((0,-1)*this%c),real((0,-1)*this%d),size(this%x))
  end function contVarDel


end module classContVar
