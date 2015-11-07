module classContVar
  implicit none
  !for interpolating
  use spline
  
  private

  public :: contVar, contVarInit, contVarInterp

  type contVar
     !the data points f(x)
     complex, allocable :: f(:)
     !the corresponding (x)
     real, allocable :: x(:)
     !the computed spline parameters
     complex, allocable :: b(:),c(:),d(:)
  end type contVar

  function contVarAllocate(Ndefined)
    real :: Ndefined
    N=Ndefined
    allocate(f(N))
    allocate(x(N))
    allocate(b(N))
    allocate(c(N))
    allocate(d(N))    
  end function contVarN
  
  subroutine contVarInit(this)
    type(contVar) :: this
    !These are for holding the spline parameters temporarily
    real, dimension(size(this%x)) :: rb,rc,rd,ib,ic,id
    call spline(this%x,real(this%f),rb,rc,rd,size(this%x))
    call spline(this%x,real((0,-i)*this%f),ib,ic,id,size(this%x)) !this funky thing is just to send the

    !combine the cofficients to a complex # array
    this%b=rb + (0,1)*ib
    this%c=rc + (0,1)*ic
    this%d=rd + (0,1)*id
  end subroutine contVarInit

  function contVarInterp(this,q)
    type(contVar) :: this
    !the value at which you want the value of f
    real :: q
    real :: contVarInterp
    contVarInterp=ispline(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(this%x)) + (0,1)*ispline(q,this%x,real((0,-1)*this%f),real((0,-1)*this%b),real((0,-1)*this%c),real((0,-1)*this%d),size(this%x))
  end subroutine contVarInterp

end module classContVar
