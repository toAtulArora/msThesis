module classContVar
  implicit none
  private
  public :: contVar, contVarInit, contVarInterp

  type contVar
     !the number of data points
     real :: N
     !the data points f(x)
     complex, dimension(N) :: f
     !the corresponding (x)
     real, dimension(N) :: x
     !the computed spline parameters
     complex, dimension(N) :: b,c,d
  end type contVar

  subroutine contVarInit(this)
    type(contVar) :: this
    !These are for holding the spline parameters temporarily
    real, dimension(size(this%x)) :: rb,rc,rd,ib,ic,id
    call spline(this%x,real(this%f),rb,rc,rd,size(this%x))
    call spline(this%x,real((0,-i)*this%f),ib,ic,id,size(this%x)) !this funky thing is just to send the

    !combine the cofficients to a complex # array
    b=rb + (0,1)*ib
    c=rc + (0,1)*ic
    d=rd + (0,1)*id
  end subroutine contVarInit

  function contVarInterp(this,q)
    type(contVar) :: this
    real :: q
    real :: contVarInterp
    contVarInterp=ispline(q,this%x,real(this%f),real(this%b),real(this%c),real(this%d),size(psiPar)) + (0,1)*ispline(q,xPar,real((0,-1)*del2psiPar),real((0,-1)*b),real((0,-1)*c),real((0,-1)*d),size(psiPar))
  end subroutine contVarInterp

end module classContVar
