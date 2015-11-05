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
    real, dimension(size(this%f)) :: rb,rc,rd,ib,ic,id
    call spline(xPar,real(del2psiPar),rb,rc,rd,size(xPar))
    !combine the cofficients to a complex # array
    b=rb + (0,1)*ib
    c=rc + (0,1)*ic
    d=rd + (0,1)*id
    call spline(this%x,real((0,-i)*this%f),ib,ic,id,size(xPar)) !this funky thing is just to send the    
  end subroutine contVarInit
end module classContVar
