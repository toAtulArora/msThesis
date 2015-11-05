module classContVar
  implicit none
  private
  public :: contVar, contVarInit, contVarInterp

  type contVar
     !the data points f(x)
     complex, dimension(:) :: f
     !the corresponding (x)
     real, dimension(:) :: x
     !the computed spline parameters
     complex, dimension(:) :: b,c,d
  end type contVar

  subroutine contVarInit(this)
    type(contVar) :: this
    real, dimension(size(this%
    call spline(xPar,real(del2psiPar),rb,rc,rd,size(xPar))
    !combine the cofficients to a complex # array
    b=rb + (0,1)*ib
    c=rc + (0,1)*ic
    d=rd + (0,1)*id
    call spline(xPar,real((0,-i)*del2psiPar),ib,ic,id,size(xPar)) !this funky thing is just to send the    
  end subroutine contVarInit
end module classContVar
