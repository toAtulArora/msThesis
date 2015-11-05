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
end module classContVar
