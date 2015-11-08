program testing
  complex :: a
  real, dimension(10):: alpha, beta
  complex, dimension(10) :: b

  
  a=1/(0,1)
  write(*,*) a

  alpha=(/(j,j=1,10)/)
  beta=(/(l*0.5,l=1,10)/)
  b= alpha + (0,1)*beta !(/((alpha(l),beta(l)),l=1,10)/)
  call sendReal(real((0,-1)*b)) 
  alpha=initTest(alpha)
  
  !write (*,*) b
contains
  subroutine sendReal(alphaPar)
    real, dimension(:) :: alphaPar
    real, dimension(size(alphaPar)) :: ba
    ba=alphaPar
    write(*,*) alphaPar,ba
  end subroutine sendReal

  function initTest(gla)
    real :: gla(:)
    real :: initTest(:)
    gla=gla+1
  end function initTest
end program testing
