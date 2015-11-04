program testing
  complex :: a
  real, dimension(10):: alpha, beta
  complex, dimension(10) :: b

  
  a=1/(0,1)
  write(*,*) a

  alpha=(/(j,j=1,10)/)
  beta=(/(l*0.5,l=1,10)/)
  b= alpha + (0,1)*beta !(/((alpha(l),beta(l)),l=1,10)/)

  write (*,*) b

  subroutine sendReal(alphaPar)
    real, dimension(:) :: alphaPar
    write(*,*) alphaPar
  end subroutine sendReal
end program testing
