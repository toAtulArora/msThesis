program oneParticle
  use gnuplot_fortran
  implicit none

  real, parameter :: xMax=10, xMin=-10, dx=0.01, dt=0.001, tMax=10
  integer, parameter :: maxS=(xMax-xMin)/dx, maxT=tMax/dt
  
  real, dimension(maxS,maxT) :: xc,k1,k2,k3,k4
  complex, dimension(maxS,maxT) :: x
  complex, dimension(maxS,maxT) :: psi
  complex, dimension(maxS) :: psic,m1,m2,m3,m4

  psi=0
  x=0

  psic = 
  
contains
  subroutine initGaussian(psiPar)
    complex, dimension(:) :: psiPar
  end subroutine initGaussian
  
end program oneParticle
