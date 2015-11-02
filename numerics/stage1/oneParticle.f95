program oneParticle
  use gnuplot_fortran
  implicit none

  real, parameter :: xMax=10, xMin=-10, dx=0.01, dt=0.001, tMax=10
  integer, parameter :: maxS=(xMax-xMin)/dx, maxT=tMax/dt
  
  real, dimension(maxS) :: x,k1,k2,k3,k4
  complex, dimension(maxS) :: psi,m1,m2,m3,m4

  
contains

  
end program oneParticle
