program oneParticle
  use gnuplot_fortran
  implicit none

  real, parameter :: xMax=10, xMin=-10, dx=0.01, dt=0.001, tMax=10, sigma=0.5
  real, parameter:: pi=3.14159265359,rootTwoPi=sqrt(2*pi)
  integer, parameter :: maxS=(xMax-xMin)/dx, maxT=tMax/dt
  
  real, dimension(maxS,maxT) :: qc,k1,k2,k3,k4
  complex, dimension(maxS,maxT) :: q
  complex, dimension(maxS,maxT) :: psi
  complex, dimension(maxS) :: psic,m1,m2,m3,m4
  real, dimension(maxS) :: x

  integer :: timeStep
  
  call startPlot()
  
  psi=0
  x=0
  ! initialize the state to guassian
  call initGaussian(psic,x)

  !start with that state
  psi(:,1)=psic
  call nextPlot2d(x,abs(psic))
  !call plot2dSave(x,x,filename='initialState.pdf',picFormat=1)
  do timeStep=1,maxT
     
  end do
  
  
  call nextPlot2d(x,abs(psic))
  !call plot2dSave(x,x,filename='testinggla.pdf',picFormat=1)

  call endPlot()
contains
  subroutine initGaussian(psiPar,xPar)
    complex, dimension(:) :: psiPar
    real, dimension(:) :: xPar
    real :: xr
    integer :: l
    psiPar=0
    xPar=0
    do l=1,maxS
       xr=XFi(l) !dx*l + xMin
       psiPar(l)=exp(-(xr*xr)/(2*sigma*sigma))/(sigma*rootTwoPi)
       x(l)=xr
    end do
  end subroutine initGaussian
  function xFi(index)
    integer:: index
    real::xFi
    xFi=dx*index + xMin
  end function xFi
  function tFi(index)
    integer:: index
    real:: tFi
    tFi=dt*index
  end function tFi
end program oneParticle
