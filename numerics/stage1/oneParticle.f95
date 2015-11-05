program oneParticle
  use gnuplot_fortran
  implicit none

  real, parameter :: xMax=10, xMin=-10, dx=0.01, dt=0.001, tMax=10, sigma=0.5
  real, parameter:: pi=3.14159265359,rootTwoPi=sqrt(2*pi),hbar=1
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
  do timeStep=2,maxT
     psic=psi(:,timeStep)

     !evaluate splines coffecients for delt2
     
     !without enforcing the boundary condition
     do qStep=1,maxS
        m1=qDot(psic,q)
        m2=qDot(psic,q + 0.5*dt*m1)
        m3=qDot(psic,q + 0.5*dt*m2)
        m4=qDot(psic,q + dt*m3)
        
     end do
  end do
  
  
  call nextPlot2d(x,abs(psic))
  !call plot2dSave(x,x,filename='testinggla.pdf',picFormat=1)

  call endPlot()
contains
  subroutine initGaussian(psiPar,xPar)
    complex, dimension(:) :: psiPar
    real, dimension(:) :: xPar
    real :: q
    integer :: l
    psiPar=0
    xPar=0
    do l=1,maxS
       q=qFi(l) !dx*l + xMin
       psiPar(l)=exp(-(xr*xr)/(2*sigma*sigma))/(sigma*rootTwoPi)
       x(l)=xr
    end do
  end subroutine initGaussian

  !this will generate the array b,c,d which will be used to interpolate del2psi
  subroutine initInterpolateDel2psi(psiPar,del2psiPar,xPar,b,c,d)
    
    do m=2,maxS-1
       del2psiPar(m)=( psiPar(m+1) + psiPar(m-1) - 2*psiPar(m) )/dx
    end do
    !bad boundary conditions, but what to do :(
    del2psiPar(1)=del2psiPar(2)
    del2psiPar(maxS)=del2psiPar(maxS-1)
    spline(xPar,del2psiPar,b,c,d,size(xPar))
  end subroutine initInterpolateDel2psi
  
  !give it psi at all x, x (like -10,-9.8 ... 9.8,10), and q (the point at which to evalute) and it'll yield del2psi
  !but make sure you call the initInterpolateDel2psi function before this
  function del2psiAtQ(del2psiPar,xPar,q,b,c,d)
    complex, dimension(:)::del2psiPar,xPar,b,c,d
    real :: q,del2psiAtQ
    del2psiAtQ=ispline(q,xPar,real(del2psiPar),real(b),real(c),real(d),size(psiPar)) + (0,1)*ispline(q,xPar,real(del2psiPar),real(b),real(c),real(d),size(psiPar))
  end function del2psiAtQ
  
  !give it psi(q),del2psi(q) and q, it'll give you psi dot
  function psiDot(psiAtQ,del2psiAtQ,q)
    complex, dimension(:) :: psiPar
    complex :: psiDot,kineticPart,potentialPart
    real :: q
    !integer :: m
    !real :: q,qPlus,qMinus,qDelta
    ! obtain q from index
    ! q=qFi(m)
    ! qPlus=qFi(m+1)
    ! qMinus=qFi(m-1)
    ! qDelta=qPlus-q
    kineticPart=-hbar*hbar*del2psiAtQ !(psiPar(qPlus) + psiPar(qMinus) - 2*psiPar(q))/(qDelta*qDelta)
    potentialPart=V(q)*psiAtQ !psiPar(q)
    psiDot=(1/(0,1)hbar)*(kineticPart + potentialPart)
    
  end function psiDot

  function V(q)
    real :: q
    real :: V
    V=-q*q
  end function V
  
  function qFi(index)
    integer:: index
    real::qFi
    qFi=dx*index + xMin
  end function qFi
  function tFi(index)
    integer:: index
    real:: tFi
    tFi=dt*index
  end function tFi
end program oneParticle
