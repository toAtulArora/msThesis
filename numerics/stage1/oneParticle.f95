program oneParticle
  use classContVar
  implicit none

  real, parameter :: xMax=10, xMin=-10, dx=0.01, dt=0.001, tMax=10, sigma=0.5
  real, parameter:: pi=3.14159265359,rootTwoPi=sqrt(2*pi),hbar=1
  integer, parameter :: maxS=(xMax-xMin)/dx, maxT=tMax/dt
  
  real, dimension(maxS,maxT) :: qc
  complex :: k1,k2,k3,k4
  complex, dimension(maxS,maxT) :: q
  !type(contVar), dimension(maxT) :: psi
  !complex, dimension(maxS,maxT) :: psi
  type(contVar) :: psic,del2psic
  complex :: m1,m2,m3,m4
  !complex, dimension(maxS) :: psic,m1,m2,m3,m4,e,f,g,del2psic,b,c,d
  real, dimension(maxS) :: x

  integer :: timeStep
  
  call startPlot()
  
  !allocates appropriate space in the contVar datatype
  call contVarAllocate(psic,maxS)
  call contVarAllocate(del2psic,maxS)
  !we're assuming that x wouldn't change over iterations
  x = (/(qFi(j),j=1,maxS)/)
  psic%x=x
  del2psic%x=x

  psi=0
  del2psic=0
  x=0
  ! initialize the state to guassian
  call initGaussian(psic,x)

  !start with that state
  psi(:,1)=psic
  call nextPlot2d(x,abs(psic))
  !call plot2dSave(x,x,filename='initialState.pdf',picFormat=1)
  do timeStep=2,maxT
     !pick the current psi and save it in psic
     psic%f=psi(:,timeStep)
     !evaluate del2psi at specific points
     del2psic%f=evalDel2psi(psic%f,x)
     
     !evaluate del2 and splines coffecients for delt2
     !call initInterpolateDel2psi(psic,del2psic,b,c,d)
     !interpolate spline cofficients for psi
     !call initInterpolatePsi(psic,e,f,g)

     !with the tabulated points defined, evalute the spline coefficients
     contVarInit(psic)
     contVarInit(del2psic)
     
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

  function evalDel2psi(psiPar,xPar)
    complex, dimension(:) :: psiPar
    complex, dimension(size(psiPar)) :: evalDel2psi
    real, dimension(:) :: xPar
    integer :: m

    !evaluate del2psi at tabulated points
    do m=2,maxS-1
       evalDel2psi(m)=( psiPar(m+1) + psiPar(m-1) - 2*psiPar(m) )/dx
    end do
    !bad boundary conditions, but what to do :(
    evalDel2psi(1)=evalDel2psi(2)
    evalDel2psi(maxS)=evalDel2psi(maxS-1)

  end function initInterpolateDel2psi

  
  !OBSOLTE
  !this will generate the array b,c,d which will be used to interpolate del2psi
  subroutine initInterpolateDel2psi(psiPar,del2psiPar,xPar,b,c,d)
    complex, dimension(:) :: psiPar,del2psiPar,b,c,d
    real, dimension(:) :: xPar
    real, dimension(size(xPar)) :: rb,rc,rd,ib,ic,id
    integer :: m

    !evaluate del2psi at tabulated points
    do m=2,maxS-1
       del2psiPar(m)=( psiPar(m+1) + psiPar(m-1) - 2*psiPar(m) )/dx
    end do
    !bad boundary conditions, but what to do :(
    del2psiPar(1)=del2psiPar(2)
    del2psiPar(maxS)=del2psiPar(maxS-1)

    !now find the coefficients to interpolate
    call spline(xPar,real(del2psiPar),rb,rc,rd,size(xPar))
    call spline(xPar,real((0,-i)*del2psiPar),ib,ic,id,size(xPar)) !this funky thing is just to send the imaginary part. for some reason couldn't figure how to do this any more simply in fortran
    !combine the cofficients to a complex # array
    b=rb + (0,1)*ib
    c=rc + (0,1)*ic
    d=rd + (0,1)*id
  end subroutine initInterpolateDel2psi
  
  !give it psi at all x, x (like -10,-9.8 ... 9.8,10), and q (the point at which to evalute) and it'll yield del2psi
  !but make sure you call the initInterpolateDel2psi function before this
  function del2psiAtQ(del2psiPar,xPar,q,b,c,d)
    complex, dimension(:)::del2psiPar,b,c,d
    real, dimension(:) :: xPar
    real :: q,del2psiAtQ
    del2psiAtQ=ispline(q,xPar,real(del2psiPar),real(b),real(c),real(d),size(psiPar)) + (0,1)*ispline(q,xPar,real((0,-1)*del2psiPar),real((0,-1)*b),real((0,-1)*c),real((0,-1)*d),size(psiPar))
  end function del2psiAtQ
  
  !give it psi(q),del2psi(q) and q, it'll give you psi dot
  function psiDot(psiAtQ,del2psiAtQ,q)
    complex, dimension(:) :: psiPar
    complex :: del2psiAtQ
    real :: q
    
    complex :: psiDot,kineticPart,potentialPart

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
