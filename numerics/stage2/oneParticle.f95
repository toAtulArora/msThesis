program oneParticle
  use classContVar
  use gnuplot_fortran
  implicit none

  !dt=0.0000001  
  real, parameter :: xMax=15, xMin=-15, dx=0.1,dt=0.0001, tMax=4.0, sigma=0.5, xNot=-2, omegaSquare=16,m=1
  ! real, parameter :: xMax=15, xMin=-20, dx=0.1,dt=0.0001, tMax=4.0, sigma=0.5, xNot=0, omegaSquare=16,m=1

  real, parameter:: pi=3.14159265359,rootTwoPi=sqrt(2*pi),hbar=1
  integer, parameter :: maxS=(xMax-xMin)/dx, maxT=tMax/dt, ensambleSize=15


  integer :: j,qStep !just for counting misc. things
  ! real, dimension(maxS,maxT) :: qc
  real, dimension(ensambleSize) :: k1,k2,k3,k4,qc
  ! this is for plotting
  real, dimension(ensambleSize*maxT) :: qTemp
  ! complex, dimension(maxS,maxT) :: q

  real, dimension(ensambleSize,maxT) :: q
  
  !type(contVar), dimension(maxT) :: psi
  complex, dimension(maxS,maxT) :: psi
  complex,dimension(maxS) :: debugArray !remove this from the final code
  type(contVar) :: psic,del2psic
  !complex:: m1,m2,m3,m4
  complex, dimension(maxS):: m1,m2,m3,m4  
  !complex, dimension(maxS) :: psic,m1,m2,m3,m4,e,f,g,del2psic,b,c,d
  real, dimension(maxS) :: x, Varray

  integer :: timeStep
  
  call startPlot()
  
  !allocates appropriate space in the contVar datatype
  call psic%contVarAllocate(maxS)
  call del2psic%contVarAllocate(maxS)
  !we're assuming that x wouldn't change over iterations
  x = (/(qFi(j),j=1,maxS)/)
  psic%x=x
  del2psic%x=x

  x=0
  ! initialize the state to guassian
  call initGaussian(psic%f,x)  
  !start with that state
  psi(:,1)=psic%f
  !init the potential array
  Varray = (/ ( V(qFi(j)), j=1, maxS) /)

  ! initialize the particle to be at 0
  q(:,1)=initEnsamble(psic)
  !q(:,1)= (/ ( qFi(j),j=1,maxS, maxS/ensambleSize) /)
  
  
  !call nextPlot2d(x,abs(psic%f))
  ! call doubleSlitPotential(Varray)
  call plot2dSave(x,Varray,filename='potential.pdf',picFormat=1)
  
  do timeStep=1,maxT-1
     !pick the current psi and save it in psic
     psic%f=psi(:,timeStep)
     call psic%contVarInit

     !pick the particle's current location
     qc=q(:,timeStep)
     
     !evaluate del2psi at specific points
     ! del2psic%f=(/ (psic%contVarDel2(qFi(j)),j=1,maxS) /) !evalDel2psi(psic%f,x)
     
     ! del2psic%f(1)=del2psic%f(2)
     ! del2psic%f(maxS)=del2psic%f(maxS-1)

     
     !del2psic%f=evalDel2psi(psic%f,x)
     
     !write(*,*) del2psic%f
     !evaluate del2 and splines coffecients for delt2
     !call initInterpolateDel2psi(psic,del2psic,b,c,d)
     !interpolate spline cofficients for psi
     !call initInterpolatePsi(psic,e,f,g)

     !with the tabulated points defined, evalute the spline coefficients
     !call psic%contVarInit
     !call del2psic%contVarInit

     
     ! if(timeStep == 1 ) then
     !    Varray=0
     ! elseif (timeStep == maxT/8) then
     !    call doubleSlitPotential(Varray)
     ! elseif (timeStep == maxT/8 + 5) then !maxT/64) then
     !    Varray=0
     ! end if


     ! Varray = (/ ( Vdouble(qFi(j)), j=1, maxS) /)

     m1=psiDot(psic%f,Varray)
     m2=psiDot(psic%f + 0.5*dt*m1,Varray)
     m3=psiDot(psic%f + 0.5*dt*m2,Varray)
     m4=psiDot(psic%f + dt*m3, Varray)
     psi(:,timeStep+1)=psic%f + (dt/6)*(m1 + 2*m2 + 2*m3 + m4)

     !evolve the particle using RK4 (don't know how useful it is..but let's see)
     k1=qDot(psic,qc)
     k2=qDot(psic,qc+0.5*dt*k1)
     k3=qDot(psic,qc+0.5*dt*k2)
     k4=qDot(psic,qc+dt*k3)
     q(:,timeStep+1) = qc + (dt/6)*(k1 + 2*k2 + 2*k3 + k4)





     
     ! write(*,*) k1
     ! !without enforcing the boundary condition
     ! do qStep=1,maxS
     !    q=qFi(qStep)
     !    m1=psiDot(psic,del2psic,q)
     !    m2=psiDot(psic,del2psic,q + 0.5*dt*(abs(m1)))
     !    m3=psiDot(psic,del2psic,q + 0.5*dt*(abs(m2)))
     !    m4=psiDot(psic,del2psic,q + dt*(abs(m3)))
     !    psi(qStep,timeStep+1)=psic%f(qStep) + (dt/6)*(m1 + 2*m2 + 2*m3 + m4)

     !    ! q=qFi(qStep)
     !    ! psi(qStep,timeStep+1) = psic%f(qStep) + psiDot(psic,del2psic,q)*dt
     !    !!write(*,*) psiDot(psic,del2psic,q)
     ! end do
     if (mod(timeStep,100)==0 ) then
        ! if (mod(timeStep,maxT/10)==0) then
        !call nextPlot2d(x(2:maxS-1),Varray(2:maxS-1))
        !call nextPlot2d(x(2:maxS-1),abs(psi(2:maxS-1,timeStep)))
        
        !       call nextPlot2d( (/  ( tFi(j),j=1,timeStep) /) , q(1,1:timeStep)   )

        qTemp=(/ (q(j,1:timeStep),j=1,ensambleSize) /)
        call nextPlot2d( (/ ( tFi(mod(j,timeStep)),j=1,timeStep*ensambleSize) /) ,   qTemp(1:timeStep*ensambleSize)   )
         
        !call nextPlot2d(x(2:maxS-1),abs( (/ (psic%contVarDel2(qFi(j)),j=2,maxS-1) /) ))
        !call nextPlot2d(x(2:maxS-1),abs( (/ (psiDot(psic,del2psic,(qFi(j))),j=2,maxS-1) /) ))
        
     end if

     !psi(:,timeStep+1)=psi(:,timeStep)
     ! call nextPlot2d(x,abs(psi(:,timeStep)))
     !call nextPlot2d(x,abs(del2psic%f))
     ! call nextPlot2d(x,abs(del2psic%f))

     ! call psic%contVarInit()
     ! !debugArray = (/(psic%contVarInterp(j),j=1,maxS)/)

     ! !debugArray = (/(psic%contVarInterp(qFi(j)),j=1,maxS)/)
     ! !call nextPlot2d(x,abs(debugArray)) !abs((/(psic%contVarInterp(qFi(j)),j=1,maxS)/)))
     ! call nextPlot2d(x,abs((/(psic%contVarInterp(qFi(j)),j=1,maxS)/)))

     ! call nextPlot2d(x,x)
     
     !call nextPlot2d(x,abs(del2psic%f))
  end do
  
  
  !call nextPlot2d(x,abs(psic))
  !call plot2dSave(x,x,filename='testinggla.pdf',picFormat=1)

  call endPlot()
contains
  function qDot(psiPar,q)
    type(contVar) :: psiPar
    real,dimension(:) :: q
    real,dimension(size(q)) :: qDot
    ! write(*,*) psiPar%contVarDel(q)
    !precision(q(1))
    qDot = hbar*(1/m)* real( (0,-1) * psiPar%contVarDel(q)/(psiPar%contVarInterp(q)+ (1E-24) ) )
    !write(*,*) psiPar%contVarDel(q)
  end function qDot
  
  function psiDot(psi,Varray)
    complex, dimension(:) :: psi
    real, dimension(:) :: Varray
    complex, dimension(size(psi)) :: psiDot,kineticPart,potentialPart

    kineticPart=-hbar*hbar*evalDel2(psi)/(2*m)
    kineticPart(1)=kineticPart(2)
    kineticPart(size(psi))=kineticPart(size(psi)-1)

    potentialPart=Varray*psi
    psiDot=((0,-1)/hbar) * (kineticPart + potentialPart)
    ! psiDot=0
  end function psiDot
  
  subroutine initGaussian(psiPar,xPar)
    complex, dimension(:) :: psiPar
    real, dimension(:) :: xPar
    real :: q,omega
    integer :: l
    psiPar=0
    xPar=0
    omega=pi/(qFi(maxS))
    do l=1,maxS
       q=qFi(l) !dx*l + xMin
       psiPar(l)=exp(-((q-xNot)*(q-xNot))/(2*sigma*sigma))/(sigma*rootTwoPi)
       !psiPar(l)=q !*q !cos(omega*q)
       xPar(l)=q
    end do
  end subroutine initGaussian


  function initEnsamble(psiPar)
    type(contVar) :: psiPar
    real,dimension(size(psiPar%f)) :: prob
    real :: randHeight,probMax,randQ
    integer :: randIndex,particlesSoFar
    real,dimension(ensambleSize) :: initEnsamble

    !TODO: Describe the lagorithm somewhere (this is new and generalizable, although equivalent to the older one you'd thought of independently (but turned out to be standard))
    particlesSoFar=0
    prob=abs(psiPar%f)*abs(psiPar%f)
    probMax=maxval(prob)
    do
       randHeight = rand()*probMax
       !randIndex = rand()*maxS
       randQ=rand()*(xMax-xMin) + xMin
       if (randIndex == 0) then
          cycle
       end if
       !the following step could be improved using the interpolation, but pehraps later; not that important
       if ( prob(iFq(randQ)) > randHeight) then
          particlesSoFar=particlesSoFar + 1
          initEnsamble(particlesSoFar) = randQ !qFi(randIndex)
          if ( particlesSoFar >= ensambleSize) then
             exit
          end if
       end if
    end do
   
  end function initEnsamble
  ! function evalDel(y)
    
  ! end function evalDel
  
  function evalDel2(y)
    complex, dimension(:) :: y
    complex, dimension(size(y)) :: evalDel2
    integer :: m
    do m=2,maxS-1
       evalDel2(m)=( y(m+1) + y(m-1) - 2*y(m) )/(dx*dx)
    end do
    !bad boundary conditions, but what to do :(
    ! evalDel2(1)=evalDel2(2)
    ! evalDel2(maxS)=evalDel2(maxS-1)
    !put cyclic boundary conditions
    evalDel2(1)= ( y(2) + y(maxS) - 2*y(1) )/(dx*dx)
    evalDel2(maxS)=( y(1) + y(maxS-1) - 2*y(maxS) )/(dx*dx)
    
  end function evalDel2
  
  function evalDel2psi(psiPar,xPar)
    complex, dimension(:) :: psiPar
    complex, dimension(size(psiPar)) :: evalDel2psi
    real, dimension(:) :: xPar
    integer :: m

    !evaluate del2psi at tabulated points
    do m=2,maxS-1
       evalDel2psi(m)=( psiPar(m+1) + psiPar(m-1) - 2*psiPar(m) )/(dx*dx)
    end do
    !bad boundary conditions, but what to do :(
    evalDel2psi(1)=evalDel2psi(2)
    evalDel2psi(maxS)=evalDel2psi(maxS-1)
  end function evalDel2psi !initInterpolateDel2psi

  
  !OBSOLTE
  !this will generate the array b,c,d which will be used to interpolate del2psi
  ! subroutine initInterpolateDel2psi(psiPar,del2psiPar,xPar,b,c,d)
  !   complex, dimension(:) :: psiPar,del2psiPar,b,c,d
  !   real, dimension(:) :: xPar
  !   real, dimension(size(xPar)) :: rb,rc,rd,ib,ic,id
  !   integer :: m

  !   !evaluate del2psi at tabulated points
  !   do m=2,maxS-1
  !      del2psiPar(m)=( psiPar(m+1) + psiPar(m-1) - 2*psiPar(m) )/dx
  !   end do
  !   !bad boundary conditions, but what to do :(
  !   del2psiPar(1)=del2psiPar(2)
  !   del2psiPar(maxS)=del2psiPar(maxS-1)

  !   !now find the coefficients to interpolate
  !   call spline(xPar,real(del2psiPar),rb,rc,rd,size(xPar))
  !   call spline(xPar,real((0,-i)*del2psiPar),ib,ic,id,size(xPar)) !this funky thing is just to send the imaginary part. for some reason couldn't figure how to do this any more simply in fortran
  !   !combine the cofficients to a complex # array
  !   b=rb + (0,1)*ib
  !   c=rc + (0,1)*ic
  !   d=rd + (0,1)*id
  ! end subroutine initInterpolateDel2psi
  


  !give it psi at all x, x (like -10,-9.8 ... 9.8,10), and q (the point at which to evalute) and it'll yield del2psi
  !but make sure you call the initInterpolateDel2psi function before this
  ! function del2psiAtQ(del2psiPar,xPar,q,b,c,d)
  !   complex, dimension(:)::del2psiPar,b,c,d
  !   real, dimension(:) :: xPar
  !   real :: q,del2psiAtQ
  !   del2psiAtQ=ispline(q,xPar,real(del2psiPar),real(b),real(c),real(d),size(psiPar)) + (0,1)*ispline(q,xPar,real((0,-1)*del2psiPar),real((0,-1)*b),real((0,-1)*c),real((0,-1)*d),size(psiPar))
  ! end function del2psiAtQ

  !   !give it psi(q),del2psi(q) and q, it'll give you psi dot
  ! function psiDot(psiPar,del2psiPar,qStep)
  !   type(contVar) :: psiPar,del2psiPar
  !   integer :: qStep
    
  !   complex :: psiDot,kineticPart,potentialPart
  !   complex :: psiAtQ, del2psiAtQ

  !   psiAtQ=psiPar%f(qStep) !contVarInterp(psiPar,q)
  !   !write (*,*) psiAtQ
  !   del2psiAtQ=del2psiPar%f(qStep) !contVarInterp(del2psiPar,q)
  !   !write(*,*) del2psiAtQ
  !   !integer :: m
  !   !real :: q,qPlus,qMinus,qDelta
  !   ! obtain q from index
  !   ! q=qFi(m)
  !   ! qPlus=qFi(m+1)
  !   ! qMinus=qFi(m-1)
  !   ! qDelta=qPlus-q
  !   kineticPart=-hbar*hbar*del2psiAtQ !(psiPar(qPlus) + psiPar(qMinus) - 2*psiPar(q))/(qDelta*qDelta)
  !   potentialPart=V(q)*psiAtQ !psiPar(q)
  !   psiDot=((0,-1)*hbar)*(kineticPart + potentialPart)
    
  ! end function psiDot


  

  ! !give it psi(q),del2psi(q) and q, it'll give you psi dot
  ! function psiDotLegacy(psiPar,del2psiPar,q)
  !   type(contVar) :: psiPar,del2psiPar
  !   real :: q
    
  !   complex :: psiDotLegacy,kineticPart,potentialPart
  !   complex :: psiAtQ, del2psiAtQ

  !   psiAtQ=psiPar%contVarInterp(q)
  !   !write (*,*) psiAtQ
  !   del2psiAtQ=psiPar%contVarDel2(q)
    
  !   !write(*,*) del2psiAtQ
  !   !integer :: m
  !   !real :: q,qPlus,qMinus,qDelta
  !   ! obtain q from index
  !   ! q=qFi(m)
  !   ! qPlus=qFi(m+1)
  !   ! qMinus=qFi(m-1)
  !   ! qDelta=qPlus-q
  !   kineticPart=-hbar*hbar*del2psiAtQ/2 !(psiPar(qPlus) + psiPar(qMinus) - 2*psiPar(q))/(qDelta*qDelta)
  !   potentialPart=V(q)*psiAtQ !psiPar(q)
  !   psiDotLegacy=((0,-1)/hbar)*(kineticPart + potentialPart)
  !   !psiDotLegacy=-omegaSquare*psiAtQ
    
  ! end function psiDotLegacy

  function V(q)
    real :: q
    real :: V
    V= omegaSquare*q*q/2
  end function V
  
  function qFi(index)
    integer:: index
    real::qFi
    qFi=dx*index + xMin
  end function qFi

  function iFq(q)
    real:: q
    integer::iFq
    iFq= (q - xMin)/dx
  end function iFq
  
  function tFi(index)
    integer:: index
    real:: tFi
    tFi=dt*index
  end function tFi

  subroutine doubleSlitPotential(Varray)
    real, dimension(:) :: Varray
    integer :: centre1,centre2,width,l

    centre1=maxS/2 + maxS/32
    centre2=maxS/2 - maxS/32
    width=maxS/64

    Varray=1E4
    do l=1,width
       Varray(centre1 + l - (width/2) ) = 0
       Varray(centre2 + l - (width/2) ) = 0
    end do
  end subroutine doubleSlitPotential
end program oneParticle
