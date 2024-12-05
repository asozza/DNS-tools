module global
  implicit none
  SAVE
  
  ! constants
  real*8, parameter :: pi=2.d0*asin(1.d0)
  integer, parameter :: ndim=2
  integer, parameter :: nlev_max=1000,nmom=1
  real*8, parameter :: xlx=2.d0*pi,xly=xlx
  
  ! parameters
  !integer, parameter :: np=3200000
  integer, parameter :: ifr1=6,ifr2=6,ndep=10
  integer, parameter :: nmin=2,nlev=13
  integer, parameter :: nx=nmin,ny=nx
  integer, parameter :: nx_max=nmin*2**(nlev-1),ny_max=nx_max
  real*8, parameter :: dx=xlx/nx,dy=xly/ny
  real*8, parameter :: invdx=1.d0/dx,invdy=1.d0/dy
  integer, parameter :: maxn=40000
  
  ! variables
  real*8 :: icount(nlev_max)
  real*8 :: mom(nlev_max,nmom)
  real*8, allocatable :: xp(:,:)
  
end module global
! main program
program boxcount
  use global
  implicit none
  
  integer, allocatable :: xplabel(:,:,:),nbox(:,:)
  real*8, allocatable :: xp_local(:,:)
  real*4, allocatable :: dumxp(:,:)
  integer :: i,j,k,ilev,npl,ipl,ip,jp,imom,it,ifr,idep,nptot
  real*8 :: scra,scram,norm,x,y
  character*80 :: filename

  nptot=0
  icount=0.d0
  mom=0.d0
  
  allocate(xplabel(maxn,nx,ny),nbox(nx,ny),xp_local(ndim,maxn))
  
  do ifr=ifr1,ifr2
     do idep=1,ndep
        
        write(filename,"('./Post/iso.'i3.3'.'i3.3)")ifr,idep
        open(unit=11,file=filename,form='unformatted')
        read(11)npl
        write(6,*)npl
        allocate(dumxp(3,npl),xp(3,npl))
        read(11)dumxp
        xp=dble(dumxp)
        close(11)
        deallocate(dumxp)  
        
        xplabel=0
        nbox=0
        xp_local=0.d0
        do ip=1,npl
           i=xp(1,ip)*invdx+1
           j=xp(2,ip)*invdy+1
           nbox(i,j)=nbox(i,j)+1
           xplabel(nbox(i,j),i,j)=ip
        enddo
        
        ilev=1
        do j=1,ny
           do i=1,nx
              if (nbox(i,j).eq.0) cycle
              do ipl=1,nbox(i,j)
                 xp_local(1,ipl)=xp(1,xplabel(ipl,i,j))*invdx-dble(i-1)
                 xp_local(2,ipl)=xp(2,xplabel(ipl,i,j))*invdy-dble(j-1)
              end do
              call descend(xp_local,nbox(i,j),1)
              icount(1)=icount(1)+1.d0
              scra=dble(nbox(i,j))
              scram=scra-1.d0
              do imom=1,nmom
                 scram=scra*scram
                 mom(1,imom)=mom(1,imom)+scram
              end do
           end do
        end do

        ! sum points
        nptot=nptot+npl
        
        deallocate(xp)
        
     enddo
  enddo
     
  norm=real(ifr2-ifr1+1)
  do ilev=nlev,1,-1
     do imom=1,nmom
        mom(ilev,imom)=mom(ilev,imom)/(norm*dble(nptot)**(nmom+1))
     enddo
  enddo

  write(filename,"('./Post/boxcount.dat')")
  open(unit=100,file=filename,status='unknown')  
  do ilev=nlev,1,-1
     x=dx*0.5d0**(ilev-1)
     write(100,97)real(x),real(mom(ilev,1))
  end do
  close(100)
  
  deallocate(xplabel,nbox,xp_local)
  
97 format(24g)

end program boxcount
! recursive descend
recursive subroutine descend(xp_local,nplocal,ilev)
  use global
  implicit none
  
  real*8 :: xp_local(2,maxn),xp_down(2,maxn)
  integer :: nbox_local(0:1,0:1),box_local(nplocal,0:1,0:1)
  integer :: imom,ir,jr,ilev,ipl,jp,local_count,nplocal
  real*8 :: scra,scram
  
  nbox_local=0
  box_local=0
  if (ilev.lt.nlev) then
     do ipl=1,nplocal
        ir=xp_local(1,ipl)*2
        jr=xp_local(2,ipl)*2
        nbox_local(ir,jr)=nbox_local(ir,jr)+1
        box_local(nbox_local(ir,jr),ir,jr)=ipl
     end do
     do jr=0,1
        do ir=0,1
           if (nbox_local(ir,jr).eq.0) cycle 
           do ipl=1,nbox_local(ir,jr)
              xp_down(1,ipl)=2.0*xp_local(1,box_local(ipl,ir,jr))-dble(ir)
              xp_down(2,ipl)=2.0*xp_local(2,box_local(ipl,ir,jr))-dble(jr)
           end do
           icount(ilev+1)=icount(ilev+1)+1.d0
           scra=dble(nbox_local(ir,jr))
           scram=scra-1.d0
           do imom=1,nmom
              scram=scra*scram
              mom(ilev+1,imom)=mom(ilev+1,imom)+scram
           end do
           call descend(xp_down,nbox_local(ir,jr),ilev+1)
        end do
     end do
  end if
  
  return
end subroutine descend
! uniform random number generator
function rann(irand)
  implicit none
  integer irand
  integer mask
  real*8 rann
  
  mask = '7FFFFFFF'X
  irand = iand((69069*irand + 1),mask)
  rann = dble(irand) / dble(mask)
  
  return
end function rann
! rebox
function gp(x,dx,xl)
  implicit none
  real*8 x,dx,xl,gp
  
  gp=x+dx
  
10 if (gp.lt.0.d0) then
     gp=gp+xl
     goto 10
  end if
  
20 if (gp.ge.xl) then
     gp=gp-xl
     goto 20
  end if
  
  return
end function gp
