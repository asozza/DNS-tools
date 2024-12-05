module global
implicit none
integer, parameter :: NLEV_MAX=1000,NMOM=1
real*8, save:: icount(NLEV_MAX)
real*8, save:: mom(NLEV_MAX,NMOM)
integer, save :: nlev,maxn

end module

program boxcount
use global
implicit none
real*8, parameter :: xlx=6.2831853071795864769d0,xly=xlx,xlz=xlx
!integer, parameter :: NX_MAX=4096,NY_MAX=NX_MAX
!integer, parameter :: NX_MAX2=NX_MAX/2,NY_MAX2=NY_MAX/2
!integer, parameter :: NMIN=8
integer, parameter :: NP=3200000/20
!real*8 xp(2,NP)
real*8 d0
!integer box(NX_MAX, NY_MAX),box2(NX_MAX2,NY_MAX_2)
integer, allocatable :: box(:,:,:,:),nbox(:,:,:)
real*4, allocatable :: xp(:,:)
real*8, allocatable :: xp_local(:,:)
integer i,j,k,ilev,nx,ny,nz,nx_max,ny_max,nz_max,nmin,ipl,ip,imom,it
real*8 scra,scram,norm
real*8 dx,dy,dz,invdx,invdy,invdz

character*255 buf

integer ifr1,ifr2,ifr
character*80 nome

!seme=52310480
call getarg(1,buf)
read(buf,*,err=90, end=90)ifr1
call getarg(2,buf)
read(buf,*,err=90, end=90)ifr2

call getarg(3,buf)
read(buf,*,err=90, end=90)nmin
call getarg(4,buf)
read(buf,*,err=90, end=90)nlev

call getarg(5,buf)
read(buf,*,err=90, end=90)it

!nx=NX_MAX/2**(NLEV-1)
!ny=NY_MAX/2**(NLEV-1)
nx=nmin
ny=nmin
nz=nmin
nx_max=nmin*2**(nlev-1)
ny_max=nmin*2**(nlev-1)
nz_max=nmin*2**(nlev-1)
dx=xlx/nx
dy=xly/ny
dz=xlz/nz
invdx=1.d0/dx
invdy=1.d0/dy
invdz=1.d0/dz
maxn=(NP/(nx*ny*nz)+1)*30
icount=0.d0
mom=0.d0

allocate(box(maxn,nx,ny,nz),nbox(nx,ny,nz),xp_local(3,maxn),xp(3,NP))

xp=0.d0
xp_local=0.d0

do ifr=ifr1,ifr2
  box=0
  nbox=0
  write(nome,'("./Select/xp.",i2.2,".",i3.3)')it,ifr
  open(1,file=nome,form="unformatted")
  !legge
  read(1)xp
  do ip=1,NP
   i=xp(1,ip)*invdx+1
   !10 if (i.gt.nx) then
   !  xp(1,ip)=xp(1,ip)-1.d-6
   !  i=xp(1,ip)*invdx+1
   !  goto 10
   !end if
   if (i.gt.nx) then
     write(0,*)"errore box size x"
     stop
   end if
   
   j=xp(2,ip)*invdy+1
   !20 if (j.gt.ny) then
   !  xp(2,ip)=xp(2,ip)-1.d-6
   !  j=xp(2,ip)*invdy+1
   !  goto 20
   !end if
   if (j.gt.ny) then
     write(0,*)"errore box size y"
     stop
   end if
 
   k=xp(3,ip)*invdz+1
   !20 if (j.gt.ny) then
   !  xp(2,ip)=xp(2,ip)-1.d-6
   !  j=xp(2,ip)*invdy+1
   !  goto 20
   !end if
   if (k.gt.nz) then
     write(0,*)"errore box size z"
     stop
   end if

 
   nbox(i,j,k)=nbox(i,j,k)+1
   if (nbox(i,j,k).gt.maxn) then
     write(0,*)"maxn troppo piccolo"
     stop
   end if
   box(nbox(i,j,k),i,j,k)=ip
  end do
  close(1)
  
  ilev=1
  do k=1,nz
  do j=1,ny
  do i=1,nx
    if (nbox(i,j,k).eq.0) cycle
    do ipl=1,nbox(i,j,k)
      xp_local(1,ipl)=xp(1,box(ipl,i,j,k))*invdx-dble(i-1)
      xp_local(2,ipl)=xp(2,box(ipl,i,j,k))*invdy-dble(j-1)
      xp_local(3,ipl)=xp(3,box(ipl,i,j,k))*invdz-dble(k-1)
    end do
    call descend(xp_local,nbox(i,j,k),1)
    icount(1)=icount(1)+1.d0
    scra=dble(nbox(i,j,k))
    scram=scra-1.d0
    do imom=1,NMOM
      scram=scra*scram
      mom(1,imom)=mom(1,imom)+scram
    end do
  end do
  end do
  end do

end do


norm=real(ifr2-ifr1+1)
do ilev=nlev,1,-1
  write(6,'(20g)')real(dx*0.5d0**(ilev-1)), real(icount(ilev)/(NP*norm)),(real(mom(ilev,imom)/(norm*dble(NP)**(nmom+1))),imom=1,NMOM)
end do

deallocate(box,nbox,xp_local,xp)

goto 100
90 write(0,*)"uso:boxcount ifr1 ifr2 nmin nlev"

100 end program


recursive subroutine descend(xp_local,nlocal,level)
use global
implicit none
real*8 xp_local(3,maxn), xp_down(3,maxn)
integer nbox_local(0:1,0:1,0:1),box_local(nlocal,0:1,0:1,0:1)
integer ir,jr,kr,level,ipl
integer imom
integer local_count,nlocal
real*8 scra,scram

nbox_local=0
box_local=0
if (level.lt.nlev) then
   do ipl=1,nlocal
     !write(100,*)real(xp_local(1,ipl))
     ir=xp_local(1,ipl)*2
     jr=xp_local(2,ipl)*2
     kr=xp_local(3,ipl)*2
     nbox_local(ir,jr,kr)=nbox_local(ir,jr,kr)+1
     box_local(nbox_local(ir,jr,kr),ir,jr,kr)=ipl
   end do
   do ir=0,1
   do jr=0,1
   do kr=0,1
     if (nbox_local(ir,jr,kr).eq.0) cycle 
     do ipl=1,nbox_local(ir,jr,kr)
       xp_down(1,ipl)=2*xp_local(1,box_local(ipl,ir,jr,kr))-dble(ir)
       xp_down(2,ipl)=2*xp_local(2,box_local(ipl,ir,jr,kr))-dble(jr)
       xp_down(3,ipl)=2*xp_local(3,box_local(ipl,ir,jr,kr))-dble(kr)
     end do
     icount(level+1)=icount(level+1)+1.d0
     scra=dble(nbox_local(ir,jr,kr))
     scram=scra-1.d0
     do imom=1,NMOM
      scram=scra*scram
      mom(level+1,imom)=mom(level+1,imom)+scram 
     end do
     call descend(xp_down,nbox_local(ir,jr,kr),level+1)
   end do
   end do
   end do
end if
 
return
end subroutine

function rann(irand)
implicit none
integer irand
integer mask
real*8 rann
      
mask = '7FFFFFFF'X
irand = iand((69069*irand + 1),mask)
rann = dble(irand) / dble(mask)

return
end function
