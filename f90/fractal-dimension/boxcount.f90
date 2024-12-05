module global
  implicit none
  SAVE
  real*8, parameter :: pi=2.d0*asin(1.d0)
  integer, parameter :: ndim=3,npe=256
  integer, parameter :: nlev_max=1000,nmom=1
  real*8, parameter :: xlx=2.d0*pi,xly=xlx,xlz=xlx
  integer, parameter :: np=640000,ntype=4,npt=np/ntype
  real*8 :: icount(ntype,nlev_max)
  real*8 :: mom(ntype,nlev_max,nmom)
  real*8, dimension(ntype,ndim,npt) :: xp
  real*8, dimension(ntype,npt) :: xpu
  integer, parameter :: ifr1=100,ifr2=200
  integer, parameter :: nmin=2,nlev=13
  integer, parameter :: nx=nmin,ny=nx,nz=nx
  integer, parameter :: nx_max=nmin*2**(nlev-1),ny_max=nx_max,nz_max=nx_max
  real*8, parameter :: dx=xlx/nx,dy=xly/ny,dz=xlz/nz
  real*8, parameter :: invdx=1.d0/dx,invdy=1.d0/dy,invdz=1.d0/dz
  integer, parameter :: maxn=400000
end module global
program boxcount
  use global
  implicit none
  integer, allocatable :: box(:,:,:,:),nbox(:,:,:)
  real*8, allocatable :: xp_local(:,:)
  integer :: i,j,k,ilev,ipl,ip,jp,imom,it,ifr
  real*8 :: scra,scram,norm,x,y
  character*80 nome
  
  icount=0.d0
  mom=0.d0
  
  allocate(box(maxn,nx,ny,nz),nbox(nx,ny,nz),xp_local(ndim,maxn))
  
  do ifr=ifr1,ifr2
     
     call legge(ifr)
     
     do jp=1,ntype

        box=0
        nbox=0
        xp_local=0.d0
        
        do ip=1,npt
           i=xp(jp,1,ip)*invdx+1
           if ((i.gt.nx).or.(i.lt.1)) then
              write(0,*)"errore box size x"
              write(0,*)xp(jp,1,ip),xp(jp,2,ip),xp(jp,3,ip)
              stop
           end if
           
           j=xp(jp,2,ip)*invdy+1
           if ((j.gt.ny).or.(j.lt.1)) then
              write(0,*)"errore box size y"
              write(0,*)xp(jp,1,ip),xp(jp,2,ip),xp(jp,3,ip)
              stop
           end if
           
           k=xp(jp,3,ip)*invdz+1
           if ((k.gt.nz).or.(k.lt.1)) then
              write(0,*)"errore box size z"
              write(0,*)xp(jp,1,ip),xp(jp,2,ip),xp(jp,3,ip)
              stop
           end if

           nbox(i,j,k)=nbox(i,j,k)+1
           if (nbox(i,j,k).gt.maxn) then
              write(0,*)"maxn troppo piccolo"
              stop
           end if
           box(nbox(i,j,k),i,j,k)=ip
        enddo
        
        ilev=1
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 if (nbox(i,j,k).eq.0) cycle
                 do ipl=1,nbox(i,j,k)
                    xp_local(1,ipl)=xp(jp,1,box(ipl,i,j,k))*invdx-dble(i-1)
                    xp_local(2,ipl)=xp(jp,2,box(ipl,i,j,k))*invdy-dble(j-1)
                    xp_local(3,ipl)=xp(jp,3,box(ipl,i,j,k))*invdz-dble(k-1)
                 end do
                 call descend(xp_local,nbox(i,j,k),1,jp)
                 icount(jp,1)=icount(jp,1)+1.d0
                 scra=dble(nbox(i,j,k))
                 scram=scra-1.d0
                 do imom=1,nmom
                    scram=scra*scram
                    mom(jp,1,imom)=mom(jp,1,imom)+scram
                 end do
              end do
           end do
        end do
        
     enddo
     
  end do
  
  norm=real(ifr2-ifr1+1)
  do jp=1,ntype
     do ilev=nlev,1,-1
        do imom=1,nmom
           mom(jp,ilev,imom)=mom(jp,ilev,imom)/(norm*dble(npt)**(nmom+1))
        enddo
     enddo
  enddo

  write(nome,"('boxcount.dat')")
  open(unit=100,file=nome,status='unknown')  
  do ilev=nlev,1,-1
     x=dx*0.5d0**(ilev-1)
     write(100,97)real(x),(real(mom(jp,ilev,1)),jp=1,ntype)
  end do
  close(100)
  
  deallocate(box,nbox,xp_local)
  
97 format(24g)

end program boxcount
recursive subroutine descend(xp_local,nlocal,level,jp)
  use global
  implicit none
  real*8 xp_local(3,maxn),xp_down(3,maxn)
  integer nbox_local(0:1,0:1,0:1),box_local(nlocal,0:1,0:1,0:1)
  integer ir,jr,kr,level,ipl,jp
  integer imom
  integer local_count,nlocal
  real*8 scra,scram
  
  nbox_local=0
  box_local=0
  if (level.lt.nlev) then
     do ipl=1,nlocal
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
              icount(jp,level+1)=icount(jp,level+1)+1.d0
              scra=dble(nbox_local(ir,jr,kr))
              scram=scra-1.d0
              do imom=1,nmom
                 scram=scra*scram
                 mom(jp,level+1,imom)=mom(jp,level+1,imom)+scram 
              end do
              call descend(xp_down,nbox_local(ir,jr,kr),level+1,jp)
           end do
        end do
     end do
  end if
  
  return
end subroutine descend
subroutine legge(ifr)
  use global
  implicit none
  real*4, allocatable :: te(:,:),tu(:)
  integer, allocatable :: xplabel(:),xptype(:)
  real*8 :: gp
  integer :: i,j,k,jp,ip,ifr,ipe,s(npt),npl
  character*80 filename
  
  xp=0.d0
  xpu=0.d0
  s=0
  do ipe=0,npe-1
     
     write(filename,"('../Part/part.'i3.3'.'i3.3)")ifr,ipe
     open(unit=11,file=filename,form='unformatted')
     read(11)npl
     allocate(xplabel(npl),xptype(npl),te(ndim,npl),tu(npl))
     read(11)xplabel(1:npl)
     read(11)xptype(1:npl)
     read(11)te(:,1:npl)
     read(11)tu(1:npl)
     close(11)
     
     do j=1,npl
        k=xptype(j)
        s(k)=s(k)+1
        do i=1,ndim
           xp(k,i,s(k))=dble(te(i,j))
           xpu(k,s(k))=dble(tu(j))
        end do
     end do

     deallocate(xplabel,xptype,te,tu)
     
  enddo ! end loop on processors
  
  do jp=1,ntype
     do ip=1,npt
        xp(jp,1,ip)=gp(xp(jp,1,ip),0.d0,xlx)
        xp(jp,2,ip)=gp(xp(jp,2,ip),0.d0,xly)
        xp(jp,3,ip)=gp(xp(jp,3,ip),0.d0,xlz)
     enddo
  enddo
  
  return
end subroutine legge
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
