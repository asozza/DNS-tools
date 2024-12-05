! D2 BOX ASSISTED CODE
! conta le coppie nel box e nei primi vicini
program codim
implicit none
integer, parameter :: np=3200000,tp=5,npt=np/tp,ndim=3,nskip=4
integer, parameter :: nx=128,mt=16,npe=32,npmax=50000,nbin=31
real, parameter :: pi=3.141592d0
real*4, allocatable :: te(:,:),tu(:)
integer, allocatable :: xplabel(:),xptype(:)
real*8, dimension(tp,ndim,np) :: xp
real*8, dimension(tp,np) :: xpu
integer, dimension (mt,mt,mt,npmax) :: label
integer nbox(mt,mt,mt)
real*8 pdf(tp,nbin),pdft(tp,nbin),scra
real*8 rmax,rmin,xmin,xmax,ratio,rd(nbin),r,x,xr,yr,zr,lx,lx2,dx
integer i,j,k,ifr,t,ipe,s(tp),npl
integer ip,jp,ip1,ip2,np1,np2,ib,jb
integer im,jm,km,im2,jm2,km2,sx,sy,sz
integer, parameter :: ifr1=20,ifr2=192,nfr=9
character*80 filename

lx=2*pi
lx2=pi
rmax=2.*pi/mt
ratio=10**(0.1)
rmin=rmax/ratio**(dble(nbin-1.))
dx=dlog(ratio)
xmin=dlog(rmin)
xmax=dlog(rmax)

do ib=1,nbin
   rd(ib)=rmin*ratio**(ib-1.)
enddo

pdf=0.d0
do ifr=ifr2,ifr2+nfr-1

   write(6,*)' frame: ',ifr

   if (ifr2.ne.ifr1) then
   write(filename,"('partdim.'i3.3)")ifr-1
   open(unit=21,file=filename,status='unknown')
   do ip=1,nbin
   read(21,*)rd(ip),(pdf(t,ip),t=1,tp)
   enddo
   endif

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

 do t=1,tp

  nbox=0
  do ip=1,npt,nskip
    i=int(xpu(t,ip)/rmax)+1
    j=int(xp(t,2,ip)/rmax)+1
    k=int(xp(t,3,ip)/rmax)+1
    if((i.ge.1).and.(i.le.mt)) then
    nbox(i,j,k)=nbox(i,j,k)+1
    np1=nbox(i,j,k)
    if(np1.gt.npmax) then
    write(6,*)'stop npmax'
    stop 
    endif
    label(i,j,k,np1)=ip
    endif
  end do

  do im=1,mt ! loop sui box
  do jm=1,mt
  do km=1,mt
  np1=nbox(im,jm,km)

   do ip=1,np1
     ip1=label(im,jm,km,ip)

      do sx=-1,1
       im2=im+sx
       if((im2.eq.0).or.(im2.eq.mt+1)) cycle
      do sy=-1,1
       jm2=jm+sy
       if(jm2.eq.0) jm2=mt
       if(jm2.eq.mt+1) jm2=1
      do sz=-1,1
       km2=km+sz
       if(km2.eq.0) km2=mt
       if(km2.eq.mt+1) km2=1

       np2=nbox(im2,jm2,km2)
       do jp=1,np2 ! cerca le coppie
        ip2=label(im2,jm2,km2,jp)

        if(ip1.ne.ip2) then
          xr=xpu(t,ip1)-xpu(t,ip2)
          yr=dabs(xp(t,2,ip1)-xp(t,2,ip2))
          if(yr.gt.lx2) yr=lx-yr
          zr=dabs(xp(t,3,ip1)-xp(t,3,ip2))
          if(zr.gt.lx2) zr=lx-zr
  
          r=dsqrt(xr**2.+yr**2.+zr**2.)
          x=log(r)-xmin
          ib=floor(x/dx)+1
          if (ib.lt.1) ib=0

          do jb=ib+1,nbin
            pdf(t,jb)=pdf(t,jb)+1.d0
          enddo

        endif

      enddo

     enddo 
     enddo
     enddo

   enddo 

  enddo
  enddo
  enddo

 end do

do t=1,tp
!scra=pdf(t,nbin)
 scra=1.d0
 pdft(t,:)=pdf(t,:)/scra
end do

write(filename,"('partdim.',i3.3)")ifr
open(unit=32,file=filename,status='unknown')
do ib=1,nbin
  write(32,99)real(rd(ib)),(real(pdft(t,ib)),t=1,tp)
end do
close(32)

end do

write(6,*)' Done! '

99 format(12g)

end
