! Box-assisted algorithm
program boxcount
  implicit none
  
  ! constants
  real*8, parameter :: pi=2.d0*asin(1.d0)
  integer, parameter :: ndim=3
  real*8, parameter :: xlx=2.d0*pi,xly=xlx,xlz=xlx
  ! parameters
  integer, parameter :: ifr1=6,ifr2=6,ndep=10
  integer, parameter :: nx=256,ny=nx,nz=nx,nr=nx/2  
  ! variables
  real*8, allocatable :: xp(:,:)
  real*4, allocatable :: dumxp(:,:)
  real*8, dimension(nr) :: g,f,dg
  real*8 :: dxp(ndim),y,r,dr,r1,r2,rmin,rmax,scra,norm
  integer :: ifr,idep,nptot,npl
  integer :: i,j,k,ip,jp,ipl,ir,jr,icount
  character*80 :: filename

  ! initiate
  nptot=0
  icount=0
  g=0.d0
  f=0.d0

  ! define extrema
  rmin=xlx/dble(nr)
  rmax=xlx
  dr=dlog(rmax/rmin)/dble(nr-1)
  
  do ifr=ifr1,ifr2
     do idep=1,ndep
        
        write(filename,"('./Post/iso.'i3.3'.'i3.3)")ifr,idep
        open(unit=11,file=filename,form='unformatted')
        read(11)npl
        write(6,*)npl
        allocate(dumxp(ndim+1,npl),xp(ndim+1,npl))
        read(11)dumxp
        xp=dble(dumxp)
        close(11)
        deallocate(dumxp)  
        
        do ip=1,npl
           do jp=1,npl
              if (ip.eq.jp) cycle
              dxp(:)=dabs(xp(:,ip)-xp(:,jp))
              scra=0.d0
              do i=1,ndim
                 scra=scra+dxp(i)**2.
              enddo
              r=dsqrt(scra)
              y=dlog(r/rmin)
              ir=int(y/dr)+1
              if ((ir.ge.1).and.(ir.le.nr)) then
                 icount=icount+1
                 f(ir)=f(ir)+1.d0
                 do jr=ir,nr
                    g(jr)=g(jr)+1.d0
                 enddo
              endif
           enddo
        enddo
        
        ! sum points
        nptot=nptot+npl
        
        deallocate(xp)
        
     enddo
  enddo

  ! normalization of g(r)
  norm=real(ifr2-ifr1+1)*dble(icount)
  g(:)=g(:)/norm
  
  ! normalization of f(r)
  norm=0.d0
  do ir=1,nr
     norm=norm+dr*f(ir)
  enddo
  f(:)=f(:)/norm

  ! write correlation function
  write(filename,"('./Post/codim_log.dat')")
  open(unit=100,file=filename,status='unknown')  
  do ir=1,nr
     r=rmin*dexp(dr*dble(ir-1))
     write(100,99)real(r),real(g(ir)),real(f(ir))
  end do
  close(100)

  ! logarithmic derivative
  dg=0.d0
  do ir=2,nr-1
     r1=rmin*dexp(dr*dble(ir-1-1))
     r2=rmin*dexp(dr*dble(ir+1-1))
     dg(ir)=(dlog(g(ir+1))-dlog(g(ir-1)))/(dlog(r2)-dlog(r1))
  enddo
  ! write output
  write(filename,"('./Post/d2_log.dat')")
  open(unit=100,file=filename,status='unknown')
  do ir=1,nr
     r=rmin*dexp(dr*dble(ir-1))
     write(100,99)real(r),real(dg(ir))
  end do
  close(100)
  
99 format(4g)

end program boxcount
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
