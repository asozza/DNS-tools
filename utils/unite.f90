! prende campo parallelo e lo rende seriale
program unite
  implicit none
  integer, parameter :: nx=256,ny=nx,nz=nx
  integer, parameter :: npe=64,nzl=nz/npe
  real*8, dimension(nx,ny,nz) :: te
  real*4, allocatable :: z4(:,:,:)
  integer :: ifr,ipe,i,j,k,kk
  character*80 :: arg,filename
  
  CALL getarg(1,arg)
  if (arg=="") then
     write(6,*)' Error! No Frame Selected! '  
     write(6,*)' Usage: ./a.out ifr '
     stop
  else
     read(arg,*)ifr
  endif
  
  te=0.d0
  do ipe=0,NPE-1
     
     allocate(z4(nx,ny,nzl))
     write(filename,"('rho.',i3.3,'.',i3.3)")ifr,ipe
     open(unit=1,file=filename,action='read',form='unformatted')
     read(1)z4
     close(1)  
     do k=1,nzl
        kk=k+ipe*nzl
        do j=1,ny
           do i=1,nx
              te(i,j,kk)=dble(z4(i,j,k))
           end do
        end do
     end do
     deallocate(z4)
     
  enddo
  
  ! write reunited fields
  write(filename,'("./New/rho."i3.3)')ifr
  open(1,file=filename,form="unformatted")
  write(1)real(te)
  close(1)
  
end program unite
