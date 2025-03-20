! change the number of processors/slices
program change_proc
  implicit none

  integer, parameter :: nx=256,ny=nx,nz=nx
  integer, parameter :: npe1=64,nzl1=nz/npe1
  integer, parameter :: npe2=128,nzl2=nz/npe2
  real*8, dimension(nx,ny,nz) :: te
  real*8, dimension(nx,ny,nzl2) :: tel
  real*4, allocatable :: z4(:,:,:)
  integer :: ifr,ipe,i,j,k,kk,k1,k2
  character*80 :: arg,filename
  character*5 :: str
  
  CALL getarg(1,arg)
  if (arg=="") then
     write(6,*)' Error! No Frame Selected! '  
     write(6,*)' Usage: ./a.out frame fieldname '
     stop
  else
     read(arg,*)ifr
  endif
  
  CALL getarg(2,str)
  
  te=0.d0
  do ipe=0,npe1-1     
     allocate(z4(nx,ny,nzl1))
     write(filename,'("./Frames/",a,"."i3.3"."i3.3)')trim(str),ifr,ipe
     open(unit=1,file=filename,action='read',form='unformatted')
     read(1)z4
     close(1)  
     do k=1,nzl1
        kk=k+ipe*nzl1
        do j=1,ny
           do i=1,nx
              te(i,j,kk)=dble(z4(i,j,k))
           end do
        end do
     end do
     deallocate(z4)
  enddo
  
  do ipe=0,npe2-1
     k1=1+nzl2*ipe
     k2=k1+nzl2-1
     tel=te(:,:,k1:k2)
     write(filename,'("./New/",a,"."i3.3"."i3.3)')trim(str),ifr,ipe
     open(1,file=filename,form="unformatted")
     write(1)real(tel)
     close(1)
  end do
  
end program change_proc
