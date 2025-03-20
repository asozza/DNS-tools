! increase grid resolution by a factor 2
program change_res
  implicit none
  
  integer, parameter :: nx=256,ny=nx,nz=nx
  integer, parameter :: npe=128,nzl=nz/npe
  integer, parameter :: nx2=nx*2,ny2=ny*2,nz2=nz*2
  integer, parameter :: nzl1=nzl+1,nzl2=nzl*2
  real*4, allocatable :: z4(:,:,:),te(:,:,:)
  integer :: ifr,ipe,ipe1,i,j,k,i1,j1,k1,is,js,ks
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
  
  do ipe=0,npe-1
     
     allocate(z4(nx,ny,nzl),te(nx2,ny2,nzl2))
     write(filename,'("./Frames/",a,"."i3.3"."i3.3)')trim(str),ifr,ipe
     open(unit=1,file=filename,action='read',form='unformatted')
     read(1)z4
     close(1)  
     do k=1,nzl
        do ks=1,2
           do j=1,ny
              do js=1,2
                 do i=1,nx
                    do is=1,2
                       te(2*(i-1)+is,2*(j-1)+js,2*(k-1)+ks)=z4(i,j,k)              
                    end do
                 end do
              end do
           enddo
        enddo
     enddo
     deallocate(z4)
     
     ipe1=2*ipe
     write(filename,'("./New/",a,"."i3.3"."i3.3)')trim(str),ifr,ipe1
     open(1,file=filename,action='write',form="unformatted")
     write(1)te(:,:,1:nzl)
     close(1)
     ipe1=2*ipe+1
     write(filename,'("./New/",a,"."i3.3"."i3.3)')trim(str),ifr,ipe1
     open(1,file=filename,action='write',form="unformatted")
     write(1)te(:,:,nzl1:nzl2)
     close(1)
     
     deallocate(te)
     
  enddo
  
end program change_res
