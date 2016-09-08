!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This fortran program is the main function of the cressman interpolation
!It will call other two programs(cressman_lookup and interp_cressman)
!You can change the input data's dimension, 
!like scattered points interpolate to grid points
!or grid points interpolate to grid points with high resolution.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

parameter (vm=999999)
parameter (nx2=526,ny2=426)

!data is the input file content.
!slat1 is the station's latitude.
!slon1 is the station's longitude.
!dat1 is the station's observation value.
real, allocatable :: data(:, :), slat1(:), slon1(:),dat1(:)
!dat2 is the grid value what we want.
!slat2 is the grid latitude what we want.
!slon2 is the grid longitude what we want.
real dat2(nx2,ny2) 
real slat2(nx2,ny2),slon2(nx2,ny2)
!count is the amount of the stations.
!stat is the file's status.
integer stat,count
character*20 title
stat = 0

! INPUT DATA
count = 0
open(10, file = 'PP20160329195557.txt', status = 'old')

do while(.true.)
  read(10, *, iostat = stat)
  count = count + 1
  if(stat /= 0)exit
enddo

allocate(data(count-2, 4))
allocate(slon1(count-2))
allocate(slat1(count-2))
allocate(dat1(count-2))

rewind(10)
read(10, *)title
read(10,*)((data(i,j), j = 1, 4),i = 1, count-2)
close(10)

slon1 = data(:,2)
slat1 = data(:,3)
dat1 = data(:,4)

! OUTPUT DATA                           
do i = 1, 526
  do j = 1, 426
    slon2(i, j) = 73 + 0.12 * (i-1)
    slat2(i, j) = 3 + 0.12 * (j-1)
  enddo 
enddo
write(*,*)"The amount of the stations is ",count

call cressman_lookup(slat1,slon1,dat1,count-2,slat2,slon2,nx2,ny2)
call interp_cressman(slat1,slon1,dat1,count-2,slat2,slon2,dat2,nx2,ny2)

open(20,file='PP20160329195557Format.txt', form='formatted',status='unknown')
do i = 1, 526
  do j = 1, 426
    write(20,'(F10.3)') dat2(i, j)
  enddo
enddo
close(20)

end
