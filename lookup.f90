!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This fortran program is to generate two files(lookup.tab1 and lookup.tab2)
!lookup.tab1 records the amount of input points that each output point needs
!lookup.tab2 records the data of input points that each output point needs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cressman_lookup(lati,loni,data,count,lato,lono,nxo,nyo)

  !creates lookup table for cressman interpolation routine

  parameter(pi=3.14159, num=500)

  integer count,nxo,nyo
  real lati(count),loni(count)
  real lato(nxo,nyo),lono(nxo,nyo)
  real data(count)
  integer ilonlat(num)

  guess = 10.  ! search radius, the unit is km
  xmiss = 999999 ! missing value

  open(30,file='lookup.tab1',status='unknown')
  open(31,file='lookup.tab2',status='unknown')

  !reinterpolate to NMC Octagonal grid using cressman
  !weights with a specified search radius

  do jj=1,nyo
    write(*,*)"cressman_lookup jj = ", jj
    do ii=1,nxo

      sum1=0
      sum2=0
      ilevs=0 ! the amount of input points that each output point needs

      do i = 1, count
        ! find distance between NMC point to be interpolated to
        ! and original point. 

        dim=sin(lati(i)*pi/180.)*sin(lato(ii,jj)*pi/180.)
        dam=cos(lati(i)*pi/180.)*cos(lato(ii,jj)*pi/180.)
        dam1=lono(ii,jj)-loni(i)
        dam1=cos(dam1*pi/180.)
        dist=dim+dam*dam1
        dist=acos(dist)*110.949

!write(*, *)loni(i), " === ", lati(i), " === ", lono(ii, jj), " === ", lato(ii, jj), " === ", dist

        ! if distance is .lt. guess km then identify those
        ! cases and create lookup table
        
        if(dist.le.guess .and. data(i).ne.xmiss) then
          ilevs=ilevs+1
          ilonlat(ilevs)=i
        endif
      enddo
        
      write(30,*) ilevs
      if(ilevs.ne.0)then
        write(31,*) (ilonlat(i),i=1,ilevs)
      endif
    enddo
  enddo
  close(30)
  close(31)

  return
end
