!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This fortran program is the cressman interpolation
!It will read the lookup.tab1 and lookup.tab2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine interp_cressman(lati,loni,data,count,lato,lono,datao,nxo,nyo)

  ! interpolates fields to another grid as specified 
  ! by lat., lon. values.
  ! uses a Cressman interpolation with a specified 
  ! search radius 
  ! uses output from lookup.f to determine search points

  parameter(pi=3.14159, num=500)

  integer count,nxo,nyo
  real lati(count),loni(count)
  real lato(nxo,nyo),lono(nxo,nyo)
  real data(count)
  integer ilonlat(num)
  real datao(nxo,nyo)

  open(30,file='lookup.tab1',status='unknown')
  open(31,file='lookup.tab2',status='unknown')

  guess = 10.  ! search radius
  xmiss = 999999
  scale = 1.  !corrected coefficient
  
  !********************************************************
  !reinterpolate to NMC Octagonal grid using Cressman 
  !weights with a specified search radius

  do jj=1,nyo
    write(*,*)"interpolate jj = ", jj 
    do ii=1,nxo

      sum1=0.
      sum2=0.
      ilevs=0

      !read in indices from lookup table

      read(30,*) ilevs
      
      !process indices

      if(ilevs.ne.0.)then
      
        read(31,*) (ilonlat(i),i=1,ilevs)  
        
        do kk=1,ilevs
          i = ilonlat(kk)

          !find distance between NMC point to be interpolated to
          !and original point. 

          dim=sin(lati(i)*pi/180.)*sin(lato(ii,jj)*pi/180.)
          dam=cos(lati(i)*pi/180.)*cos(lato(ii,jj)*pi/180.)
          dam1=lono(ii,jj)-loni(i)
          dam1=cos(dam1*pi/180.)
          dist=dim+dam*dam1
          dist=acos(dist)*110.949
          
          !if distance is .lt. guess km then identify those
          !cases and find the cressman weights

          if(dist.le.guess .and. data(i).ne.xmiss)then
            zpiggy=(guess**2)-(dist**2)
            zpiggy1=(guess**2)+(dist**2)
            sum=zpiggy/zpiggy1
            sum1=sum1+(sum*data(i)) 
            sum2=sum2+sum  
          endif
        enddo
      endif

      !find interpolated value

      if(ilevs.ge.1) then
        datao(ii,jj)=(sum1/sum2)*scale
      else
        write(*,*) 'Insufficient search radius ...exiting'
        datao(ii,jj)=xmiss
      endif
    enddo
  enddo
  close(30) 
  close(31) 

  return
end
