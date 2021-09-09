c********0*********0*********0*********0*********0*********0*********
c#####################################################################
      subroutine polar_to_polar(azcount,elev)
c#####################################################################
c     Interpolates radar data from a Polar grid to a NEXRAD Level III
c     polar grid and converts full resolution data to Nexrad Level III
c     data levels.
c
c     Author: Paul Harasti October 2003
c
c     implicit none
c
      include 'remap.inc'
c
      integer*2 azcount
      integer elev
      integer i,j,k,m,p,pp,iaz1,iaz2,ir1,ir2,bins(np)
      real x0,y0,x,y,r_factor,pi,ppidata(nrv,np),val,t1,t2,xydis
      real az0,az,az1,az2,azdiff21,azdiff01,azdiff20,ms2kt
      real r0,r1,rbinsize(np),range(nrv,np),rng
      real temp1,temp2,daz,deltaaz,dra2,ave1,ave2,ave3,binsize
      real maxval,minval
      real dummy,sumazdiff,a,el_angle
      real suspect_clutter(nrv),count_data(nrv),ratio_small_vr(nrv)
      logical clockwise

      pi=acos(-1.)
      el_angle=real(the(elev))/100.
      ms2kt=3.6/1.852 
      maxval=-2000.
      minval=2000.
      dummy=10.**(8.)
      sumazdiff=0.
      k=elev
      do i=1,int(azcount*0.1)
      az1=real(phi(i,k))/10.
      if (az1.lt.0.) az1=az1+360.
      az2=real(phi(i+1,k))/10.
      if (az2.lt.0.) az2=az2+360.
      if (az1.lt.10.and.az2.gt.350) az1=360.+az1
      if (az2.lt.10.and.az1.gt.350) az2=360.+az2
      sumazdiff=sumazdiff+(az2-az1)
      enddo
      if (sumazdiff.gt.0.) then
       clockwise=.true.
      else
       clockwise=.false.
      endif
      do i=1,np
       do j=1,nrv
         ppidata(j,i)=dummy
       enddo
        bins(i)=bins_z(k)
        rbinsize(i)=real(ref_gate_size(k))/1000.
      enddo

      do j=1,azcount
      do i=1,bins(j)
         range(i,j)=range_z(i,j,k)
c        print*,range(i,j),i,j,k
         if (gridref(i,j,k).eq.refmis) then
          ppidata(i,j)=dummy
         else
          ppidata(i,j)=10.**(gridref(i,j,k)/100.)
         endif
      enddo
      enddo


      do i=1,max_r
       do j=1,max_a
        gridpolar(i,j)=dummy
       enddo
      enddo
      do i=1,max_r
       r0=real(i) 
       do j=1,max_a
        az0=real(j)
        if (az0.eq.360.) az0=0.
        iaz2=0
        do m=2,azcount
         az1=real(phi(m-1,k))/10.
         az2=real(phi(m,k))/10.
         if (az1.lt.0.) az1=az1+360.
         if (az1.ge.360.) az1=az1-360.
         if (az2.lt.0.) az2=az2+360.
         if (az2.ge.360.) az2=az2-360.
         if (clockwise) then 
          if (az2.ge.az0.and.az1.le.az0) iaz2=m
          if (az2.lt.10.and.az1.gt.350.) then
           if (az0.lt.10.) then
           if (az2.ge.az0.and.az1.le.(360.+az0)) iaz2=m
           endif
           if (az0.gt.350.) then
           if ((360.+az2).ge.az0.and.az1.le.az0) iaz2=m
           endif 
          endif
         else
          if (az1.ge.az0.and.az2.le.az0) iaz2=m
          if (az1.lt.10.and.az2.gt.350.) then
           if (az0.lt.10.) then
           if (az1.ge.az0.and.az2.le.(360.+az0)) iaz2=m
           endif
           if (az0.gt.350.) then
           if (az2.le.az0.and.(360.+az1).ge.az0) iaz2=m
           endif 
          endif
         endif
        enddo
        if (iaz2.gt.1) iaz1=iaz2-1
c
c    Special case: If scan exactly spans 360 degrees,
c    then need to join first and last ray in display.
c
         m=1
         az1=real(phi(azcount,k))/10.
         az2=real(phi(m,k))/10.
         if (az1.lt.0.) az1=az1+360.
         if (az1.ge.360.) az1=az1-360.
         if (az2.lt.0.) az2=az2+360.
         if (az2.ge.360.) az2=az2-360.
         if (clockwise) then 
          if (az2.ge.az0.and.az1.le.az0) iaz2=m
          if (az2.lt.10.and.az1.gt.350.) then
           if (az0.lt.10.) then
           if (az2.ge.az0.and.az1.le.(360.+az0)) iaz2=m
           endif
           if (az0.gt.350.) then
           if ((360.+az2).ge.az0.and.az1.le.az0) iaz2=m
           endif 
          endif
         else
          if (az1.ge.az0.and.az2.le.az0) iaz2=m
          if (az1.lt.10.and.az2.gt.350.) then
           if (az0.lt.10.) then
           if (az1.ge.az0.and.az2.le.(360.+az0)) iaz2=m
           endif
           if (az0.gt.350.) then
           if (az2.le.az0.and.(360.+az1).ge.az0) iaz2=m
           endif 
          endif
         endif
        if (iaz2.eq.1) iaz1=azcount
c
c end of special case.
c
        if (iaz2.eq.0) goto 10
        az1=real(phi(iaz1,k))/10.
        if (az1.lt.0.) az1=az1+360.
        az2=real(phi(iaz2,k))/10.
        if (az2.lt.0.) az2=az2+360.
        azdiff21=abs(az2-az1)
        if (azdiff21.gt.350.) then
        if (az1.lt.10.and.az2.gt.350) azdiff21=360.+az1-az2
        if (az2.lt.10.and.az1.gt.350) azdiff21=360.+az2-az1
        endif
        if (azdiff21.lt.180..and.azdiff21.gt.4.0) goto 10
        azdiff01=abs(az0-az1)
        if (az0.lt.10.and.az1.gt.350) azdiff01=360.+az0-az1
        if (az1.lt.10.and.az0.gt.350) azdiff01=360.+az1-az0
        azdiff20=abs(az2-az0)
        if (az0.lt.10.and.az2.gt.350) azdiff20=360.+az0-az2
        if (az2.lt.10.and.az0.gt.350) azdiff20=360.+az2-az0
        daz=azdiff01
        deltaaz=azdiff01+azdiff20
         rng=range(bins(iaz1),iaz1)
         if (r0.gt.rng) goto 10
         rng=range(bins(iaz2),iaz2)
         if (r0.gt.rng) goto 10
        if (bins(iaz1).ne.bins(iaz2)) then
         if (rbinsize(iaz1).ne.rbinsize(iaz2)) goto 10
         rng=range(bins(iaz1),iaz1)
         if (rng.lt.(r0+rbinsize(iaz1))) goto 10
         rng=range(bins(iaz2),iaz2)
         if (rng.lt.(r0+rbinsize(iaz2))) goto 10
        endif 
        if (azdiff21.gt.4..or.azdiff01.gt.4..or.azdiff20.gt.4.) 
     +print*,"here",azdiff21,azdiff01,azdiff20
        ir1=0
        ir2=0
        do m=1,bins(iaz1)
         rng=range(m,iaz1)
         if (rng.lt.r0) ir1=m
         if (ir1.gt.0) ir2=ir1+1
        enddo
        if (ir2.eq.0) goto 10
        r1=range(ir1,iaz1)
        if (ir2.eq.1) then
c average across azimuth on gate ngate+1 and elev nh
           temp1=ppidata(ir2,iaz1)
           temp2=ppidata(ir2,iaz2)
           call ave(temp1,temp2,daz,deltaaz,ave2,dummy)
           gridpolar(i,j)=ave2
        endif
        if (ir2.gt.bins(iaz1).and.ir1.eq.bins(iaz1)) then
c average across azimuth on gate ngate and elev nh
           temp1=ppidata(ir1,iaz1)
           temp2=ppidata(ir1,iaz2)
           call ave(temp1,temp2,daz,deltaaz,ave1,dummy)
           gridpolar(i,j)=ave1
        endif
        if (ir1.gt.0.and.ir2.le.bins(iaz1)) then
         dra2=r0-r1
c average across azimuth on gate ngate and elev nh
           temp1=ppidata(ir1,iaz1)
           temp2=ppidata(ir1,iaz2)
           call ave(temp1,temp2,daz,deltaaz,ave1,dummy)
c average across azimuth on gate ngate+1 and elev nh
           temp1=ppidata(ir2,iaz1)
           temp2=ppidata(ir2,iaz2)
           call ave(temp1,temp2,daz,deltaaz,ave2,dummy)
c average across range
           binsize=rbinsize(iaz1)
           call ave(ave1,ave2,dra2,binsize,ave3,dummy)
c assign value to the grid
           gridpolar(i,j)=ave3
        endif
       val=gridpolar(i,j)
       if (val.ne.dummy.and.val.gt.maxval) maxval=real(gridpolar(i,j))
       if (val.ne.dummy.and.val.lt.minval) minval=real(gridpolar(i,j))
10    continue
      enddo
      enddo
  
      print*,""
      print*,"PPI Tilt Number = ", k
      print*,"Elevation Angle (degrees) = ",el_angle
       if (maxval.ne.-2000.) then
        print*,"Maximum Reflectivity (dbZ) = ",10.*alog10(maxval)
       else
        print*,"Maximum Reflectivity  = ","no data"
       endif
       if (minval.ne.2000.) then
        print*,"Minimum Reflectivity (dBZ) = ",10.*alog10(minval)
       else
        print*,"Minimum Reflectivity  = ","no data"
       endif

      RETURN
      END
