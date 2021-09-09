; Display Z or Vr values with 10 colors.
; POLYFILL routine used to display uncompressed radar data files
; Use bwppi.pro for B/W hardcopy version.
;
pro display_nexrad_compz
DEVICE, DECOMPOSED=0
restore,'iriscol.dat'
lowz = 0. ; dBZ
stepz = 5 ; dBZ increments
infile = '' &  location = ''
month = (['January','February','March',  'April',  'May',     'June', $
          'July',   'August','September','October','November','December'])
pd  = !pi / 180.
OPENR,1,'nexrad_compz.in'
READF,1,infile
CLOSE,1
; open file and read the header information
OPENR,2,infile
READF,2,location,FORMAT='(A4)'
READF,2,year,imonth,day,hour,minute,second,FORMAT='(i4,5(1x,i2))'
READF,2,rbins,rstep,rstart,FORMAT='(i3,1x,f6.3,1x,f6.3)'
   rangemax = rstep * (rbins-1) + rstart
   range = FLTARR(rbins) & FOR I=0,rbins-1 do range(I) = rstep * I + rstart
tvlct,r,g,b
window,1,xsize=700,ysize=700,title='PPI data'
mx=rangemax + fix(rangemax/20)
ra = FLTARR(rangemax) & th = FLTARR(rangemax)
PLOT,ra,th,XRANGE=[-1.*mx,mx],YRANGE=[-1.*mx,mx],/NODATA,xstyle=1,ystyle=1 $
 ,xtitle='West-East (km)',ytitle='South-North (km)',pos=[0.2,0.2,0.8,0.8] $
 ,background=255,color=0
ly=fltarr(4) & lx=fltarr(4)
FOR i=1,10 DO BEGIN
   ly(0) = -1.25 * mx & lx(0) = mx * (i-6)/5.
   ly(1) = -1.35 * mx & lx(1) = mx * (i-6)/5.
   ly(2) = -1.35 * mx & lx(2) = mx * (i-5)/5.
   ly(3) = -1.25 * mx & lx(3) = mx * (i-5)/5.
   POLYFILL,lX,lY,COLOR=i
      label = STRING(lowz+stepz*(I-1), FORMAT='(I3)') ; lower justified
   XYOUTS,(lx(1)+lx(2))/2.,(ly(0)+ly(1))/2.-0.02*mx,label, $
     ALIGNMENT=0.5,CHARSIZE=1.5,color=0
ENDFOR
   label = 'Composite Reflectivity [dBZ]'
XYOUTS,0.,-1.5*mx,label,ALIGNMENT=0.5,CHARSIZE=2,color=0
raydat = FLTARR(rbins)
num_az=360
PRINT,'Reading data...'
   data = FLTARR(rbins,num_az)
   cdata = intARR(rbins,num_az)
   azdata = findgen(num_az)+1
;
;  loop through and read the num_azual number of rays
   FOR J=0,num_az-1 DO BEGIN
      READF,2,raydat
      data(0:rbins-1,J) = raydat
         FOR K = 0,rbins-1 DO BEGIN
            color = 1 + FIX(data(K,J) -lowz) / stepz
;           color = FIX(data(K,J) -lowz - stepz) / stepz
;        Whiteout (color 15) missing data
;         color = color*(data(K,J) NE -99.0) + 15*(data(K,J) EQ -99.0)
          color = color*(data(K,J) GE lowz) + 15*(data(K,J) LT lowz)
            cdata(K,J)=color < 15
         END ; FOR K=0,rbins-1
   ENDFOR ; J=0,num_az
CLOSE,2
; plot data
dcolor=intarr(rbins,num_az)
dcolor=cdata(*,*)
gap=fltarr(2)
x=fltarr(4) & y=fltarr(4)
for j=0,num_az-2 do begin
 m=j+1
 n=j-1
 u=num_az-1
 if m gt u then m=m-u-1
 if n lt 0 then n=n+u+1
 gap(0)=(azdata(m)-azdata(j))/2.
 if gap(0) lt 0 then gap(0)=(360.+azdata(m)-azdata(j))/2.
 gap(1)=(azdata(j)-azdata(n))/2.
 if gap(1) lt 0 then gap(1)=(360.+azdata(j)-azdata(n))/2.
 az=azdata(j)*pd
  for k=0,rbins-1 do begin
         x(0) = (range(K) + rstep/2.) * SIN(az-gap(1)*pd)
         x(1) = (range(K) + rstep/2.) * SIN(az+gap(0)*pd)
         x(2) = (range(K) - rstep/2.) * SIN(az+gap(0)*pd)
         x(3) = (range(K) - rstep/2.) * SIN(az-gap(1)*pd)
         y(0) = (range(K) + rstep/2.) * COS(az-gap(1)*pd)
         y(1) = (range(K) + rstep/2.) * COS(az+gap(0)*pd)
         y(2) = (range(K) - rstep/2.) * COS(az+gap(0)*pd)
         y(3) = (range(K) - rstep/2.) * COS(az-gap(1)*pd)
     POLYFILL,X,Y,COLOR=dcolor(k,j)
  endfor
endfor
; plot left-over radial that straddles 0 degrees azimuth
 az=azdata(num_az-1)*pd
  for k=0,rbins-1 do begin
         x(0) = (range(K) + rstep/2.) * SIN(az-pd)
         x(1) = (range(K) + rstep/2.) * SIN(az+pd)
         x(2) = (range(K) - rstep/2.) * SIN(az+pd)
         x(3) = (range(K) - rstep/2.) * SIN(az-pd)
         y(0) = (range(K) + rstep/2.) * COS(az-pd)
         y(1) = (range(K) + rstep/2.) * COS(az+pd)
         y(2) = (range(K) - rstep/2.) * COS(az+pd)
         y(3) = (range(K) - rstep/2.) * COS(az-pd)
     POLYFILL,X,Y,COLOR=dcolor(k,num_az-1)
   endfor
oplot,[-1.*mx,mx],[0,0],thick=0.5,color=0
oplot,[0,0],[-1.*mx,mx],thick=0.5,color=0
; range rings every 100 km
theta=indgen(361) * pd
radcir=fltarr(fix(rangemax/100.),361)
for i=0,fix(rangemax/100)-1 do begin
   radcir(i,*) = (i+1)*100.
   oplot, /polar, radcir(i,*), theta, thick=0.5,color=0
endfor
;
; radials every 30 degrees
tic10 = fltarr(2)
tic10(1)=mx/cos(30.*pd)
tic10(0)=mx/sin(60.*pd)
theta=fltarr(4)
   theta(0) = 30.0 * pd
   theta(1) = 60.0 * pd
   theta(2) = 120.0 * pd
   theta(3) = 150.0 * pd
   oplot, /polar,[tic10(0),tic10(0)], [theta(0),theta(0)+180.*pd],thick=0.5,color=0
   oplot, /polar,[tic10(0),tic10(0)], [theta(3),theta(3)+180.*pd],thick=0.5,color=0
   oplot, /polar,[tic10(1),tic10(1)], [theta(1),theta(1)+180.*pd],thick=0.5,color=0
   oplot, /polar,[tic10(1),tic10(1)], [theta(2),theta(2)+180.*pd],thick=0.5,color=0
label =STRING(year,FORMAT='(I4)') + ' ' + month(imonth-1) + $
       STRING(day,FORMAT='(I3)') + STRING(hour,FORMAT='(I3)') + ':' + $
       STRING(minute,FORMAT='(I2.2)') +':'+ STRING(second,FORMAT='(I2.2)') $
       + ' UTC'
XYOUTS,-0.98*mx,1.17*mx,label,CHARSIZE=2,color=0
xyouts,-0.98*mx,1.05*mx,'100 km Range Rings',charsize=2,color=0
xyouts,-0.98*mx,1.29*mx,'WSR-88D '+location,charsize=2,color=0
END
