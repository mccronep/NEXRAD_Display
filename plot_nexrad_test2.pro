;#==============================================================
;#
;#
;#==-FNMOC/N38DI PYTHON PROGRAM DEFINITION-==========================================
;#
;# NAME:
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#              plot_nexrad_level2.pro
;#              ---> plot_nexrad_test2.pro
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#
;#  PROGRAM OVERVIEW:
;#       (1) This program reads in the RAPIDSCAT data from Royal Netherlands Meteorological Institute (KNMI).
;#       (2) The input file is in NEXRAD LEVEL II ASCII format.
;#       (3) The output file is a JPG file.
;#       (4) There are several functions,
;#               -a- -
;#               -b- -
;#
;#--------------------------------------------------------------------------------------------------
;# PARAMETER TABLE:
;#--------------------------------------------------------------------------------------------------
;#
;# I/O           NAME            TYPE            FUNCTION
;#--------------------------------------------------------------------------------------------------
;#  I            NEXRAD LEVEL II input           INPUT DATA FROM 557 WW.
;#  O            ASCII file      output          
;#_________________________________________________________________________________________________
;#=================================================================================================
;#
;#=================================================================================================
;#-
;#
;# Programmer: Dr. Paul Harasti (NRL)   01 JULY 2016 (Original)
;#             Mr. Paul McCrone (FNMOC) 03 AUG  2016 (Maintainer-FNMOC) 
;#
;# Modification History  : BELOW
;#========================================================================================
;#  Version 1.0, Dated 2015-AUG-03
;#               ORIGINAL OPERATIONAL VERSION.
;#
;#
;#========================================================================================
;#  NOTE: THIS PROGRAM ASSUMES THE USE OF IDL Version 8.0
;#---------------------------------------------------------------
;#
;#========================================================================================
;#
;# Data file contents: NEXRAD Level II data files
;#
;#  File naming convention:
;#
;#  YYYYMMDD_HHmmSS_iss____RRRRR_2hr_o_EEE_1903_ovw_l2.nc
;#
;#  WHERE:
;#  ======
;#  YYYY         4-digit year of data file creation date
;#  MM           2-digit month of data file creation date
;#  DD           2-digit day of month of data file creation date
;#  HH           2-digit hour of 24-hour day of data file creation time
;#  mm           2-digit minute of hour of data file creation time
;#  SS           2-digit Seconds of minute of  hour of data file creation time
;#  iss          The International Space Station (ISS)
;#  l2           Level 2
;#  gz           GNU Zip file extension
;#
;#  EXAMPLE:
;#  rapid_20160721_121748_iss____10375_2hr_o_500_1903_ovw_l2.nc
;'#
;'#
;'#========================================================================================
;#
;#

; Display Z or Vr values with 10 colors.
; POLYFILL routine used to display uncompressed radar data files
; Use bwppi.pro for B/W hardcopy version.
;
;pro display_nexrad_compz
;
PRO plot_nexrad_test2
;plot_nexrad_level2
;
dadots='. . . . . . . . . . . . . . . . . . . . . . . . . '
dadash='- - - - - - - - - - - - - - - - - - - - - - - - - '

jpg_path='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/JPG'

;
;#============================================================
;                                                             

;call_sign='KNKX'
call_sign='KXYZ' 
;location_name="San-Diego-CA"
location_name="AAA-BBBBB-CC" 
;ctr_long=-117.041944444     
ctr_long=0.0                 
;ctr_lat=32.9188888889       
ctr_lat=0.0                  

;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------
;                                                                                
; Read the RADAR SITE Configuration data                                         
;                                                                                
;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------
;                                                                                

SITE_DIR='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/SITE_DATA'

SITEdatafile=SITE_DIR+'/CURRENT_SITE.dat'

radarsite=READ_CSV(SITEdatafile)

help,radarsite,/str

call_sign=radarsite.FIELD1

location_name=radarsite.FIELD2

ctr_long=radarsite.FIELD3

ctr_lat=radarsite.FIELD4

;
;#============================================================
;


print,dadash
print,"Plot NEXRAD LEVEL II data BEGINS:"
SPAWN,'date',date_info
print,date_info

print,dadash


DEVICE, DECOMPOSED=0
restore,'iriscol.dat'
lowz = 0. ; dBZ
stepz = 5 ; dBZ increments
infile = '' &  location = ''

print, "-----------------"
print, "infile "
print, "-----------------"
print, infile
print, "==============================================="

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

TVLCT, rr, gg, bb, /GET

;
;--------------------------------------------------------
; The WINDOW must be instantiated as a /pixmap
; in order to support running from cron/PBS. 
; PJMC 2016-09-26
;--------------------------------------------------------
;
WINDOW,1,xsize=700,ysize=700,title='PPI data'
;WINDOW,1,xsize=700,ysize=700,title='PPI data',/pixmap
window_1=!D.WINDOW


;
mx=rangemax + fix(rangemax/20)
ra = FLTARR(rangemax) & th = FLTARR(rangemax)
PLOT,ra,th,XRANGE=[-1.*mx,mx],YRANGE=[-1.*mx,mx],/NODATA,xstyle=1,ystyle=1 $
 ,xtitle='West-East (km)',ytitle='South-North (km)',pos=[0.2,0.2,0.8,0.8] $
 ,background=255,color=0

;# ------------------------------------------------------------------------
;# Start FOR LOOP
;#
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
;#
;#  End of FOR LOOP
;# ------------------------------------------------------------------------


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
xyouts,-0.98*mx,1.41*mx, location_name+'- NEXRAD -'+location,charsize=2,color=0

JPG_GRAPHIC=TVRD()

;------------------------------------------------

;------------------------------------------------
;WINDOW,/free,xs=440,ys=440,/pixmap
WINDOW,/free,xs=440,ys=440

window_2=!D.WINDOW

nd248=4.139667

minlat = ctr_lat - nd248
maxlat = ctr_lat + nd248
minlon = ctr_long- nd248
maxlon = ctr_long+ nd248
llimts=[minlat,minlon,maxlat,maxlon]
map_set,ctr_lat, ctr_long, /ORTHOGRAPHIC,/CONTINENTS, LIMIT=llimts,/GRID
plots, ctr_long,ctr_lat,PSYM=-2, SYMSIZE=2.5
xyouts, ctr_long+.25,ctr_lat+.25,call_sign,CHARTHICK=2,CHARSIZE=2.25
MAP_GRAPHIC=TVRD()

;------------------------------------------------

sMAP_GRAPHIC=MAP_GRAPHIC[5:435,10:420]
s2map=congrid(sMAP_GRAPHIC,420,420)
tv,s2map

;------------------------------------------------


;WINDOW,xsize=700,ysize=700,title='MAP data',/free,/pixmap
WINDOW,xsize=700,ysize=700,title='MAP data',/free

window_4=!D.WINDOW

TV, s2map, 140,140
s3map=TVRD()

JPG_GRAPHIC2=JPG_GRAPHIC

JPG_GRAPHIC2(WHERE(s3map EQ 255))=1


;WINDOW,/free,xs=700,ys=700,/pixmap
WINDOW,/free,xs=700,ys=700

window_3=!D.WINDOW

tv, JPG_GRAPHIC2
loadct,39
tvlct,rrr,ggg,bbb,/GET
tv, JPG_GRAPHIC2


;#-------------------------------------------------------------------------------
; This section added by Paul McCrone to read in the completed graphic as save as 
; a jpg file
; PJMC 2016-09-26
;#-------------------------------------------------------------------------------

theday = (['_00_' ,'_01_','_02_','_03_',  '_04_',  '_05_', '_06_','_07_','_08_','_09_','_10_','_11_','_12_', $
          '_13_', '_14_','_15_','_16_','_17_','_18_', '_19_','_20_','_21_','_22_','_23_', '_24_','_25_',  $
          '_26_', '_27_','_28_','_29_','_30_','_31_'])

thehour = (['_00' ,'_01','_02','_03',  '_04',  '_05', '_06','_07','_08','_09','_10','_11','_12', $
          '_13', '_14','_15','_16','_17','_18', '_19','_20','_21','_22','_23'])

themin = (['00','01','02','03','04','05','06','07','08','09',  $ 
           '10','11','12','13','14','15','16','17','18','19',  $
           '20','21','22','23','24','25','26','27','28','29',  $
           '30','31','32','33','34','35','36','37','38','39',  $
           '40','41','42','43','44','45','46','47','48','49',  $
           '50','51','52','53','54','55','56','57','58','59'])

thesec = (['00_','01_','02_','03_','04_','05_','06_','07_','08_','09_',   $ 
           '10_','11_','12_','13_','14_','15_','16_','17_','18_','19_',   $
           '20_','21_','22_','23_','24_','25_','26_','27_','28_','29_',  $
           '30_','31_','32_','33_','34_','35_','36_','37_','38_','39_',  $
           '40_','41_','42_','43_','44_','45_','46_','47_','48_','49_',  $
           '50_','51_','52_','53_','54_','55_','56_','57_','58_','59_'])

label2 =  STRING(location)+'_'+ STRING(year,FORMAT='(I4)') + '_' + month(imonth-1) + $
          STRING(theday(day)) + STRING(thehour(hour)) + $
          STRING(themin(minute)) + STRING(thesec(second))+'_UTC'

JPG_PATH='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/JPG/'

;;--==       PNG_PRJ_IMAGE_ARRAYs = TVRD()
;;--==       WRITE_PNG,FILENAME_HIST_WSPD,PNG_PRJ_IMAGE_ARRAYs


FILENAME_JPG=JPG_PATH+label2+'.jpg'

FILENAME_PNG=JPG_PATH+label2+'.png'

FILENAME_GIF=JPG_PATH+label2+'.gif'

;JPG_GRAPHIC=TVRD()

wset,window_3

loadct,39

TVLCT, rrR, ggG, bbB, /GET

JPG_GRAPHIC=TVRD()

bblack=0
wwhite=255

;WRITE_PNG,FILENAME_PNG, JPG_GRAPHIC,rrR,ggG,bbB
;WRITE_JPEG,FILENAME_JPG, JPG_GRAPHIC, TRUE =1, QUALITY=100

bbB(165)=255
rrR(165)=25
gg(165)=25
;----------
rrR(1)=25
ggG(1)=25
bbB(1)=255
;===========
rrR(2)=25
ggG(2)=25
bbB(2)=255
;===========
rrR(3)=25
ggG(3)=255
bbB(3)=25
;===========
rrR(4)=25
ggG(4)=225
bbB(4)=25
;===========
rrR(5)=25
ggG(5)=225
bbB(5)=255
;===========
rrR(6)=25
ggG(6)=225
bbB(6)=255
;===========
rrR(7)=125
ggG(7)=125
bbB(7)=255
;===========
rrR(87)=125
ggG(87)=125
bbB(87)=255
;===========
rrR(9)=225
ggG(9)=25
bbB(9)=255
;===========

WRITE_GIF,FILENAME_GIF , JPG_GRAPHIC, rrR, ggG, bbB , BACKGROUND_COLOR=bblack

;FOR KKOLOR = 1, 74  DO BEGIN
;
;    FILENAME_GIF=JPG_PATH+label2+"_CT_"+STRING(KKOLOR)+'.gif'
;    loadct,KKOLOR
;    TVLCT, rrR, ggG, bbB, /GET
;    WRITE_GIF,FILENAME_GIF , JPG_GRAPHIC, rrR, ggG, bbB , BACKGROUND_COLOR=bblack
;
;ENDFOR


;#-------------------------------------------------------------------------------
print,dadash
print,"Plot NEXRAD LEVEL II data ENDS:"
SPAWN,'date',date_info
print,date_info
print,dadash
;#-------------------------------------------------------------------------------

;#
END

;#-------------------------------------------------------------------------------
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--## END OF IDL CODE                                                       
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;#                                                                               
;
;IDL> xyouts,-575,-495,".",color=0                 
;IDL> xyouts,-575,-490,".",color=255               
;IDL> xyouts,-575,-490,"HERE",color=255            
;IDL> xyouts,-575,-490,"HERE",color=2              
;IDL> xyouts,-575,-490,"HERE",color=45             
;IDL> rr                                          
;
;
;IDL> bb(165)=255
;IDL> rr(165)=25
;IDL> gg(165)=25
;IDL> tvlct,rr,gg,bb,/GET
;IDL> tv,image
;
;IDL> image(140,140)=255 ; 00000 
;IDL> image(560,560)=255 ; 00000000000
;
;     The PPI Lower Left  Hand Corner is at 140,140
;     The PPI Upper Right Hand Corner is at 560,560
;
;
;
;
;;
