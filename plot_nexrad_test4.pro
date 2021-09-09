;#==============================================================
;#
;#
;#==-FNMOC/N38DI PYTHON PROGRAM DEFINITION-==========================================
;#
;# NAME:
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#              plot_nexrad_level2.pro
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


;========================================================================================
;========================================================================================
;
;
;==-FNMOC/N34DI IDL PROGRAM DEFINITION-==========================================
;
;  NAMES:  COMPUTE_PROCEDURE_RUNTIME
;
;  PROGRAM OVERVIEW:
;               (1) Computes the total wall clock time needed to run a program.
;               (2) Assumes that you keep track of the start and end time.
;------------------------------------------------------------------------------
; PARAMETER TABLE:
;------------------------------------------------------------------------------
;
; I/O           NAME              TYPE       FUNCTION
;------------------------------------------------------------------------------
;  I            TIME_START        STRING     COMES from IDL -SYSTIME()-
;  I            TIME_END          STRING     COMES from IDL -SYSTIME()-
;  O            pHOURS            INT
;  O            pMINUTES          INT
;  O            pSECONDS          INT
;_______________________________________________________________________________
;==============================================================================
;
;==============================================================================
;-
;
; Programmer: Mr. Paul McCrone     31 August 2009
;
; Modified  : NONE
;==============================================================================
;  NOTE: THIS PROGRAM ASSUMES THE USE OF IDL version 7.0 for RHEL.
;        Linux lx37.uc.nps.edu 2.4.21-40.ELsmp #1 SMP Thu Feb 2 22:22:39 EST 2006 i686 i686 i386 GNU/Linux
;
;        It should work in other UNIX/LINUX when O/S specific spawns are changed.
;
;---------------------------------------------------------------
;
;
PRO COMPUTE_PROCEDURE_RUNTIME, TIME_START, TIME_END, pHOURS, pMINUTES, pSECONDS

        PRINT, ' '
        PRINT, '---------------------------------------------------'
        PRINT, 'EXECUTING PROCEDURE ---COMPUTE_PROCEDURE_RUNTIME---'
        PRINT, '---------------------------------------------------'
        PRINT, ' '

 ;---------------------------------------------------------------------------------------
 ;
 ; This procedure/subroutine will compute the amount on time needed by the system to
 ; perform the computations. It assumes that you are feeding in the start time and
 ; end time in the format shown below from the IDL 'systime' function:
 ;
 ;                ;=======================================================
 ;                ;IDL> TIME_1= systime()
 ;                ;IDL> help, TIME_1,/str
 ;                ;TIME_1          STRING    = 'Thu Feb  7 17:14:50 2008'
 ;                ;=======================================================
 ;                ;00000000001111111111222222
 ;                ;01234567890123456789012345
 ;                ;Thu Feb  7 17:14:50 2008
 ;                ;=======================================================
 ;
 ; I am going to assume that the procedure will NOT take more than 1 day to run. So I
 ; will not bother to read in the date.  However, I will read in the hours/min/sec
 ; of both the start and end time strings. If there is a situation where the procedure
 ; starts at 23:59:00 and ends at 00:03:00, I will add 24 hours to the end time.
 ; Then, I will convert start/end times into total seconds.  Then I will determine the
 ; difference in seconds, then convert that difference back to total hours, minutes, and seconds.
 ;
 ;---------------------------------------------------------------------------------------
 ;---------------------------------------------------------------------------------------

        start_hours_string = STRMID(TIME_START, 11,2)
          end_hours_string = STRMID(TIME_END,   11,2)

        start_min_string   = STRMID(TIME_START, 14,2)
          end_min_string   = STRMID(TIME_END,   14,2)


        start_sec_string   = STRMID(TIME_START, 17,2)
          end_sec_string   = STRMID(TIME_END,   17,2)

        start_hours = FLOAT(start_hours_string)
          end_hours = FLOAT(end_hours_string)

          start_min = FLOAT(start_min_string)
            end_min = FLOAT(end_min_string)

          start_sec = FLOAT(start_sec_string)
            end_sec = FLOAT(end_sec_string)

        IF (end_hours LT start_hours) THEN end_hours=end_hours+24.0
        total_start_seconds = 0
        total_start_seconds = (start_hours*3600.0)+(start_min*60.0)+start_sec

        total_end_seconds = 0
        total_end_seconds = (end_hours * 3600.0) +(end_min*60.0) + end_sec

        total_diff_seconds=total_end_seconds - total_start_seconds

        IF (total_diff_seconds LT 0) THEN BEGIN
                PRINT, '--error---error--error--error--'
                PRINT, 'There was an error computing processing time. '
        ENDIF

        PRINT, ' '
        PRINT, '----------------------------------------'
        PRINT, 'TOTAL SECONDS OF TIME NEEDED TO PROCESS:'
        PRINT, '----------------------------------------'
         HELP, total_diff_seconds,/str
                pHOURS = LONG(total_diff_seconds/3600.0)

        remaining_secs = total_diff_seconds - (3600.0*pHOURS)

              pMINUTES = LONG(remaining_secs/60.0)

        remaining_secs = total_diff_seconds - (3600.0*pHOURS) - (60.0*pMINUTES)

              pSECONDS = LONG(remaining_secs/1.0)

        PRINT, ' '
        PRINT, ' '
        PRINT, '----------------------------------------'
        PRINT, '----------------------------------------'
        PRINT, 'TOTAL PROCESSING TIME FROM START TO END:  '
        PRINT, '----------------------------------------'
        PRINT, 'HRS: ',pHOURS
        PRINT, 'MIN: ',pMINUTES
        PRINT, 'SEC: ',pSECONDS
        PRINT, '----------------------------------------'
        PRINT, ' '
        PRINT, ' '
        PRINT, ' '
        PRINT, '---------------------------------------------------'
        PRINT, 'COMPLETE: PROCEDURE ---COMPUTE_PROCEDURE_RUNTIME---'
        PRINT, '---------------------------------------------------'
        PRINT, ' '

RETURN
END

;#========================================================================================
;#========================================================================================
;#========================================================================================

;#==-FNMOC/N38DI PYTHON PROGRAM DEFINITION-==========================================
;#
;# NAME:
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#              plot_nexrad_level2.pro
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#
;#========================================================================================
;
;
;---------------------------------------------------------------
;---------------------------------------------------------------
; Display Z or Vr values with 10 colors.
; POLYFILL routine used to display uncompressed radar data files
; Use bwppi.pro for B/W hardcopy version.
;
;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;
;
; START MAIN IDL PROCEDURE
;
;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;-;
;
PRO plot_nexrad_test4

;

TIME_START= systime()
TIME_END= systime()

help, TIME_START ,/str

PRINT, "The procedure started at:",TIME_START


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

COLOUR_PATH='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/colour/'

MAP_PATH='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/SITE_MAPS/'

SITE_DIR='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/SITE_DATA'

SITEdatafile=SITE_DIR+'/CURRENT_SITE.dat'

radarsite=READ_CSV(SITEdatafile)

help,radarsite,/str

call_sign=radarsite.FIELD1

location_name=radarsite.FIELD2

ctr_long=radarsite.FIELD3

ctr_lat=radarsite.FIELD4

MAP_FILE=MAP_PATH+STRING(call_sign)+'_MAP_ARRAY.sav'

PRINT, dadash
PRINT, 'The MAP_FILE is:'
PRINT, dadash
PRINT, MAP_FILE
PRINT, dadash+dadash

;
;#============================================================
;


print,dadash
print,"Plot NEXRAD LEVEL II data BEGINS:"
SPAWN,'date',date_info
print,date_info

print,dadash


;#DEVICE, DECOMPOSED=0
restore,'iriscol.dat'
lowz = 0. ; dBZ
stepz = 5 ; dBZ increments
infile = '' &  location = ''

print, "-----------------"
print, "infile "
print, "-----------------"
print, infile
print, "==============================================="
;
month = (['January','February','March',  'April',  'May',     'June', $
          'July',   'August','September','October','November','December'])
;
monthmonth = (['01','02','03','04','05','06','07','08','09','10','11','12'])
;
;

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
;;tvlct,r,g,b,/GET
;tvlct,r,g,b

RESTORE, COLOUR_PATH+'rgb_01.sav'

SAVE_COLOUR_TBL='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL'

;TVLCT, rr, gg, bb, /GET

RESTORE, COLOUR_PATH+'rrggbb_02.sav'

;
;--------------------------------------------------------
; The WINDOW must be instantiated as a /pixmap
; in order to support running from cron/PBS. 
; PJMC 2016-09-26
;--------------------------------------------------------
;
;WINDOW,1,xsize=700,ysize=700,title='PPI data'
;WINDOW,1,xsize=700,ysize=700,title='PPI data',/pixmap
;window_1=!D.WINDOW

;  idl z-buffer device for batch mode (no X windows)
;
  xwindow=700
;
  ywindow=700
;
  zbuff=[xwindow,ywindow]
  SET_PLOT, 'Z'
  DEVICE, SET_RESOLUTION=zbuff
;

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
xyouts,-0.98*mx,1.41*mx, location_name+'- NEXRAD -',charsize=2,color=0
;xyouts,-0.98*mx,1.53*mx, '-Comp. Reflectivity -',charsize=2,color=0

JPG_GRAPHIC=TVRD()

;;------------------------------------------------

;;------------------------------------------------
	;WINDOW,/free,xs=440,ys=440,/pixmap
;
	;window_2=!D.WINDOW
;
nd248=4.139667
;
minlat = ctr_lat - nd248
maxlat = ctr_lat + nd248
minlon = ctr_long- nd248
maxlon = ctr_long+ nd248
llimts=[minlat,minlon,maxlat,maxlon]
	;map_set,ctr_lat, ctr_long, /ORTHOGRAPHIC,/CONTINENTS, LIMIT=llimts,/GRID
	;plots, ctr_long,ctr_lat,PSYM=-2, SYMSIZE=2.5
	;xyouts, ctr_long+.25,ctr_lat+.25,call_sign,CHARTHICK=2,CHARSIZE=2.25
	;MAP_GRAPHIC=TVRD()
;
;;------------------------------------------------
;
	;sMAP_GRAPHIC=MAP_GRAPHIC[5:435,10:420]
	;s2map=congrid(sMAP_GRAPHIC,420,420)
	;tv,s2map
;
;;------------------------------------------------
;
;
	;WINDOW,xsize=700,ysize=700,title='MAP data',/free,/pixmap
;
	;window_4=!D.WINDOW
;
	;TV, s2map, 140,140
	;s3map=TVRD()
;
RESTORE,MAP_FILE
;
;Note: The -MAP_FILE- contains the -s3map- variable used below.
;

JPG_GRAPHIC2=JPG_GRAPHIC
;
;;JPG_GRAPHIC2(WHERE(s3map EQ 255))=1
JPG_GRAPHIC2(WHERE(s3map EQ 255))=0
;
;
	;WINDOW,/free,xs=700,ys=700,/pixmap
;
	;window_3=!D.WINDOW
;
	;tv, JPG_GRAPHIC2
;loadct,39
;loadct,14
;tvlct,rrr,ggg,bbb,/GET
RESTORE, COLOUR_PATH+'rrrgggbbb_04.sav'

;SAVE, rrr, ggg, bbb,FILENAME = 'rrrgggbbb_03.sav'


;tv, JPG_GRAPHIC2
;

;#-------------------------------------------------------------------------------
; This section added by Paul McCrone to read in the completed graphic as save as 
; a jpg file
; PJMC 2016-09-26
;#-------------------------------------------------------------------------------

theday = (['_00_' ,'_01_','_02_','_03_',  '_04_',  '_05_', '_06_','_07_','_08_','_09_','_10_','_11_','_12_', $
          '_13_', '_14_','_15_','_16_','_17_','_18_', '_19_','_20_','_21_','_22_','_23_', '_24_','_25_',  $
          '_26_', '_27_','_28_','_29_','_30_','_31_'])

dayday= (['00' ,'01','02','03',  '04',  '05', '06','07','08','09','10','11','12', $
          '13', '14','15','16','17','18', '19','20','21','22','23', '24','25',  $
          '26', '27','28','29','30','31'])


thehour = (['_00' ,'_01','_02','_03',  '_04',  '_05', '_06','_07','_08','_09','_10','_11','_12', $
          '_13', '_14','_15','_16','_17','_18', '_19','_20','_21','_22','_23'])

hourhour= (['00' ,'01','02','03',  '04', '05', '06','07','08','09','10','11','12', $
          '13', '14','15','16','17','18', '19','20','21','22','23'])


themin = (['00','01','02','03','04','05','06','07','08','09',  $ 
           '10','11','12','13','14','15','16','17','18','19',  $
           '20','21','22','23','24','25','26','27','28','29',  $
           '30','31','32','33','34','35','36','37','38','39',  $
           '40','41','42','43','44','45','46','47','48','49',  $
           '50','51','52','53','54','55','56','57','58','59'])

minmin = (['00','01','02','03','04','05','06','07','08','09',  $
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

secsec = (['00','01','02','03','04','05','06','07','08','09',   $
           '10','11','12','13','14','15','16','17','18','19',   $
           '20','21','22','23','24','25','26','27','28','29',  $
           '30','31','32','33','34','35','36','37','38','39',  $
           '40','41','42','43','44','45','46','47','48','49',  $
           '50','51','52','53','54','55','56','57','58','59'])
;
;
label2 =  STRING(location)+'_'+ STRING(year,FORMAT='(I4)') + '_' + month(imonth-1) + $
          STRING(theday(day)) + STRING(thehour(hour)) + $
          STRING(themin(minute)) + STRING(thesec(second))+'_UTC'
;
;#
;#==========================================================================
;nexrad_KCLL_d20160819_s132800_Z_comp_refl.gif
;#.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
;nexrad_ 	- ..SAME..
;KCLL_   	- RADAR SITE CALL SIGN
;d20160819_	- dYYYYMMDD -- the letter -d- with year-month-day
;s132800_Z_	- sHHMMSS_Z_-- the letter -s- with hour-minunte-second
;comp_refl.gif	- ..SAME..
;#
;#==========================================================================
;#
;
label3 =  STRING('nexrad_')+ STRING(location)+'_d'+ STRING(year,FORMAT='(I4)') + $
          monthmonth(imonth-1) + $
          STRING(dayday(day)) +STRING("_s")+ STRING(hourhour(hour)) + $
          STRING(themin(minute)) + STRING(thesec(second))+'UTC'

JPG_PATH='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/JPG/'
JPG_PATH3='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/GIF_TEST/'

;;--==       PNG_PRJ_IMAGE_ARRAYs = TVRD()
;;--==       WRITE_PNG,FILENAME_HIST_WSPD,PNG_PRJ_IMAGE_ARRAYs


FILENAME_JPG=JPG_PATH+label2+'.jpg'

FILENAME_PNG=JPG_PATH+label2+'.png'

FILENAME_GIF=JPG_PATH+label2+'.gif'
FILENAME_GIF2=JPG_PATH+label2+'_map.gif'
;
FILENAME_GIF3=JPG_PATH3+label3+'_comp_refl.gif'
;
;JPG_GRAPHIC=TVRD()

;;wset,window_3
;wset,window_1

;loadct,39

;TVLCT, rrR, ggG, bbB, /GET
RESTORE, COLOUR_PATH+'rrRggGbbB_03.sav'

;SAVE, rrR, ggG, bbB, FILENAME = 'rrRggGbbB_04.sav'

JPG_GRAPHIC=TVRD()

bblack=0
wwhite=255

;WRITE_PNG,FILENAME_PNG, JPG_GRAPHIC,rrR,ggG,bbB
;WRITE_JPEG,FILENAME_JPG, JPG_GRAPHIC, TRUE =1, QUALITY=100

MY_COLOR_MODIFY=0

IF (MY_COLOR_MODIFY EQ 1) THEN BEGIN

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

ENDIF

;WRITE_GIF,FILENAME_GIF , JPG_GRAPHIC, rrR, ggG, bbB , BACKGROUND_COLOR=bblack
;WRITE_GIF,FILENAME_GIF2 , JPG_GRAPHIC2, rrr, ggg, bbb , BACKGROUND_COLOR=bblack
WRITE_GIF,FILENAME_GIF3 , JPG_GRAPHIC2, rrr, ggg, bbb , BACKGROUND_COLOR=bblack

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

;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------
;=================NEXT TO LAST ITEM        =====================================
;=================DETERMINE PROGRAM RUNTIME=====================================
;=================DO THIS LAST BEFORE PROGRAM ENDS==============================

CLOSEOUTPROGRAM:

;wwd

   PRINT, "The procedure started at:",TIME_START

   TIME_END= systime()

   PRINT, "The procedure ended at  :",TIME_END

   pHOURS   = 0
   pMINUTES = 0
   pSECONDS = 0

   COMPUTE_PROCEDURE_RUNTIME, TIME_START, TIME_END, pHOURS, pMINUTES, pSECONDS

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
;
;
;
;
;
;
;
;
;;
