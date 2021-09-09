;#==============================================================
;#
;#
;#==-FNMOC/N38DI PYTHON PROGRAM DEFINITION-==========================================
;#
;# NAME:
;# :::::::::::::::::::::::::::::::::::::::::::::::
;#             plot_nexrad_test.pro 
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

;
; Micelaneous Procedures
;
;
;==-FNMOC/N34DI IDL PROGRAM DEFINITION-==========================================
;
;  NAMES:   WWD
;
;  PROGRAM OVERVIEW:
;               (1) WWD deletes all graphics windows in IDL.
;------------------------------------------------------------------------------
; PARAMETER TABLE:
;------------------------------------------------------------------------------
;
; I/O           NAME              TYPE       FUNCTION
;------------------------------------------------------------------------------
;  I          IDL Windows         input window ID's
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
;========================================================================================
;

PRO WWD

        PRINT, ' '
        PRINT, '----------------------------------------------'
        PRINT, 'EXECUTING PROCEDURE ---WWD---'
        PRINT, '----------------------------------------------'
        PRINT, ' '

        WHILE (!D.WINDOW ne -1) DO WDELETE

        PRINT, ' '
        PRINT, '----------------------------------------------'
        PRINT, 'PROCEDURE ---WWD--- DONE'
        PRINT, '----------------------------------------------'
        PRINT, ' '

END

;
;========================================================================================
;========================================================================================
;

;
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
;---------------------------------------------------------------
;
;========================================================================================
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





;'#========================================================================================
;'#========================================================================================
;'#========================================================================================
;
; BEGIN THE MAIN PROGRAM
;
;
;
;'#========================================================================================
;'#========================================================================================
;'#========================================================================================

; Display Z or Vr values with 10 colors.
; POLYFILL routine used to display uncompressed radar data files
; Use bwppi.pro for B/W hardcopy version.
;
;pro display_nexrad_compz
;
PRO plot_nexrad_test

TIME_START= systime()
TIME_END= systime()

help, TIME_START ,/str

PRINT, "The procedure started at:",TIME_START

wwd

;-----------------------------------------------------------
; IF WINDOWS_os_yes=1, THEN WE ARE USING MS WINDOWS
; IF WINDOWS_os_yes=0, THEN WE ARE -NOT- USING MS WINDOWS
; 
; IF LINUX_os_yes=0, THEN WE ARE -NOT- USING LINUX
; IF LINUX_os_yes=1, THEN WE ARE USING LINUX
;-----------------------------------------------------------

WINDOWS_os_yes=0
LINUX_os_yes=1

IF (WINDOWS_os_yes EQ 1) THEN BEGIN
    PRINT, 'PROGRAM Set for WINDOWS operating system.'
ENDIF

IF (LINUX_os_yes EQ 1) THEN BEGIN
    PRINT, 'PROGRAM Set for LINUX operating system.'
ENDIF

IF (LINUX_os_yes EQ WINDOWS_os_yes) THEN BEGIN

        PRINT, ' '
        PRINT, '...........................................'
        PRINT, 'ERROR  ERROR ERROR ERROR ERROR ERROR ERROR'
        PRINT, '...........................................'
        PRINT, ' '
        PRINT, 'PROGRAM Set for BOTH LINUX AND WINDOWS operating systems.'
        PRINT, 'INVALID: THIS WILL CAUSE ERRORS. '
        PRINT, 'PROGRAM WILL END NOW: no output will be made. '
        PRINT, ' '
        PRINT, '...........................................'
        PRINT, 'ERROR  ERROR ERROR ERROR ERROR ERROR ERROR'
        PRINT, '...........................................'
        PRINT, ' '

        GOTO ,CLOSEOUTPROGRAM

ENDIF

;-----------------------------------------------------------
;-----------------------------------------------------------

;
;  Originally --plot_nexrad_level2--
;
dadots='. . . . . . . . . . . . . . . . . . . . . . . . . '
dadash='- - - - - - - - - - - - - - - - - - - - - - - - - '

jpg_path='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/JPG'


print,dadash
print,"---plot_nexrad_orig.pro---"
print,dadash

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
print, "infile is: "
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
tvlct,r,g,b,/GET
;
;
r1r=r
g1g=g
b1b=b
;
;
WINDOW,1,xsize=700,ysize=700,title='PPI data'
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
          print,"color = "+STRING(color)
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
     print,"dcolor-kj- = "+STRING(dcolor(k,j))
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
     print,"dcolor-k-az- = "+STRING(dcolor(k,num_az-1))
   endfor
oplot,[-1.*mx,mx],[0,0],thick=0.5,color=0
oplot,[0,0],[-1.*mx,mx],thick=0.5,color=0

RAW_TV_IMAGE=TVRD()


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

;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------

window_1=!D.WINDOW
JPG_GRAPHIC=TVRD()

;------------------------------------------------

;------------------------------------------------
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

sMAP_GRAPHIC=MAP_GRAPHIC[5:435,10:420]
;IDL> 
s2map=congrid(sMAP_GRAPHIC,420,420)
;IDL> 
tv,s2map

WINDOW,xsize=700,ysize=700,title='MAP data',/free
window_4=!D.WINDOW

TV, s2map, 140,140

s3map=TVRD()

JPG_GRAPHIC2=JPG_GRAPHIC

JPG_GRAPHIC2(WHERE(s3map EQ 255))=1


WINDOW,/free,xs=700,ys=700
window_3=!D.WINDOW

tv, JPG_GRAPHIC2
loadct,39
tvlct,rrr,ggg,bbb,/GET
tv, JPG_GRAPHIC2

;#-------------------------------------------------------------------------------


WINDOW,/free,xs=700,ys=700
window_5=!D.WINDOW

tvscl, JPG_GRAPHIC2

JPG_GRAPHIC3=BYTSCL(JPG_GRAPHIC2)


loadct,24
tvlct,rrr,ggg,bbb,/GET
tvscl, JPG_GRAPHIC3

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


help, FTW_DATA_RAW_STRUCTURE._FILENAME, /STRUCTURE

print, '..Nearly at the end...'


;#-------------------------------------------------------------------------------
;#-------------------------------------------------------------------------------
print,dadash
print,"Plot NEXRAD LEVEL II data ENDS:"
SPAWN,'date',date_info
print,date_info
print,dadash
;#-------------------------------------------------------------------------------

;#
STOP
END

;#-------------------------------------------------------------------------------
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--## END OF IDL CODE                                                       
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
;#                                                                               

