;#===================================================================================
;#
;#
;#==-FNMOC/N38DI PYTHON PROGRAM DEFINITION-==========================================
;#
;# NAME:
;# :::::::::::::::::::::::::::::::::::::::::::::::
;# make_radar_maps.pro
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
;===================================================================================



; Display Z or Vr values with 10 colors.
; POLYFILL routine used to display uncompressed radar data files
; Use bwppi.pro for B/W hardcopy version.
;
;
PRO make_radar_maps_KFDR
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

MAP_PATH='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/SITE_MAPS2/'

SITE_DIR='/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/SITE_DATA'

SITEdatafile=SITE_DIR+'/KFDR.dat'

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
print,"Plot NEXRAD LEVEL II data map BEGINS:"
SPAWN,'date',date_info
print,date_info

print,dadash


;#DEVICE, DECOMPOSED=0
restore,'iriscol.dat'

print, "-----------------"
print, "-----------------"
print, "==============================================="

month = (['January','February','March',  'April',  'May',     'June', $
          'July',   'August','September','October','November','December'])
pd  = !pi / 180.

tvlct,r,g,b

TVLCT, rr, gg, bb, /GET

;
;--------------------------------------------------------
; The WINDOW must be instantiated as a /pixmap
; in order to support running from cron/PBS. 
; PJMC 2016-09-26
;--------------------------------------------------------
;
;WINDOW,1,xsize=700,ysize=700,title='PPI data'
;WINDOW,1,xsize=700,ysize=700,title='PPI data',/pixmap
WINDOW,1,xsize=700,ysize=700,title='PPI data'
window_1=!D.WINDOW


;

JPG_GRAPHIC=TVRD()

;;------------------------------------------------

;;------------------------------------------------
;WINDOW,/free,xs=440,ys=440,/pixmap
WINDOW,/free,xs=440,ys=440
;
window_2=!D.WINDOW
;
nd248=4.139667
;
minlat = ctr_lat - nd248
maxlat = ctr_lat + nd248
minlon = ctr_long- nd248
maxlon = ctr_long+ nd248
llimts=[minlat,minlon,maxlat,maxlon]
map_set,ctr_lat, ctr_long, /ORTHOGRAPHIC,/CONTINENTS, LIMIT=llimts,/GRID,/USA
plots, ctr_long,ctr_lat,PSYM=-2, SYMSIZE=2.5
xyouts, ctr_long+.25,ctr_lat+.25,call_sign,CHARTHICK=2,CHARSIZE=2.25
MAP_GRAPHIC=TVRD()
;
;;------------------------------------------------
;
sMAP_GRAPHIC=MAP_GRAPHIC[5:435,10:420]
s2map=congrid(sMAP_GRAPHIC,420,420)
tv,s2map
;
;;------------------------------------------------
;
;
;WINDOW,xsize=700,ysize=700,title='MAP data',/free,/pixmap
WINDOW,xsize=700,ysize=700,title='MAP data',/free
;
window_4=!D.WINDOW
;
TV, s2map, 140,140
s3map=TVRD()

MAP_ARRAY=s3map


MAP_FILE=MAP_PATH+call_sign+"_MAP_ARRAY.sav"

PRINT, "Saving map data in file: "+MAP_FILE


SAVE,s3map,FILENAME = MAP_FILE

PRINT, "Saved"

PRINT, "Deleted Windows"

WWD

;
JPG_GRAPHIC2=JPG_GRAPHIC
;
;;JPG_GRAPHIC2(WHERE(s3map EQ 255))=1
JPG_GRAPHIC2(WHERE(s3map EQ 255))=0
;
;
WINDOW,/free,xs=700,ys=700,/pixmap
;
window_3=!D.WINDOW
;
tv, JPG_GRAPHIC2
;loadct,39
;loadct,14
tvlct,rrr,ggg,bbb,/GET
tv, JPG_GRAPHIC2
;

;#-------------------------------------------------------------------------------
; This section added by Paul McCrone to read in the completed graphic as save as 
; a jpg file
; PJMC 2016-09-26
;#-------------------------------------------------------------------------------

;WRITE_GIF,FILENAME_GIF2 , JPG_GRAPHIC2, rrr, ggg, bbb , BACKGROUND_COLOR=bblack



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
