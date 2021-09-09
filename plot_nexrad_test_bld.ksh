#!/bin/ksh
#    SCCS ID: $HeadURL$
#    SCCS ID: @(#)$Id$
#
#==-FNMOC/N38DI KSH SCRIPT DEFINITION-==========================================
#
# NAME:
# :::::::::::::::::::::::::::::::::::::::::::::::
# nexrad_image_process.py
# :::::::::::::::::::::::::::::::::::::::::::::::
#
#  PROGRAM OVERVIEW:
#       Finally, an IDL program takes the ASCII output and produces a graphical display
#           -- The output is saved as both JPG and Animated GIF  
#
#--------------------------------------------------------------------------------------------------
# PARAMETER TABLE:
#--------------------------------------------------------------------------------------------------
#
# I/O           NAME                               TYPE            FUNCTION
#--------------------------------------------------------------------------------------------------
#_________________________________________________________________________________________________
#=================================================================================================
#
#=================================================================================================
#-
#-
#
# Programmer: Mr. Paul McCrone     23 September 2016
#             NOTE:
#                  While Mr. McCrone wrote the PYTHON code, the -C- code and IDL Code
#                  were originally written by Dr. Paul Harasti of Naval Research Lab.
#                  (McCrone made the 
#                  modifications to the IDL code.) 
#
# Modification  :  BELOW
#========================================================================================
#  Version 1.0   , Dated 2016-Sep-23
#                  Initial Build.
#  - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
#========================================================================================



#------------------------------------------------------
# This is the script that builds the IDL Runtime 
# program version of plot_nexrad_level2.pro
#
#-----------------------------------------------------
#



PROCESSOR=`uname -p`
echo $PROCESSOR

#export IDL_DIR=/opt/idl/idl81
export IDL_DIR=/u/alpha/


#export IDL_INIT=${IDL_DIR}/bin/idl_setup.ksh
export IDL_INIT=${IDL_DIR}/bin/idl_init.ksh

. $IDL_INIT

idl << EOF
.compile plot_nexrad_test.pro
resolve_all
save,/routines,filename='plot_nexrad_test.sav'
exit
EOF
