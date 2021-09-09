#!/bin/ksh
#    SCCS ID: $HeadURL$
#    SCCS ID: @(#)$Id$
#
#==-FNMOC/N38DI KSH SCRIPT DEFINITION-==========================================
#
# NAME:
# :::::::::::::::::::::::::::::::::::::::::::::::
# nexrad_run_idlrt.ksh
# Run by:
#        -nexrad_image_process.py
# IDL run file:
#        -plot_nexrad_level2.sav
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
#
######################################
##
## the idl program 
######################################

# idl:     aliased to /gpfs/site/opt/idl/idl80/bin/idl

#IDL_DIR=/gpfs/site/opt/idl/idl80
IDL_DIR=/gpfs/site/opt/idl/idl84

PROGBINFTW=/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL

${IDL_DIR}/bin/idl -rt="${PROGBINFTW}/plot_nexrad_level2.sav"


###
### /gpfs/site/opt/idl/idl80/bin/idl -rt="/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_CompZ_IDL/plot_nexrad_level2.sav"
###


