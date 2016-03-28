@echo off
rem
rem   BUILD_ALL
rem
rem   Build all the objects from the ESCR source library.
rem
setlocal
set srcdir=escr

call src_get %srcdir% build_escr_lib.bat
call build_escr_lib

call src_prog %srcdir% escr
