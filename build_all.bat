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

call treename_var (cog)doc tnam
call treename_var (cog)source/escr/doc srcdoc
cd /d "%tnam%"
if not exist escr mkdir escr
cd escr
copy "%srcdoc%\*.htm" /y
copyt "%srcdoc%/backg.jpg" backg.jpg
cd ..
slink escr.htm escr/index.htm

