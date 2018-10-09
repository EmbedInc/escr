@echo off
rem
rem   Build and install the documentation from this source directory.
rem
setlocal
call build_pasinit
set docname=escr

call treename_var (cog)source/%srcdir%/doc srcdoc
call treename_var (cog)doc dstdoc
cd /d "%dstdoc%"
if not exist %docname% mkdir %docname%
cd %docname%
copy "%srcdoc%\*.htm" /y
copyt "%srcdoc%/backg.jpg" backg.jpg
cd ..
slink %docname%.htm escr/index.htm
