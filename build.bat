@echo off
rem
rem   Build everything from this source directory.
rem
setlocal

rem copyt (cog)com/escr_old.exe (cog)com/escr.exe
rem call delete ~/com_dbg/escr.exe
rem call src_clean

call godir "(cog)source/escr"

call build_lib
call build_progs
call build_doc
