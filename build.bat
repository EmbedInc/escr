@echo off
rem
rem   Build everything from this source directory.
rem
setlocal

copyt (cog)com/escr_old.exe (cog)com/escr.exe
call delete ~/com_dbg/escr.exe
call src_clean

call godir "(cog)source/escr"

call build_lib
call build_progs
call build_doc
