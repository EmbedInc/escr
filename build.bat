@echo off
rem
rem   BUILD [-dbg]
rem
rem   Build everything from the ESCR source directory.
rem
setlocal
call godir (cog)source/escr

call build_lib %1
call build_progs %1
call build_doc %1
