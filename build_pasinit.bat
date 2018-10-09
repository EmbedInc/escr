@echo off
rem
rem   Set up for building a Pascal (.pas suffix) module.
rem
set srcdir=escr
set libname=escr
set buildname=

call src_get %srcdir% %libname%.ins.pas
call src_get %srcdir% %libname%2.ins.pas
call src_getfrom sys base.ins.pas
call src_getfrom sys sys.ins.pas
call src_getfrom util util.ins.pas
call src_getfrom string string.ins.pas
call src_getfrom file file.ins.pas

call src_builddate "%srcdir%"
