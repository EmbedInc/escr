@echo off
rem
rem   BUILD_ESCR_LIB [-dbg]
rem
rem   Build the ESCR library from the ESCR source directory.
rem
setlocal
set srcdir=escr
set libname=escr

call src_go %srcdir%
call src_getfrom sys base.ins.pas
call src_getfrom sys sys.ins.pas
call src_getfrom util util.ins.pas
call src_getfrom string string.ins.pas
call src_getfrom file file.ins.pas
call src_getfrom pic pic.ins.pas

call src_msg %srcdir% %libname%

call src_insall %srcdir% %libname%

call src_pas %srcdir% %libname%_cmd
call src_pas %srcdir% %libname%_cmd_block
call src_pas %srcdir% %libname%_cmd_if
call src_pas %srcdir% %libname%_cmd_loop
call src_pas %srcdir% %libname%_cmd_macro
call src_pas %srcdir% %libname%_cmd_subr
call src_pas %srcdir% %libname%_err
call src_pas %srcdir% %libname%_exblock
call src_pas %srcdir% %libname%_format
call src_pas %srcdir% %libname%_func
call src_pas %srcdir% %libname%_get
call src_pas %srcdir% %libname%_infile
call src_pas %srcdir% %libname%_inh
call src_pas %srcdir% %libname%_inline
call src_pas %srcdir% %libname%_open
call src_pas %srcdir% %libname%_run
call src_pas %srcdir% %libname%_sym
call src_pas %srcdir% %libname%_sym_cmd
call src_pas %srcdir% %libname%_term
call src_pas %srcdir% %libname%_ulab
call src_pas %srcdir% %libname%_util
call src_pas %srcdir% %libname%_val

call src_lib %srcdir% %libname%
