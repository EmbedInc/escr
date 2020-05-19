@echo off
rem
rem   BUILD_LIB
rem
rem   Build the ESCR library.
rem
setlocal
call build_pasinit

call src_insall %srcdir% %libname%

call src_pas %srcdir% %libname%_cmd
call src_pas %srcdir% %libname%_cmd_block
call src_pas %srcdir% %libname%_cmd_cmd
call src_pas %srcdir% %libname%_cmd_func
call src_pas %srcdir% %libname%_cmd_if
call src_pas %srcdir% %libname%_cmd_loop
call src_pas %srcdir% %libname%_cmd_macro
call src_pas %srcdir% %libname%_cmd_subr
call src_pas %srcdir% %libname%_err
call src_pas %srcdir% %libname%_exblock
call src_pas %srcdir% %libname%_format
call src_pas %srcdir% %libname%_get
call src_pas %srcdir% %libname%_ifn
call src_pas %srcdir% %libname%_ifun
call src_pas %srcdir% %libname%_infile
call src_pas %srcdir% %libname%_inh
call src_pas %srcdir% %libname%_inline
call src_pas %srcdir% %libname%_open
call src_pas %srcdir% %libname%_out
call src_pas %srcdir% %libname%_run
call src_pas %srcdir% %libname%_stat
call src_pas %srcdir% %libname%_sym
call src_pas %srcdir% %libname%_sym_cmd
call src_pas %srcdir% %libname%_sym_func
call src_pas %srcdir% %libname%_syt
call src_pas %srcdir% %libname%_term
call src_pas %srcdir% %libname%_ulab
call src_pas %srcdir% %libname%_util
call src_pas %srcdir% %libname%_val
call src_pas %srcdir% %libname%_vcon

call src_lib %srcdir% %libname%
call src_msg %srcdir% %libname%
