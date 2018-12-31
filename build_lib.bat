@echo off
rem
rem   BUILD_LIB [-dbg]
rem
rem   Build the ESCR library.
rem
setlocal
call build_pasinit

call src_insall %srcdir% %libname%

call src_pas %srcdir% %libname%_cmd %1
call src_pas %srcdir% %libname%_cmd_block %1
call src_pas %srcdir% %libname%_cmd_if %1
call src_pas %srcdir% %libname%_cmd_loop %1
call src_pas %srcdir% %libname%_cmd_macro %1
call src_pas %srcdir% %libname%_cmd_subr %1
call src_pas %srcdir% %libname%_err %1
call src_pas %srcdir% %libname%_exblock %1
call src_pas %srcdir% %libname%_format %1
call src_pas %srcdir% %libname%_get %1
call src_pas %srcdir% %libname%_ifn %1
call src_pas %srcdir% %libname%_ifun %1
call src_pas %srcdir% %libname%_ifun_vnl %1
call src_pas %srcdir% %libname%_infile %1
call src_pas %srcdir% %libname%_inh %1
call src_pas %srcdir% %libname%_inline %1
call src_pas %srcdir% %libname%_open %1
call src_pas %srcdir% %libname%_out %1
call src_pas %srcdir% %libname%_run %1
call src_pas %srcdir% %libname%_stat %1
call src_pas %srcdir% %libname%_sym %1
call src_pas %srcdir% %libname%_sym_cmd %1
call src_pas %srcdir% %libname%_sym_func %1
call src_pas %srcdir% %libname%_syt %1
call src_pas %srcdir% %libname%_term %1
call src_pas %srcdir% %libname%_ulab %1
call src_pas %srcdir% %libname%_util %1
call src_pas %srcdir% %libname%_val %1

call src_lib %srcdir% %libname%
call src_msg %srcdir% %libname%
