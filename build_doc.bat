@echo off
rem
rem   Build and install the documentation from this source directory.
rem
setlocal
call build_vars
set docname=%srcdir%

if "%DEBUGGING%"=="true" exit /b 0

call treename_var (cog)source/%docname%/doc srcdoc
call godir (cog)doc
if exist "%docname%.txt" del "%docname%.txt"
if exist "%docname%.htm" del "%docname%.htm"
if exist "%docname%.@" del "%docname%.@"
if exist "%docname%" delt "%docname%" -nshow
copyt "%srcdoc%" "%docname%"
