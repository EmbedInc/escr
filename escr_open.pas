{   Routines for manipulating ESCR system use states.
*
*   For a application to use the ESCR system, it must create a use state, use
*   the system, then delete the use state.  The application maintains the
*   pointer to the use state, and passes it to all ESCR routines.
}
module escr_open;
define escr_parse_init;
define escr_open;
define escr_close;
define escr_set_incsuff;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_PARSE_INIT (PAR)
*
*   Initialize the input parsing state data structure, PAR.
}
procedure escr_parse_init (            {init input parsing state descriptor}
  out     par: escr_parse_t);          {parsing state to init}
  val_param;

begin
  par.prev_p := nil;

  par.ibuf.max := size_char(par.ibuf.str);
  par.ibuf.len := 0;
  par.ip := 1;

  par.lparm.max := size_char(par.lparm.str);
  par.lparm.len := 0;

  par.cmd.max := size_char(par.cmd.str);
  par.cmd.len := 0;

  par.funarg.p := 1;
  par.funarg.s.max := size_char(par.funarg.s.str);
  par.funarg.s.len := 0;

  par.funame.max := size_char(par.funame.str);
  par.funame.len := 0;

  par.funret.max := size_char(par.funret.str);
  par.funret.len := 0;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_OPEN (MEM, E_P, STAT)
*
*   Create a new ESCR system use state.  This must be the first routine called
*   for any use of the ESCR system.
*
*   MEM is the parent memory context within which all new dynamic memory will be
*   allocated.  Only a single sub-context will be added directly to MEM.
*
*   E_P is return pointing to the new ESCR system use state.  This pointer must
*   be maintained by the application, and passed to each ESCR routine it calls
*   for this use of the system.  If the new use state can not be created for
*   some reason, then E_P will be returned NIL and STAT will indicate the error
*   condition.
}
procedure escr_open (                  {start a new use of the ESCR system}
  in out  mem: util_mem_context_t;     {parent memory context, will make sub context}
  out     e_p: escr_p_t;               {will point to new initialized ESCR use state}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  mem_p: util_mem_context_p_t;         {pointer to new top memory context}

label
  err_nocontext, err;
{
******************************
*
*   Subroutine ADDCMD (NAME, ROUTINE_P, STAT)
*   This routine is local to ESCR_OPEN.
*
*   Add the intrinsic command NAME to the commands symbol table.  NAME is a
*   Pascal string, not a var string.  ROUTINE_P is the pointer to the command
*   routine.
}
type
  {
  *   A separate template for a command routine is defined here.  This is the
  *   same as the official ESCR_ICMD_P_T except that the first argument is the
  *   library use state directly defined for IN and OUT use, as apposed to a
  *   pointer to the library use state.  The former is how command routines are
  *   actually defined, but the ESCR_ICMD_P_T template can't be defined that way
  *   due to circular dependencies that would be required of such a definition.
  *   Both definitions should result in the same code.
  }
  cmd_routine_p_t = ^procedure (
    in out e: escr_t;
    out   stat: sys_err_t);
    val_param;

procedure addcmd (                     {add intrinsic command to commands table}
  in      name: string;                {command name}
  in      routine_p: cmd_routine_p_t;  {pointer to command routine}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_icmd_add (                      {add intrinsic command to commands table}
    e_p^,                              {state for this use of the ESCR library}
    string_v(name),                    {command name}
    escr_icmd_p_t(routine_p),          {pointer to command routine}
    stat);
  end;
{
******************************
*
*   Subroutine ADDFUNC (NAME, ROUTINE_P, STAT)
*   This routine is local to ESCR_OPEN.
*
*   Add the intrinsic function NAME to the functions symbol table.  NAME is a
*   Pascal string, not a var string.  ROUTINE_P is the pointer to the function
*   routine.
}
type
  {
  *   A separate template for a function routine is defined here.  This is the
  *   same as the official ESCR_IFUNC_P_T except that the first argument is the
  *   library use state directly defined for IN and OUT use, as apposed to a
  *   pointer to the library use state.  The former is how function routines are
  *   actually defined, but the ESCR_IFUNC_P_T template can't be defined that
  *   way due to circular dependencies that would be required of such a
  *   definition.  Both definitions should result in the same code.
  }
  func_routine_p_t = ^procedure (
    in out e: escr_t;                  {library use state}
    out   stat: sys_err_t);
    val_param;

procedure addfunc (                    {add intrinsic function to functions table}
  in      name: string;                {function name}
  in      function_p: func_routine_p_t; {pointer to function routine}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_ifunc_add (                     {add intrinsic function to functions table}
    e_p^,                              {state for this use of the ESCR library}
    string_v(name),                    {function name}
    escr_ifunc_p_t(function_p),        {pointer to function routine}
    stat);
  end;
{
******************************
*
*   Start of ESCR_OPEN.
}
begin
  sys_error_none (stat);               {init to no error encountered}

  util_mem_context_get (mem, mem_p);   {create top mem context for this ESCR use}
  if mem_p = nil then goto err_nocontext; {unable to create new memory context ?}

  util_mem_grab (sizeof(e_p^), mem_p^, false, e_p); {create the new state block}
  if e_p = nil then begin              {couldn't get the memory ?}
    sys_stat_set (escr_subsys_k, escr_err_nomem_k, stat);
    sys_stat_parm_int (sizeof(e_p^), stat);
    goto err;
    end;
  e_p^.mem_p := mem_p;                 {save pointer to our top mem context}

  escr_sytable_init (e_p^.mem_p^, e_p^.sym_var, stat); {init variables/constants symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_sub, stat); {init subroutines symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_mac, stat); {init macros symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_fun, stat); {init functions symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_cmd, stat); {init commands symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_lab, stat); {init labels symbol table}
  if sys_error(stat) then goto err;
  escr_sytable_init (e_p^.mem_p^, e_p^.sym_src, stat); {init source code snippets symbol table}
  if sys_error(stat) then goto err;
{
*   Do basic initialization.
}
  e_p^.files_p := nil;
  escr_parse_init (e_p^.parse);
  e_p^.parse_p := addr(e_p^.parse);
  e_p^.exblock_p := nil;
  e_p^.inhroot.prev_p := nil;
  e_p^.inhroot.inh := false;
  e_p^.inhroot.inhty := escr_inhty_root_k;
  e_p^.inhibit_p := addr(e_p^.inhroot);
  e_p^.out_p := nil;
  e_p^.obuf.max := size_char(e_p^.obuf.str);
  e_p^.obuf.len := 0;
  e_p^.ulabn := 1;
  e_p^.incsuff := '';
  e_p^.cmdst.max := size_char(e_p^.cmdst.str);
  e_p^.cmdst.len := 0;
  e_p^.syexcl_p := nil;
  e_p^.commscr_p := nil;
  e_p^.commdat_p := nil;
  e_p^.syfunc.st.max := size_char(e_p^.syfunc.st.str);
  string_vstring (e_p^.syfunc.st, '[', 1);
  e_p^.syfunc.en.max := size_char(e_p^.syfunc.en.str);
  string_vstring (e_p^.syfunc.en, ']', 1);
  e_p^.syfunc_st_p := nil;
  e_p^.flags := [];
  e_p^.exstat := 0;
{
*   Do higher level initialization now that all fields have at least legal
*   values.
}
  escr_commscr_add (                   {init to default ESCR comments}
    e_p^,                              {state for this use of the ESCR system}
    string_v('//'),                    {comment start}
    string_v(''),                      {comment ends at end of line}
    stat);
  if sys_error(stat) then goto err;

  escr_syexcl_add (                    {add exclusion for double quoted strings}
    e_p^,                              {state for this use of the ESCR system}
    string_v('"'),                     {quoted string start}
    string_v('"'),                     {quoted string end}
    stat);
  if sys_error(stat) then goto err;

  escr_syexcl_add (                    {add exclusion for single quoted strings}
    e_p^,                              {state for this use of the ESCR system}
    string_v(''''),                    {quoted string start}
    string_v(''''),                    {quoted string end}
    stat);
  if sys_error(stat) then goto err;
  {
  *   Add the intrinsic commands to the commands symbol table.
  }
  addcmd ('append', addr(escr_cmd_append), stat); if sys_error(stat) then goto err;
  addcmd ('block', addr(escr_cmd_block), stat); if sys_error(stat) then goto err;
  addcmd ('call', addr(escr_cmd_call), stat); if sys_error(stat) then goto err;
  addcmd ('command', addr(escr_cmd_command), stat); if sys_error(stat) then goto err;
  addcmd ('const', addr(escr_cmd_const), stat); if sys_error(stat) then goto err;
  addcmd ('del', addr(escr_cmd_del), stat); if sys_error(stat) then goto err;
  addcmd ('dir', addr(escr_cmd_dir), stat); if sys_error(stat) then goto err;
  addcmd ('else', addr(escr_cmd_else), stat); if sys_error(stat) then goto err;
  addcmd ('endblock', addr(escr_cmd_endblock), stat); if sys_error(stat) then goto err;
  addcmd ('endcmd', addr(escr_cmd_endcmd), stat); if sys_error(stat) then goto err;
  addcmd ('endfunc', addr(escr_cmd_endfunc), stat); if sys_error(stat) then goto err;
  addcmd ('endif', addr(escr_cmd_endif), stat); if sys_error(stat) then goto err;
  addcmd ('endloop', addr(escr_cmd_endloop), stat); if sys_error(stat) then goto err;
  addcmd ('endmac', addr(escr_cmd_endmac), stat); if sys_error(stat) then goto err;
  addcmd ('endsub', addr(escr_cmd_endsub), stat); if sys_error(stat) then goto err;
  addcmd ('function', addr(escr_cmd_function), stat); if sys_error(stat) then goto err;
  addcmd ('funcval', addr(escr_cmd_funcval), stat); if sys_error(stat) then goto err;
  addcmd ('funcstr', addr(escr_cmd_funcstr), stat); if sys_error(stat) then goto err;
  addcmd ('if', addr(escr_cmd_if), stat); if sys_error(stat) then goto err;
  addcmd ('include', addr(escr_cmd_include), stat); if sys_error(stat) then goto err;
  addcmd ('loop', addr(escr_cmd_loop), stat); if sys_error(stat) then goto err;
  addcmd ('macro', addr(escr_cmd_macro), stat); if sys_error(stat) then goto err;
  addcmd ('quit', addr(escr_cmd_quit), stat); if sys_error(stat) then goto err;
  addcmd ('quitmac', addr(escr_cmd_quitmac), stat); if sys_error(stat) then goto err;
  addcmd ('repeat', addr(escr_cmd_repeat), stat); if sys_error(stat) then goto err;
  addcmd ('return', addr(escr_cmd_return), stat); if sys_error(stat) then goto err;
  addcmd ('run', addr(escr_cmd_run), stat); if sys_error(stat) then goto err;
  addcmd ('set', addr(escr_cmd_set), stat); if sys_error(stat) then goto err;
  addcmd ('show', addr(escr_cmd_show), stat); if sys_error(stat) then goto err;
  addcmd ('stop', addr(escr_cmd_stop), stat); if sys_error(stat) then goto err;
  addcmd ('subroutine', addr(escr_cmd_subroutine), stat); if sys_error(stat) then goto err;
  addcmd ('sylist', addr(escr_cmd_sylist), stat); if sys_error(stat) then goto err;
  addcmd ('then', addr(escr_cmd_then), stat); if sys_error(stat) then goto err;
  addcmd ('var', addr(escr_cmd_var), stat); if sys_error(stat) then goto err;
  addcmd ('write', addr(escr_cmd_write), stat); if sys_error(stat) then goto err;
  addcmd ('writepop', addr(escr_cmd_writepop), stat); if sys_error(stat) then goto err;
  addcmd ('writepush', addr(escr_cmd_writepush), stat); if sys_error(stat) then goto err;
  {
  *   Add the intrinsic functions to the functions symbol table.
  }
  addfunc ('abs', addr(escr_ifun_abs), stat); if sys_error(stat) then goto err;
  addfunc ('and', addr(escr_ifun_and), stat); if sys_error(stat) then goto err;
  addfunc ('arg', addr(escr_ifun_arg), stat); if sys_error(stat) then goto err;
  addfunc ('ccode', addr(escr_ifun_ccode), stat); if sys_error(stat) then goto err;
  addfunc ('char', addr(escr_ifun_char), stat); if sys_error(stat) then goto err;
  addfunc ('chars', addr(escr_ifun_chars), stat); if sys_error(stat) then goto err;
  addfunc ('cos', addr(escr_ifun_cos), stat); if sys_error(stat) then goto err;
  addfunc ('date', addr(escr_ifun_date), stat); if sys_error(stat) then goto err;
  addfunc ('degr', addr(escr_ifun_degr), stat); if sys_error(stat) then goto err;
  addfunc ('dent', addr(escr_ifun_dent), stat); if sys_error(stat) then goto err;
  addfunc ('dir', addr(escr_ifun_dir), stat); if sys_error(stat) then goto err;
  addfunc ('div', addr(escr_ifun_div), stat); if sys_error(stat) then goto err;
  addfunc ('/', addr(escr_ifun_divide), stat); if sys_error(stat) then goto err;
  addfunc ('e', addr(escr_ifun_e), stat); if sys_error(stat) then goto err;
  addfunc ('eng', addr(escr_ifun_eng), stat); if sys_error(stat) then goto err;
  addfunc ('=', addr(escr_ifun_eq), stat); if sys_error(stat) then goto err;
  addfunc ('evar', addr(escr_ifun_evar), stat); if sys_error(stat) then goto err;
  addfunc ('exist', addr(escr_ifun_exist), stat); if sys_error(stat) then goto err;
  addfunc ('exp', addr(escr_ifun_exp), stat); if sys_error(stat) then goto err;
  addfunc ('file', addr(escr_ifun_file), stat); if sys_error(stat) then goto err;
  addfunc ('fp', addr(escr_ifun_fp), stat); if sys_error(stat) then goto err;
  addfunc ('>=', addr(escr_ifun_ge), stat); if sys_error(stat) then goto err;
  addfunc ('>', addr(escr_ifun_gt), stat); if sys_error(stat) then goto err;
  addfunc ('if', addr(escr_ifun_if), stat); if sys_error(stat) then goto err;
  addfunc ('int', addr(escr_ifun_int), stat); if sys_error(stat) then goto err;
  addfunc ('~', addr(escr_ifun_inv), stat); if sys_error(stat) then goto err;
  addfunc ('isint', addr(escr_ifun_isint), stat); if sys_error(stat) then goto err;
  addfunc ('isnum', addr(escr_ifun_isnum), stat); if sys_error(stat) then goto err;
  addfunc ('lab', addr(escr_ifun_lab), stat); if sys_error(stat) then goto err;
  addfunc ('lcase', addr(escr_ifun_lcase), stat); if sys_error(stat) then goto err;
  addfunc ('<=', addr(escr_ifun_le), stat); if sys_error(stat) then goto err;
  addfunc ('lnam', addr(escr_ifun_lnam), stat); if sys_error(stat) then goto err;
  addfunc ('log', addr(escr_ifun_log), stat); if sys_error(stat) then goto err;
  addfunc ('log2', addr(escr_ifun_log2), stat); if sys_error(stat) then goto err;
  addfunc ('<', addr(escr_ifun_lt), stat); if sys_error(stat) then goto err;
  addfunc ('max', addr(escr_ifun_max), stat); if sys_error(stat) then goto err;
  addfunc ('min', addr(escr_ifun_min), stat); if sys_error(stat) then goto err;
  addfunc ('-', addr(escr_ifun_minus), stat); if sys_error(stat) then goto err;
  addfunc ('<>', addr(escr_ifun_ne), stat); if sys_error(stat) then goto err;
  addfunc ('not', addr(escr_ifun_not), stat); if sys_error(stat) then goto err;
  addfunc ('now', addr(escr_ifun_now), stat); if sys_error(stat) then goto err;
  addfunc ('or', addr(escr_ifun_or), stat); if sys_error(stat) then goto err;
  addfunc ('pi', addr(escr_ifun_pi), stat); if sys_error(stat) then goto err;
  addfunc ('+', addr(escr_ifun_plus), stat); if sys_error(stat) then goto err;
  addfunc ('1+', addr(escr_ifun_postinc), stat); if sys_error(stat) then goto err;
  addfunc ('1-', addr(escr_ifun_postdec), stat); if sys_error(stat) then goto err;
  addfunc ('+1', addr(escr_ifun_preinc), stat); if sys_error(stat) then goto err;
  addfunc ('-1', addr(escr_ifun_predec), stat); if sys_error(stat) then goto err;
  addfunc ('qstr', addr(escr_ifun_qstr), stat); if sys_error(stat) then goto err;
  addfunc ('qtk', addr(escr_ifun_qtk), stat); if sys_error(stat) then goto err;
  addfunc ('rdeg', addr(escr_ifun_rdeg), stat); if sys_error(stat) then goto err;
  addfunc ('rnd', addr(escr_ifun_rnd), stat); if sys_error(stat) then goto err;
  addfunc ('runex', addr(escr_ifun_runex), stat); if sys_error(stat) then goto err;
  addfunc ('runso', addr(escr_ifun_runso), stat); if sys_error(stat) then goto err;
  addfunc ('runtf', addr(escr_ifun_runtf), stat); if sys_error(stat) then goto err;
  addfunc ('seq', addr(escr_ifun_seq), stat); if sys_error(stat) then goto err;
  addfunc ('shiftl', addr(escr_ifun_shiftl), stat); if sys_error(stat) then goto err;
  addfunc ('shiftr', addr(escr_ifun_shiftr), stat); if sys_error(stat) then goto err;
  addfunc ('sin', addr(escr_ifun_sin), stat); if sys_error(stat) then goto err;
  addfunc ('sindx', addr(escr_ifun_sindx), stat); if sys_error(stat) then goto err;
  addfunc ('slen', addr(escr_ifun_slen), stat); if sys_error(stat) then goto err;
  addfunc ('sqrt', addr(escr_ifun_sqrt), stat); if sys_error(stat) then goto err;
  addfunc ('str', addr(escr_ifun_str), stat); if sys_error(stat) then goto err;
  addfunc ('substr', addr(escr_ifun_substr), stat); if sys_error(stat) then goto err;
  addfunc ('sym', addr(escr_ifun_sym), stat); if sys_error(stat) then goto err;
  addfunc ('tan', addr(escr_ifun_tan), stat); if sys_error(stat) then goto err;
  addfunc ('*', addr(escr_ifun_times), stat); if sys_error(stat) then goto err;
  addfunc ('tnam', addr(escr_ifun_tnam), stat); if sys_error(stat) then goto err;
  addfunc ('trunc', addr(escr_ifun_trunc), stat); if sys_error(stat) then goto err;
  addfunc ('ucase', addr(escr_ifun_ucase), stat); if sys_error(stat) then goto err;
  addfunc ('unquote', addr(escr_ifun_unquote), stat); if sys_error(stat) then goto err;
  addfunc ('v', addr(escr_ifun_v), stat); if sys_error(stat) then goto err;
  addfunc ('vnl', addr(escr_ifun_vnl), stat); if sys_error(stat) then goto err;
  addfunc ('xor', addr(escr_ifun_xor), stat); if sys_error(stat) then goto err;

  return;

err_nocontext:                         {could not get new memory context}
  sys_stat_set (escr_subsys_k, escr_err_nomcontext_k, stat);

err:                                   {return with error, STAT already set}
  if mem_p <> nil then begin           {our mem context was created ?}
    util_mem_context_del (mem_p);      {yes, delete all our dynamic memory}
    end;
  e_p := nil;                          {indicate the new state was not created}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CLOSE (E_P)
*
*   End a use of the ESCR system.  E_P must point to a previously-created ESCR
*   system use state.  All system resources allocated to the use state will be
*   released, and E_P will be returned NIL.
}
procedure escr_close (                 {end a use of the ESCR system}
  in out  e_p: escr_p_t);              {pointer to ESCR use state, returned NIL}
  val_param;

var
  mem_p: util_mem_context_p_t;         {pointer to new top memory context}

begin
  if e_p = nil then return;            {no state to deallocate ?}

  escr_out_close_all (e_p^, false);    {close all output files}

  mem_p := e_p^.mem_p;                 {get pointer to top memory context}
  util_mem_context_del (mem_p);        {delete the mem context}

  e_p := nil;                          {indicate use state no longer exists}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INCSUFF (E, SUFF)
*
*   Sets the list of allowed suffixes of include file names.  Each suffix is
*   blank-separated.  SUFF of all blank requires the include file name to be
*   exactly as specified.  This can also be one option when suffixes are
*   supplied by adding the suffix "" (in quotes).
}
procedure escr_set_incsuff (           {set allowed suffixes for include file names}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      suff: string);               {suffixes, blank separated}
  val_param;

begin
  e.incsuff := suff;
  end;
