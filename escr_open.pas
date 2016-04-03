{   Routines for manipulating ESCR system use states.
*
*   For a application to use the ESCR system, it must create a use state, use
*   the system, then delete the use state.  The application maintains the
*   pointer to the use state, and passes it to all ESCR routines.
}
module escr_open;
define escr_open;
define escr_close;
define escr_set_incsuff;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Local subroutine ESCR_SYTABLE_INIT (E, SYTABLE, STAT)
*
*   Initialize the symbol table SYTABLE.
}
procedure escr_sytable_init (          {initialize a symbol table}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     sytable: escr_sytable_t;     {the symbol table to initialize}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  util_mem_context_get (e.mem_p^, sytable.mem_p); {create mem context for sym table}
  if sytable.mem_p = nil then begin    {didn't get new memory context ?}
    sys_stat_set (escr_subsys_k, escr_err_nomcontext_k, stat);
    return;
    end;

  string_hash_create (                 {create the symbol names hash table}
    sytable.hash,                      {handle to the hash table}
    escr_sym_nbuck_k,                  {number of hash buckets}
    escr_max_namelen_k,                {max characters for hash table entry name}
    sizeof(escr_sym_pp_t),             {size of data to store each hash table entry}
    [],                                {make mem context, allow deleting entries}
    sytable.mem_p^);                   {parent memory context}
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
procedure escr_open (                  {start a new ouse of the ESCR system}
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

  escr_sytable_init (e_p^, e_p^.sym_var, stat); {init variables/constants symbol table}
  if sys_error(stat) then return;
  escr_sytable_init (e_p^, e_p^.sym_sub, stat); {init subroutines symbol table}
  if sys_error(stat) then return;
  escr_sytable_init (e_p^, e_p^.sym_mac, stat); {init macros symbol table}
  if sys_error(stat) then return;
  escr_sytable_init (e_p^, e_p^.sym_fun, stat); {init functions symbol table}
  if sys_error(stat) then return;
  escr_sytable_init (e_p^, e_p^.sym_cmd, stat); {init commands symbol table}
  if sys_error(stat) then return;
  escr_sytable_init (e_p^, e_p^.sym_lab, stat); {init labels symbol table}
  if sys_error(stat) then return;
  {
  *   Do basic initialization, pointer to NIL, strings to empty, etc.
  }
  e_p^.files_p := nil;
  e_p^.ibuf.max := size_char(e_p^.ibuf.str);
  e_p^.ibuf.len := 0;
  e_p^.ip := 1;
  e_p^.lparm.max := size_char(e_p^.lparm.str);
  e_p^.lparm.len := 0;
  e_p^.cmd.max := size_char(e_p^.cmd.str);
  e_p^.cmd.len := 0;
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
{
*   Do higher level initialization now that all fields have at least legal
*   values.
}
  escr_inline_func_init (e_p^);        {init for processing inline functions}
  {
  *   Add the standard commands to the commands symbol table.
  }
  addcmd ('BLOCK', addr(escr_cmd_block), stat); if sys_error(stat) then return;
  addcmd ('CALL', addr(escr_cmd_call), stat); if sys_error(stat) then return;
  addcmd ('CONST', addr(escr_cmd_const), stat); if sys_error(stat) then return;
  addcmd ('DEL', addr(escr_cmd_del), stat); if sys_error(stat) then return;
  addcmd ('ELSE', addr(escr_cmd_else), stat); if sys_error(stat) then return;
  addcmd ('ENDBLOCK', addr(escr_cmd_endblock), stat); if sys_error(stat) then return;
  addcmd ('ENDIF', addr(escr_cmd_endif), stat); if sys_error(stat) then return;
  addcmd ('ENDLOOP', addr(escr_cmd_endloop), stat); if sys_error(stat) then return;
  addcmd ('ENDMAC', addr(escr_cmd_endmac), stat); if sys_error(stat) then return;
  addcmd ('ENDSUB', addr(escr_cmd_endsub), stat); if sys_error(stat) then return;
  addcmd ('IF', addr(escr_cmd_if), stat); if sys_error(stat) then return;
  addcmd ('INCLUDE', addr(escr_cmd_include), stat); if sys_error(stat) then return;
  addcmd ('LOOP', addr(escr_cmd_loop), stat); if sys_error(stat) then return;
  addcmd ('MACRO', addr(escr_cmd_macro), stat); if sys_error(stat) then return;
  addcmd ('QUIT', addr(escr_cmd_quit), stat); if sys_error(stat) then return;
  addcmd ('QUITMAC', addr(escr_cmd_quitmac), stat); if sys_error(stat) then return;
  addcmd ('REPEAT', addr(escr_cmd_repeat), stat); if sys_error(stat) then return;
  addcmd ('RETURN', addr(escr_cmd_return), stat); if sys_error(stat) then return;
  addcmd ('SET', addr(escr_cmd_set), stat); if sys_error(stat) then return;
  addcmd ('SHOW', addr(escr_cmd_show), stat); if sys_error(stat) then return;
  addcmd ('STOP', addr(escr_cmd_stop), stat); if sys_error(stat) then return;
  addcmd ('SUBROUTINE', addr(escr_cmd_subroutine), stat); if sys_error(stat) then return;
  addcmd ('SYLIST', addr(escr_cmd_sylist), stat); if sys_error(stat) then return;
  addcmd ('THEN', addr(escr_cmd_then), stat); if sys_error(stat) then return;
  addcmd ('VAR', addr(escr_cmd_var), stat); if sys_error(stat) then return;
  addcmd ('WRITE', addr(escr_cmd_write), stat); if sys_error(stat) then return;
  addcmd ('WRITEEND', addr(escr_cmd_writeend), stat); if sys_error(stat) then return;
  addcmd ('WRITETO', addr(escr_cmd_writeto), stat); if sys_error(stat) then return;

  return;

err_nocontext:                         {could get new memory context}
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
