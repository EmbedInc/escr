{   Routines for manipulating ESCR system use states.
*
*   For a application to use the ESCR system, it must create a use state, use
*   the system, then delete the use state.  The application maintains the
*   pointer to the use state, and passes it to all ESCR routines.
}
module escr_open;
define escr_open;
define escr_close;
%include 'escr2.ins.pas';
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
  util_mem_context_get (mem_p^, e_p^.mem_sytable_p); {create context for symbol table}
  if e_p^.mem_sytable_p = nil then goto err_nocontext;
  util_mem_context_get (mem_p^, e_p^.mem_sym_p); {create context for symbol table data}
  if e_p^.mem_sym_p = nil then goto err_nocontext;
  util_mem_context_get (mem_p^, e_p^.mem_top_p); {create context for top execution block}
  if e_p^.mem_top_p = nil then goto err_nocontext;
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
  e_p^.inhibit_p := nil;
  e_p^.out_p := nil;
  e_p^.obuf.max := size_char(e_p^.obuf.str);
  e_p^.obuf.len := 0;
  e_p^.labeln := 1;
  e_p^.lang := escr_lang_aspic_k;
  e_p^.nflags := 0;
  e_p^.flag_byten := 0;
  e_p^.flag_bitn := 0;
  {
  *   Do higher level initialization now that all fields have at least legal
  *   values.
  }
  string_hash_create (                 {create the symbol table}
    e_p^.sym,                          {symbol table to create}
    escr_sym_nbuck_k,                  {number of buckets in the hash table}
    escr_max_namelen_k,                {max allowed characters in symbol name}
    sizeof(escr_sym_p_t),              {size of data stored in each table entry}
    [string_hashcre_memdir_k],         {use the supplied memory context directly}
    e_p^.mem_sytable_p^);              {pointer to memory context to use}

  escr_inh_new (e_p^);                 {create the root execution inhibit state}
  e_p^.inhibit_p^.inhty := escr_inhty_root_k;
  escr_exblock_new (e_p^);             {create top level execution block}
  escr_exblock_loclab_init (e_p^);     {top level block always has local labels}

  escr_inline_func_init (e_p^);        {init for processing inline functions}
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

  escr_close_out_all (e_p^, false);    {close all output files}

  mem_p := e_p^.mem_p;                 {get pointer to top memory context}
  util_mem_context_del (mem_p);        {delete the mem context}

  e_p := nil;                          {indicate use state no longer exists}
  end;
