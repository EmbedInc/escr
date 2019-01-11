{   Routines that manipulate error status values.
}
module escr_stat;
define escr_stat_cmd_noarg;
define escr_stat_sym_nfound;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_STAT_CMD_NOARG (E, STAT)
*
*   Set STAT to indicate missing command parameter, unless STAT is already
*   indicating error.
}
procedure escr_stat_cmd_noarg (        {missing command argument}
  in      e: escr_t;                   {state for this use of the ESCR system}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param;

begin
  if sys_error(stat) then return;      {indicating previous error ?}

  sys_stat_set (escr_subsys_k, escr_err_missingparm_k, stat);
  sys_stat_parm_vstr (e.parse_p^.cmd, stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_STAT_SYM_NFOUND (NAME, STAT)
*
*   Set STAT to indicate the symbol NAME was not found.
}
procedure escr_stat_sym_nfound (       {symbol not found}
  in      name: univ string_var_arg_t; {symbol name}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param;

begin
  sys_stat_set (escr_subsys_k, escr_err_nfsym_k, stat);
  sys_stat_parm_vstr (name, stat);
  end;
