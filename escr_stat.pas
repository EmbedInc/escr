{   Routines that manipulate error status values.
}
module escr_stat;
define escr_stat_cmd_noarg;
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
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param;

begin
  if sys_error(stat) then return;      {indicating previous error ?}

  sys_stat_set (escr_subsys_k, escr_err_missingparm_k, stat);
  sys_stat_parm_vstr (e.cmd, stat);
  end;
