{   IF statement.
}
module escr_cmd_if;
define escr_cmd_if;
define escr_cmd_then;
define escr_cmd_else;
define escr_cmd_endif;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_IF (E, STAT)
*
*   /IF condition [THEN]
*
*   Execute the following code if the condition is TRUE.  If the THEN keyword
*   is used, then the following code is conditionally executed until the next
*   /ENDIF command.  With THEN used, the total IF structure has the form:
*
*     /IF condition THEN
*       - executed on TRUE -
*       /ENDIF
*
*   If the THEN keyword is omitted, then the IF structure has this form:
*
*     /IF condition
*         - always executed -
*       /THEN
*         - executed on TRUE -
*       /ELSE
*         - executed on FALSE -
*       /ENDIF
}
procedure escr_cmd_if (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  pick: sys_int_machine_t;             {number of keyword picked from list}
  cond: boolean;                       {the IF condition value}

label
  err_missing;

begin
  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_if_k; {this inhibit is for IF command}
  if e.inhibit_p^.inh then return;     {already inhibited, just track IF nesting ?}
  e.inhibit_p^.if_flags := [];         {init all IF control flags to off}

  if not escr_get_bool (e, cond, stat) then begin {get the value of the conditional}
    goto err_missing;
    end;
  if cond then begin                   {the IF condition is TRUE}
    e.inhibit_p^.if_flags := e.inhibit_p^.if_flags + [escr_ifflag_true_k]; {set flag accordingly}
    e.inhibit_p^.inh := false;         {execution starts out enabled}
    end;

  e.inhibit_p^.inh := false;           {init execution enabled following IF}

  escr_get_keyword (e, 'THEN', pick, stat); {read THEN keyword if present}
  if sys_error(stat) then return;
  if pick = 1 then begin               {THEN keyword used ?}
    e.inhibit_p^.inh := not cond;      {enable execution only if condition was true}
    e.inhibit_p^.if_flags :=           {THEN and command not allowed}
      e.inhibit_p^.if_flags + [escr_ifflag_nothen_k];
    end;
  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_THEN (E, STAT)
}
procedure escr_cmd_then (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inhty <> escr_inhty_if_k then begin {not in IF structure ?}
    sys_stat_set (escr_subsys_k, escr_err_notinif_k, stat);
    return;
    end;
  if not escr_get_end (e, stat) then return; {abort if anything on command line}
  if e.inhibit_p^.prev_p^.inh then return; {whole IF inhibited, just tracking nesting ?}

  if escr_ifflag_nothen_k in e.inhibit_p^.if_flags then begin {THEN not allowed here ?}
    sys_stat_set (escr_subsys_k, escr_err_then2_k, stat);
    return;
    end;
  e.inhibit_p^.if_flags :=             {no subsequent THEN command allowed this IF}
    e.inhibit_p^.if_flags + [escr_ifflag_nothen_k];

  e.inhibit_p^.inh :=                  {disable execution if condition was FALSE}
    not (escr_ifflag_true_k in e.inhibit_p^.if_flags);
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ELSE (E, STAT)
}
procedure escr_cmd_else (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inhty <> escr_inhty_if_k then begin {not in IF structure ?}
    sys_stat_set (escr_subsys_k, escr_err_notinif_k, stat);
    return;
    end;
  if not escr_get_end (e, stat) then return; {abort if anything on command line}
  if e.inhibit_p^.prev_p^.inh then return; {whole IF inhibited, just tracking nesting ?}

  if escr_ifflag_noelse_k in e.inhibit_p^.if_flags then begin {ELSE not allowed here ?}
    sys_stat_set (escr_subsys_k, escr_err_else2_k, stat);
    return;
    end;
  e.inhibit_p^.if_flags :=             {no subsequent THEN or ELSE command allowed this IF}
    e.inhibit_p^.if_flags + [escr_ifflag_nothen_k, escr_ifflag_noelse_k];

  e.inhibit_p^.inh :=                  {disable execution if condition was TRUE}
    escr_ifflag_true_k in e.inhibit_p^.if_flags;
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ENDIF (E, STAT)
}
procedure escr_cmd_endif (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inhty <> escr_inhty_if_k then begin {not in IF structure ?}
    sys_stat_set (escr_subsys_k, escr_err_notinif_k, stat);
    return;
    end;

  if not escr_get_end (e, stat) then return; {error on extra parameter}

  if                                   {neither THEN nor ELSE encountered ?}
      (not e.inhibit_p^.prev_p^.inh) and {whole IF command not inhibited ?}
      (not (escr_ifflag_nothen_k in e.inhibit_p^.if_flags)) and {no THEN ?}
      (not (escr_ifflag_noelse_k in e.inhibit_p^.if_flags)) {no ELSE ?}
      then begin
    sys_stat_set (escr_subsys_k, escr_err_nothenelse_k, stat);
    return;
    end;

  escr_inh_end (e, stat);              {remove the execution inhibit for the IF structure}
  end;
