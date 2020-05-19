{   Commands that manage commands.
}
module escr_cmd_cmd;
define escr_cmd_command;
define escr_cmd_endcmd;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   command ESCR_CMD_COMMAND (E, STAT)
*
*   Command COMMAND name
*
*   Define the start of a command.  The command definition is stored, but the
*   command is not executed now.
}
procedure escr_cmd_command (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {command name}
  sym_p: escr_sym_p_t;                 {pointer to new command name symbol}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  error;

begin
  name.max := size_char(name.str);     {init local var string}

  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_blkdef_k; {inhibit is due to reading block definition}
  e.inhibit_p^.blkdef_type := escr_exblock_cmd_k; {block type is command}
  if e.inhibit_p^.inh then return;     {previously inhibited, don't define command}
  e.inhibit_p^.inh := true;            {inhibit execution during command definition}

  if not escr_get_token (e, name) then begin {get command name}
    escr_stat_cmd_noarg (e, stat);
    goto error;
    end;

  escr_sym_new_cmd (                   {create the new command symbol}
    e,                                 {ESCR library use state}
    name,                              {bare symbol name}
    e.exblock_p^.inpos_p^.last_p,      {starting line of command}
    false,                             {make symbol local, not global}
    sym_p,                             {returned pointer to the new symbol}
    stat);
  return;

error:                                 {error after inhibit created, STAT set}
  escr_inh_end (e, stat2);             {delete the execution inhibit}
  end;
{
********************************************************************************
*
*   command ESCR_CMD_ENDCMD (E, STAT)
*
*   Command ENDCMD
*
*   Indicate the end of a command definition.
}
procedure escr_cmd_endcmd (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if not e.inhibit_p^.inh then begin   {executing code normally ?}
    escr_cmd_return (e, stat);         {acts just like RETURN}
    return;
    end;

  if                                   {not defining a command ?}
      (e.inhibit_p^.inhty <> escr_inhty_blkdef_k) or {not in a block definition ?}
      (e.inhibit_p^.blkdef_type <> escr_exblock_cmd_k) {block is not a command ?}
      then begin
    sys_stat_set (escr_subsys_k, escr_err_notcmddef_k, stat);
    return;
    end;

  escr_inh_end (e, stat);              {end excution inhibit due to command def}
  end;
