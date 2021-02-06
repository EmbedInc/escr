{   Execution block commands.
}
module escr_cmd_block;
define escr_cmd_block;
define escr_cmd_repeat;
define escr_cmd_quit;
define escr_cmd_endblock;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_BLOCK (E, STAT)
}
procedure escr_cmd_block (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  line_p: fline_line_p_t;              {pointer to input line}

begin
  line_p := escr_in_line (e);          {get pointer to current input line}

  escr_exblock_new (e, stat);          {create new execution block state}
  if sys_error(stat) then return;
  e.exblock_p^.bltype := escr_exblock_blk_k; {indicate BLOCK ... ENBLOCK type}
  escr_exblock_goto_line (e, line_p, stat); {init input line for the new block}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_REPEAT (E, STAT)
}
procedure escr_cmd_repeat (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}
  if e.inhibit_p^.inh then return;     {execution inhibited ?}

  if escr_loop_iter(e, stat) then begin {back for another iteration ?}
    escr_exblock_repeat (e, stat);     {go back to start of block}
    return;
    end;

  if sys_error(stat) then return;
  escr_exblock_quit_curr (e);          {leave block without executing anything}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_QUIT (E, STAT)
}
procedure escr_cmd_quit (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution inhibited ?}
  escr_exblock_quit_curr (e);
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ENDBLOCK (E, STAT)
}
procedure escr_cmd_endblock (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  line_p: fline_line_p_t;              {pointer to input line}

begin
  if e.exblock_p^.prev_p = nil then begin {in root execution block}
    sys_stat_set (escr_subsys_k, escr_err_endblock_root_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    end;
  if e.exblock_p^.bltype <> escr_exblock_blk_k then begin {not in BLOCK ... ENDBLOCK block type ?}
    sys_stat_set (escr_subsys_k, escr_err_endblock_type_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    end;
  if fline_block_level(e.exblock_p^.instk_p) > 0 then begin {blk end in include file ?}
    sys_stat_set (escr_subsys_k, escr_err_endblock_include_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    end;

  line_p := escr_in_line (e);          {get pointer to curr input line}
  escr_exblock_close (e, stat);        {end this BLOCK ... ENDBLOCK execution block}
  escr_exblock_goto_line (e, line_p, stat); {restart parent block here}
  end;
