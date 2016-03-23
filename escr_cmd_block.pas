{   Execution block commands.
}
module escr_cmd_block;
define escr_cmd_block;
define escr_cmd_repeat;
define escr_cmd_quit;
define escr_cmd_endblock;
%include 'escr.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_CMD_BLOCK (STAT)
}
procedure escr_cmd_block (
  out     stat: sys_err_t);
  val_param;

begin
  exblock_new;                         {create new execution block state}
  exblock_p^.start_p :=                {save pointer to starting line of this block}
    exblock_p^.prev_p^.inpos_p^.last_p;
  exblock_p^.bltype := exblock_blk_k;  {indicate BLOCK ... ENBLOCK type}
  exblock_inline_set (                 {set next source line to execute}
    exblock_p^.prev_p^.inpos_p^.line_p);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_REPEAT (STAT)
}
procedure escr_cmd_repeat (
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution inhibited ?}

  if not loop_iter then begin          {loop terminated ?}
    exblock_quit;                      {leave block without executing anything}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_QUIT (STAT)
}
procedure escr_cmd_quit (
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution inhibited ?}
  exblock_quit;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_ENDBLOCK (STAT)
}
procedure escr_cmd_endblock (
  out     stat: sys_err_t);
  val_param;

begin
  if exblock_p^.prev_p = nil then begin {in root execution block}
    err_atline ('pic', 'err_endblock_root', nil, 0);
    end;
  if exblock_p^.bltype <> exblock_blk_k then begin {not in BLOCK ... ENDBLOCK block type ?}
    err_atline ('pic', 'err_endblock_type', nil, 0);
    end;
  if exblock_p^.inpos_p^.prev_p <> nil then begin {block ended in include file ?}
    err_atline ('pic', 'err_endblock_include', nil, 0);
    end;

  exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    exblock_p^.inpos_p^.line_p;
  exblock_close;                       {end this BLOCK ... ENDBLOCK execution block}
  end;
