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
*   Subroutine ESCR_CMD_BLOCK (STAT)
}
procedure escr_cmd_block (
  out     stat: sys_err_t);
  val_param;

begin
  escr_exblock_new;                    {create new execution block state}
  e.exblock_p^.start_p :=              {save pointer to starting line of this block}
    e.exblock_p^.prev_p^.inpos_p^.last_p;
  e.exblock_p^.bltype := escr_exblock_blk_k; {indicate BLOCK ... ENBLOCK type}
  escr_exblock_inline_set (            {set next source line to execute}
    e.exblock_p^.prev_p^.inpos_p^.line_p);
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
  if e.inhibit_p^.inh then return;     {execution inhibited ?}

  if not loop_iter then begin          {loop terminated ?}
    escr_exblock_quit;                 {leave block without executing anything}
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
  if e.inhibit_p^.inh then return;     {execution inhibited ?}
  escr_exblock_quit;
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
  if e.exblock_p^.prev_p = nil then begin {in root execution block}
    escr_err_atline ('pic', 'err_endblock_root', nil, 0);
    end;
  if e.exblock_p^.bltype <> escr_exblock_blk_k then begin {not in BLOCK ... ENDBLOCK block type ?}
    escr_err_atline ('pic', 'err_endblock_type', nil, 0);
    end;
  if e.exblock_p^.inpos_p^.prev_p <> nil then begin {block ended in include file ?}
    escr_err_atline ('pic', 'err_endblock_include', nil, 0);
    end;

  e.exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    e.exblock_p^.inpos_p^.line_p;
  escr_exblock_close;                  {end this BLOCK ... ENDBLOCK execution block}
  end;
