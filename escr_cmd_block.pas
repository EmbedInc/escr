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

begin
  escr_exblock_new (e);                {create new execution block state}
  e.exblock_p^.start_p :=              {save pointer to starting line of this block}
    e.exblock_p^.prev_p^.inpos_p^.last_p;
  e.exblock_p^.bltype := escr_exblock_blk_k; {indicate BLOCK ... ENBLOCK type}
   escr_exblock_inline_set (e,         {set next source line to execute}
    e.exblock_p^.prev_p^.inpos_p^.line_p);
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
  if e.inhibit_p^.inh then return;     {execution inhibited ?}

  if not escr_loop_iter(e) then begin  {loop terminated ?}
    escr_exblock_quit (e);             {leave block without executing anything}
    end;
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
  escr_exblock_quit (e);
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

begin
  if e.exblock_p^.prev_p = nil then begin {in root execution block}
    escr_err_atline (e, 'pic', 'err_endblock_root', nil, 0);
    end;
  if e.exblock_p^.bltype <> escr_exblock_blk_k then begin {not in BLOCK ... ENDBLOCK block type ?}
    escr_err_atline (e, 'pic', 'err_endblock_type', nil, 0);
    end;
  if e.exblock_p^.inpos_p^.prev_p <> nil then begin {block ended in include file ?}
    escr_err_atline (e, 'pic', 'err_endblock_include', nil, 0);
    end;

  e.exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    e.exblock_p^.inpos_p^.line_p;
  escr_exblock_close (e);              {end this BLOCK ... ENDBLOCK execution block}
  end;
