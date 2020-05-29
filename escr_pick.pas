{   Implementation of PICK...ENDPICK blocks.
*
*   Commands:
*
*     PICK
*     CASE
*     CASENONE
*     QUITCASE
*     QUITPICK
*     ENDPICK
*
*   Functions:
*
*     PICK
*
*   PICK/ENDPICK is a type of execution block.  This type of block keeps
*   additional data in a ESCR_PICK_T structure, pointed to by the type-specific
*   pointer PICK_P in the block.  Only the code in this module looks inside the
*   ESCR_PICK_T structure.
}
module escr_pick;
define escr_cmd_pick;
define escr_cmd_endpick;
define escr_cmd_quitpick;
define escr_cmd_case;
define escr_cmd_casenone;
define escr_cmd_quitcase;
define escr_ifun_pick;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_CMD_PICK (E, STAT)
*
*   Command PICK ALL|FIRST [WITH value]
}
procedure escr_cmd_pick (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  pick_p: escr_pick_p_t;               {points to private PICK/ENDPICK block data}
  nkeyw: sys_int_machine_t;            {number of keyword picked from list}
  val: escr_val_t;                     {optional value supplied with PICK command}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  abort1;

begin
{
*   Create, init, and install the PICK/ENDPICK execution block.
}
  escr_exblock_new (e, stat);          {create new execution block state}
  if sys_error(stat) then return;
  e.exblock_p^.start_p :=              {save pointer to starting line of this block}
    e.exblock_p^.prev_p^.inpos_p^.last_p;
  e.exblock_p^.bltype := escr_exblock_pick_k; {indicate PICK/ENDPICK type}
  escr_exblock_inline_set (            {set next source line to execute}
    e, e.exblock_p^.prev_p^.inpos_p^.line_p, stat);
  if sys_error(stat) then return;

  if e.inhibit_p^.inh then return;     {execution inhibited ?}
{
*   Add our private state to the execution block.
}
  util_mem_grab (                      {allocate PICK block private data}
    sizeof(pick_p^), e.exblock_p^.mem_p^, false, pick_p);
  e.exblock_p^.pick_p := pick_p;       {link to from execution block}
  pick_p^.val_p := nil;                {init to optional value not supplied}
  pick_p^.ncase := 0;                  {init to no CASE commands}
  pick_p^.ntrue := 0;
  pick_p^.mode := escr_pickmode_first_k; {init to something valid}
{
*   Process the <opt> command parameter.
}
  escr_get_keyword (e,                 {get mode keyword and pick from list}
    'FIRST ALL',
    nkeyw, stat);
  if sys_error(stat) then goto abort1;
  case nkeyw of                        {which keyword was it ?}
0:  begin                              {no token ?}
      sys_stat_set (escr_subsys_k, escr_err_pick_noopt_k, stat);
      goto abort1;
      end;
1:  pick_p^.mode := escr_pickmode_first_k;
2:  pick_p^.mode := escr_pickmode_all_k;
    end;
{
*   Process optional "WITH value" clause.
}
  escr_get_keyword (e, 'WITH', nkeyw, stat); {try to read WITH keyword}
  if sys_error(stat) then goto abort1;
  if nkeyw <> 0 then begin             {something was there ?}
    if not escr_get_val (e, val, stat) then begin {get <value>}
      sys_stat_set (escr_subsys_k, escr_err_pick_nval_k, stat);
      goto abort1;
      end;
    if sys_error(stat) then goto abort1;
    escr_val_clone_min (               {clone value, use minimum memory}
      e,                               {overall ESCR library use state}
      val,                             {the value to clone}
      e.mem_p^,                        {context to get new memory from}
      false,                           {will not be individually deallcated}
      pick_p^.val_p);                  {returned pointer to cloned value}
    if not escr_get_end (e, stat)      {nothing more allowed on command line}
      then goto abort1;
    end;

  return;                              {normal return point}

abort1:                                {STAT set, block exists}
  escr_exblock_close (e, stat2);       {remove the execution block created above}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_ENDPICK (E, STAT)
*
*   End the block started with the last PICK command.
}
procedure escr_cmd_endpick (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

label
  del_block;

begin
  sys_error_none (stat);               {init to no error occurred}

  if e.exblock_p^.bltype <> escr_exblock_pick_k then begin {not in PICK block type ?}
    sys_stat_set (escr_subsys_k, escr_err_notpick_k, stat);
    return;
    end;
  if e.exblock_p^.inpos_p^.prev_p <> nil then begin {block end nested from start ?}
    sys_stat_set (escr_subsys_k, escr_err_endblock_include_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    return;
    end;
  if e.inhibit_p^.inh then goto del_block; {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  if sys_error(stat) then return;



  writeln ('Ending PICK block started on line ', e.exblock_p^.start_p^.lnum,
    ' of file "', e.exblock_p^.start_p^.file_p^.tnam.str:e.exblock_p^.start_p^.file_p^.tnam.len, '"');

  write ('  Mode ');
  case e.exblock_p^.pick_p^.mode of
escr_pickmode_all_k: write ('ALL');
escr_pickmode_first_k: write ('FIRST');
otherwise
    write (ord(e.exblock_p^.pick_p^.mode));
    end;
  write (', value ');
  if e.exblock_p^.pick_p^.val_p = nil
    then begin
      write ('--none--');
      end
    else begin
      case e.exblock_p^.pick_p^.val_p^.dtype of
escr_dtype_bool_k: begin
          write ('BOOL ');
          if e.exblock_p^.pick_p^.val_p^.bool
            then write ('true')
            else write ('false');
          end;
escr_dtype_int_k: begin
          write ('INT ', e.exblock_p^.pick_p^.val_p^.int);
          end;
escr_dtype_fp_k: begin
          write ('FP ', e.exblock_p^.pick_p^.val_p^.fp);
          end;
escr_dtype_str_k: begin
          write ('STR ', e.exblock_p^.pick_p^.val_p^.str.str:e.exblock_p^.pick_p^.val_p^.str.len);
          end;
escr_dtype_time_k: begin
          write ('TIME');
          end;
        end;
      end
    ;
  writeln;



del_block:                             {delete this block}
  e.exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    e.exblock_p^.inpos_p^.line_p;
  escr_exblock_close (e, stat);        {end this execution block}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_QUITPICK (E, STAT)
}
procedure escr_cmd_quitpick (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error occurred}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_CASE (E, STAT)
}
procedure escr_cmd_case (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error occurred}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_CASENONE (E, STAT)
}
procedure escr_cmd_casenone (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error occurred}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_QUITCASE (E, STAT)
}
procedure escr_cmd_quitcase (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error occurred}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFUN_PICK (E, STAT)
*
*   Function PICK.
}
procedure escr_ifun_pick(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  sys_error_none (stat);               {init to no error occurred}
  end;
