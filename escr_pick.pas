{   Implementation of PICK...ENDPICK blocks.
*
*   Commands:
*
*     PICK
*     OPTION
*     OPTIONIF
*     OPTIONELSE
*     QUITOPT
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
define escr_cmd_option;
define escr_cmd_optionif;
define escr_cmd_optionelse;
define escr_cmd_quitopt;
define escr_ifun_pick;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Local subroutine ERR_NOPICK (E, STAT)
*
*   Set error to "Not in a PICK/ENDPICK block".
}
procedure err_nopick (                 {set "not in PICK block" error}
  in      e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {returned error status}
  val_param; internal;

begin
  sys_stat_set (escr_subsys_k, escr_err_notpick_k, stat); {not in PICK block}
  end;
{
********************************************************************************
*
*   Local function PICK_BLOCK (E, BLK_P)
*
*   Find the inner-most PICK/ENDPICK block.  If a PICK block is found, then the
*   function returns TRUE, and BLK_P will point to the block.  If no PICK block
*   is found, then the function returns FALSE, and BLK_P is set to NIL.
*
*   This routine must only be called when the PICK block was fully created,
*   which means execution must not have been inhibited at the PICK command.
*   When execution of the PICK command is inhibited, then no PICK-specific
*   descriptor is created and the PICK_P pointer in the execution block is NIL.
*   This routine returns with error when PICK_P is NIL.
}
function pick_block (                  {find the relevant PICK block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     blk_p: escr_exblock_p_t)     {returned pointing to the PICK block}
  :boolean;                            {block found, BLK_P not nil}
  val_param; internal;

begin
  pick_block := true;                  {init to PICK block found}

  blk_p := e.exblock_p;                {init to current block}
  while blk_p <> nil do begin          {scan upwards thru block nesting hierarchy}
    if blk_p^.bltype = escr_exblock_pick_k then begin {found lowest PICK block ?}
      if blk_p^.pick_p = nil then begin {just a skeleton PICK block ?}
        escr_err_atline (e, 'escr', 'pick_nopick', nil, 0);
        end;
      return;
      end;
    blk_p := blk_p^.prev_p;            {to next higher level block}
    end;                               {back to check this new block}

  pick_block := false;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_PICK (E, STAT)
*
*   Command PICK ONE|FIRST|ALL [BY value]
}
procedure escr_cmd_pick (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  line_p: fline_line_p_t;              {pointer to current input line}
  pick_p: escr_pick_p_t;               {points to private PICK/ENDPICK block data}
  nkeyw: sys_int_machine_t;            {number of keyword picked from list}
  val: escr_val_t;                     {optional value supplied with PICK command}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  abort1;

begin
  line_p := escr_in_line (e);          {save pointer to the current input line}
{
*   Create, init, and install the PICK/ENDPICK execution block.
}
  escr_exblock_new (e, stat);          {create new execution block state}
  if sys_error(stat) then return;
  e.exblock_p^.start_p := line_p;      {set pointer to starting line of this block}
  e.exblock_p^.bltype := escr_exblock_pick_k; {indicate PICK/ENDPICK type}
  e.exblock_p^.pick_p := nil;          {init to no PICK descriptor}
  escr_exblock_goto_line_aft (         {set next source line to execute}
    e, line_p, stat);
  if sys_error(stat) then return;

  if e.inhibit_p^.inh then return;     {execution inhibited ?}

  escr_inh_new (e);                    {add PICK OPTION execution inhibit}
  e.inhibit_p^.inhty := escr_inhty_opt_k;
{
*   Add our private state to the execution block.
}
  util_mem_grab (                      {allocate PICK block private data}
    sizeof(pick_p^), e.exblock_p^.mem_p^, false, pick_p);
  e.exblock_p^.pick_p := pick_p;       {link to from execution block}
  pick_p^.choice_p := nil;             {init to optional choice value not supplied}
  pick_p^.nopt := 0;                   {init to no options found}
  pick_p^.nrun := 0;
  pick_p^.mode := escr_pickmode_one_k; {init to something valid}
{
*   Process the <opt> command parameter.
}
  escr_get_keyword (e,                 {get mode keyword and pick from list}
    'ONE FIRST ALL',
    nkeyw, stat);
  if sys_error(stat) then goto abort1;
  case nkeyw of                        {which keyword was it ?}
0:  begin                              {no token ?}
      sys_stat_set (escr_subsys_k, escr_err_pick_noopt_k, stat);
      goto abort1;
      end;
1:  pick_p^.mode := escr_pickmode_one_k;
2:  pick_p^.mode := escr_pickmode_first_k;
3:  pick_p^.mode := escr_pickmode_all_k;
    end;
{
*   Process optional "BY choice" clause.
}
  escr_get_keyword (e, 'BY', nkeyw, stat); {try to read BY keyword}
  if sys_error(stat) then goto abort1;
  if nkeyw <> 0 then begin             {something was there ?}
    if not escr_get_val (e, val, stat) then begin {get <value>}
      sys_stat_set (escr_subsys_k, escr_err_pickcmd_nchoice_k, stat);
      goto abort1;
      end;
    if sys_error(stat) then goto abort1;
    escr_val_clone_min (               {clone value, use minimum memory}
      e,                               {overall ESCR library use state}
      val,                             {the value to clone}
      e.exblock_p^.mem_p^,             {context to get new memory from}
      false,                           {will not be individually deallcated}
      pick_p^.choice_p);               {returned pointer to cloned value}
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

var
  line_p: fline_line_p_t;              {pointer to current input line}

label
  del_block;

begin
  if e.exblock_p^.bltype <> escr_exblock_pick_k then begin {not in PICK block type ?}
    sys_stat_set (escr_subsys_k, escr_err_notpick_k, stat);
    return;
    end;
  if fline_hier_level(e.exblock_p^.instk_p) > 0 then begin {block end nested from start ?}
    sys_stat_set (escr_subsys_k, escr_err_endblock_include_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    return;
    end;
  if e.inhibit_p^.inh then goto del_block; {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  if sys_error(stat) then return;

del_block:                             {delete this block}
  line_p := escr_in_line (e);          {get pointer to the current input line}
  escr_exblock_close (e, stat);        {end this execution block}
  escr_exblock_goto_line_aft (e, line_p, stat); {restart prev block on next line}
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

var
  blk_p: escr_exblock_p_t;             {pointer to the PICK block}

begin
  if e.inhibit_p^.inh then begin       {execution is currently inhibited ?}
    if e.inhibit_p^.inhty <> escr_inhty_opt_k then return; {not PICK OPTION inhibit ?}
    e.inhibit_p^.inh := e.inhibit_p^.prev_p^.inh; {revert to parent's inhibit}
    if e.inhibit_p^.inh then return;   {execution inhibited at a higher level ?}
    end;

  if not pick_block (e, blk_p) then begin {get pointer to PICK block}
    err_nopick (e, stat);
    return;
    end;

  escr_exblock_quit_blks (e, blk_p^);  {inhibit execution until end of PICK block}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_OPTION (E, STAT)
*
*   Command OPTION value ... value
}
procedure escr_cmd_option (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  max_msg_args = 2;                    {max arguments we can pass to a message}

var
  blk_p: escr_exblock_p_t;             {pointer to the PICK block}
  inh_p: escr_inh_p_t;                 {pointer to execution inhibit}
  val: escr_val_t;                     {value of command parameter}
  nval: sys_int_machine_t;             {number of VALUE parameters found}
  rallowed: boolean;                   {allowed to run this option}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;

label
  inhibit, run;

begin
{
*   Check the execution inhibit.  Any previous inhibit due to PICK OPTION ends
*   here and will be evaluated anew.
}
  if e.inhibit_p^.inh then begin       {execution is currently inhibited ?}
    if e.inhibit_p^.inhty <> escr_inhty_opt_k then return; {not PICK OPTION inhibit ?}
    e.inhibit_p^.inh := e.inhibit_p^.prev_p^.inh; {revert to parent's inhibit}
    if e.inhibit_p^.inh then return;   {execution inhibited at a higher level ?}
    escr_inline_expand_rest (e, stat); {expand functions on rest of input line}
    if sys_error(stat) then return;
    end;

  if not pick_block (e, blk_p) then begin {get pointer to the PICK block}
    err_nopick (e, stat);
    return;
    end;

  if blk_p^.pick_p^.choice_p = nil then begin {no choice supplied to PICK command ?}
    sys_stat_set (escr_subsys_k, escr_err_pick_nchoice_k, stat);
    return;
    end;

  blk_p^.pick_p^.nopt := blk_p^.pick_p^.nopt + 1; {one more option this PICK}
{
*   Handle the PICK mode.  RALLOWED is set according to whether this option is
*   allowed to run.
}
  rallowed := true;                    {init to allowed to run this option}
  case blk_p^.pick_p^.mode of          {which PICK mode is in effect ?}

escr_pickmode_one_k: begin             {only one option allowed}
      rallowed := blk_p^.pick_p^.nrun <= 0; {no previous option was run ?}
      end;

escr_pickmode_first_k: begin           {only run first opt, but others are allowed}
      if blk_p^.pick_p^.nrun > 0 then begin {another option was already run ?}
        e.parse_p^.ip := e.parse_p^.ibuf.len + 1; {indicate input line used up}
        goto inhibit;
        end;
      end;

escr_pickmode_all_k: begin             {run all options with valid conditions}
      end;

otherwise                              {unexpected PICK mode}
    sys_msg_parm_int (msg_parm[1], ord(blk_p^.pick_p^.mode));
    sys_msg_parm_str (msg_parm[2], 'ESCR_CMD_OPTION');
    escr_err_atline (e, 'escr', 'pick_mode_unimpl', msg_parm, 2);
    end;                               {end of PICK block MODE cases}
{
*   Get each VALUE parameter.  If it matches the PICK command choice, then go to
*   RUN.  If no values match, then this section falls thru to INHIBIT.
}
  nval := 0;                           {init to no VALUE parameter found}
  escr_val_init (e, blk_p^.pick_p^.choice_p^.dtype, val); {set target dtype and init VAL}

  while true do begin                  {back here each new VALUE parameter}
    if not escr_get_val_dtype (e, val, stat) then begin {get VALUE parameter into VAL}
      if sys_error(stat) then return;  {error getting argument ?}
      exit;                            {done with all VALUE parameters}
      end;
    nval := nval + 1;                  {count one more VALUE parameter}
    {
    *  Run this option if VAL matches the PICK block choice.
    }
    case val.dtype of                  {what data type is it ?}
escr_dtype_bool_k: begin
        if val.bool = blk_p^.pick_p^.choice_p^.bool then begin
          goto run;
          end;
        end;
escr_dtype_int_k: begin
        if val.int = blk_p^.pick_p^.choice_p^.int then begin
          goto run;
          end;
        end;
escr_dtype_fp_k: begin
        if val.fp = blk_p^.pick_p^.choice_p^.fp then begin
          goto run;
          end;
        end;
escr_dtype_str_k: begin
        if string_equal(val.str, blk_p^.pick_p^.choice_p^.str) then begin
          goto run;
          end;
        end;
escr_dtype_time_k: begin
        if sys_clock_compare(val.time, blk_p^.pick_p^.choice_p^.time) = sys_compare_eq_k
            then begin
          goto run;
          end;
        end;
otherwise
      escr_err_dtype_unimp (e, val.dtype, 'ESCR_CMD_OPTION');
      end;                             {end of value data type cases}
    end;                               {back for next VALUE parameter}

  if nval <= 0 then begin              {no VALUE parameter at all ?}
    escr_stat_cmd_noarg (e, stat);     {set missing argument error}
    return;
    end;
{
*   Everything checks out, but don't run this option.
}
inhibit:                               {inhibit execution of this option}
  inh_p := e.inhibit_p;                {init to lowest execution inhibit}
  while true do begin                  {inhibit all up to and including OPTION}
    inh_p^.inh := true;                {inhibit execution at this level}
    if inh_p^.inhty = escr_inhty_opt_k then exit; {reached the OPTION inhibit ?}
    inh_p := inh_p^.prev_p;            {no, up to next higher inhibit}
    end;
  return;
{
*   Run this option.
}
run:                                   {run this option}
  if not rallowed then begin           {not allowed to run this option due to mode ?}
    sys_stat_set (escr_subsys_k, escr_err_pick_more1_k, stat);
    return;
    end;
  e.parse_p^.ip := e.parse_p^.ibuf.len + 1; {indicate input line used up}
  blk_p^.pick_p^.nrun := blk_p^.pick_p^.nrun + 1; {count one more option run}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_OPTIONIF (E, STAT)
*
*   Command OPTIONIF condition
}
procedure escr_cmd_optionif (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  max_msg_args = 2;                    {max arguments we can pass to a message}

var
  blk_p: escr_exblock_p_t;             {pointer to the PICK block}
  inh_p: escr_inh_p_t;                 {pointer to execution inhibit}
  rallowed: boolean;                   {allowed to run this option}
  optrue: boolean;                     {condition is TRUE}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;

label
  inhibit;

begin
{
*   Check the execution inhibit.  Any previous inhibit due to PICK OPTION ends
*   here and will be evaluated anew.
}
  if e.inhibit_p^.inh then begin       {execution is currently inhibited ?}
    if e.inhibit_p^.inhty <> escr_inhty_opt_k then return; {not PICK OPTION inhibit ?}
    e.inhibit_p^.inh := e.inhibit_p^.prev_p^.inh; {revert to parent's inhibit}
    if e.inhibit_p^.inh then return;   {execution inhibited at a higher level ?}
    escr_inline_expand_rest (e, stat); {expand functions on rest of input line}
    if sys_error(stat) then return;
    end;

  if not pick_block (e, blk_p) then begin {get pointer to the PICK block}
    err_nopick (e, stat);
    return;
    end;

  blk_p^.pick_p^.nopt := blk_p^.pick_p^.nopt + 1; {one more option this PICK}
{
*   Handle the PICK mode.  RALLOWED is set according to whether this option is
*   allowed to run.
}
  rallowed := true;                    {init to allowed to run this option}
  case blk_p^.pick_p^.mode of          {which PICK mode is in effect ?}

escr_pickmode_one_k: begin             {only one option allowed}
      rallowed := blk_p^.pick_p^.nrun <= 0; {no previous option was run ?}
      end;

escr_pickmode_first_k: begin           {only run first opt, but others are allowed}
      if blk_p^.pick_p^.nrun > 0 then begin {another option was already run ?}
        e.parse_p^.ip := e.parse_p^.ibuf.len + 1; {indicate input line used up}
        goto inhibit;
        end;
      end;

escr_pickmode_all_k: begin             {run all options with valid conditions}
      end;

otherwise                              {unexpected PICK mode}
    sys_msg_parm_int (msg_parm[1], ord(blk_p^.pick_p^.mode));
    sys_msg_parm_str (msg_parm[2], 'ESCR_CMD_OPTION');
    escr_err_atline (e, 'escr', 'pick_mode_unimpl', msg_parm, 2);
    end;
{
*   Execute this option if the condition is TRUE.
}
  if not escr_get_bool (e, optrue, stat) then begin {evaluate the condition}
    if sys_error(stat) then return;    {already have hard error ?}
    escr_stat_cmd_noarg (e, stat);     {set missing argument error}
    return;
    end;
  if not escr_get_end (e, stat) then return; {no additional parameters allowed}

  if not optrue then goto inhibit;     {condition is false, don't execute case ?}
{
*   Run this option.
}
  if not rallowed then begin           {not allowed to run this option due to mode ?}
    sys_stat_set (escr_subsys_k, escr_err_pick_more1_k, stat);
    return;
    end;

  blk_p^.pick_p^.nrun := blk_p^.pick_p^.nrun + 1; {count one more case run}
  return;                              {normal return point to execute the case}
{
*   Don't run this option.
}
inhibit:                               {inhibit execution of this case}
  inh_p := e.inhibit_p;                {init to lowest execution inhibit}
  while true do begin                  {inhibit all up to and including OPTION}
    inh_p^.inh := true;                {inhibit execution at this level}
    if inh_p^.inhty = escr_inhty_opt_k then exit; {reached the OPTION inhibit ?}
    inh_p := inh_p^.prev_p;            {no, up to next higher inhibit}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_OPTIONELSE (E, STAT)
*
*   Command OPTIONELSE
*
*   Execute this option if no previous option executed.
}
procedure escr_cmd_optionelse (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  blk_p: escr_exblock_p_t;             {pointer to the PICK block}
  inh_p: escr_inh_p_t;                 {pointer to execution inhibit}

begin
  sys_error_none (stat);               {init to no error encountered}
{
*   Check the execution inhibit.  Any previous inhibit due to PICK option ends
*   here and will be evaluated anew.
}
  if e.inhibit_p^.inh then begin       {execution is currently inhibited ?}
    if e.inhibit_p^.inhty <> escr_inhty_opt_k then return; {not PICK OPTION inhibit ?}
    e.inhibit_p^.inh := e.inhibit_p^.prev_p^.inh; {revert to parent's inhibit}
    if e.inhibit_p^.inh then return;   {execution inhibited at a higher level ?}
    end;

  if not pick_block (e, blk_p) then begin {get pointer to the PICK block}
    err_nopick (e, stat);
    return;
    end;

  blk_p^.pick_p^.nopt := blk_p^.pick_p^.nopt + 1; {one more option this PICK}
{
*   Run this case if no previous option was run.
}
  if blk_p^.pick_p^.nrun = 0 then begin {no previous option run, run this one ?}
    blk_p^.pick_p^.nrun := blk_p^.pick_p^.nrun + 1; {count one more option run}
    return;                            {run this option}
    end;
{
*   Do not run this option.
}
  inh_p := e.inhibit_p;                {init to lowest execution inhibit}
  while true do begin                  {inhibit all up to and including OPT}
    inh_p^.inh := true;                {inhibit execution at this level}
    if inh_p^.inhty = escr_inhty_opt_k then exit; {reached the OPT inhibit ?}
    inh_p := inh_p^.prev_p;            {no, up to next higher inhibit}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_QUITOPT (E, STAT)
}
procedure escr_cmd_quitopt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  blk_p: escr_exblock_p_t;             {pointer to the PICK block}
  inh_p: escr_inh_p_t;                 {pointer to execution inhibit}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if not pick_block (e, blk_p) then begin {not in a PICK block ?}
    err_nopick (e, stat);
    return;
    end;

  inh_p := e.inhibit_p;                {init to lowest execution inhibit}
  while true do begin                  {inhibit all up to and including OPTION}
    inh_p^.inh := true;                {inhibit execution at this level}
    if inh_p^.inhty = escr_inhty_opt_k then exit; {reached the OPT inhibit ?}
    inh_p := inh_p^.prev_p;            {no, up to next higher inhibit}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFUN_PICK (E, STAT)
*
*   Function PICK VAL|VALIS|NRUN|NOPT|IN
}
procedure escr_ifun_pick(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var32_t;                  {scratch token}
  keyn: sys_int_machine_t;             {number of keyword picked from list}
  blk_p: escr_exblock_p_t;             {pointer to innermost PICK block}

label
  nopick;

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error occurred}

  if escr_ifn_get_keyw (e, tk, stat)
    then begin
      string_tkpick80 (tk,
        'VAL VALIS NRUN NOPT IN',
        keyn);
      end
    else begin
      keyn := 1;
      end
    ;
  case keyn of                         {which option is it ?}

1:  begin                              {PICK VAL (default)}
      if not pick_block (e, blk_p) then goto nopick;
      if blk_p^.pick_p^.choice_p = nil then begin {no choice supplied to PICK command ?}
        sys_stat_set (escr_subsys_k, escr_err_pick_nchoice_k, stat);
        return;
        end;
      escr_ifn_ret_val (e, blk_p^.pick_p^.choice_p^); {return the value}
      end;

2:  begin                              {PICK VALIS}
      if not pick_block (e, blk_p) then goto nopick;
      escr_ifn_ret_bool (e, blk_p^.pick_p^.choice_p <> nil);
      end;

3:  begin                              {PICK NRUN}
      if not pick_block (e, blk_p) then goto nopick;
      escr_ifn_ret_int (e, blk_p^.pick_p^.nrun);
      end;

4:  begin                              {PICK NOPT}
      if not pick_block (e, blk_p) then goto nopick;
      escr_ifn_ret_int (e, blk_p^.pick_p^.nopt);
      end;

5:  begin                              {PICK IN}
      escr_ifn_ret_bool (e, pick_block (e, blk_p));
      end;

otherwise
    escr_ifn_bad_keyw (e, tk, stat);   {set bad keyword error}
    end;
  return;                              {normal return point}

nopick:                                {not in a PICK block}
  err_nopick (e, stat);                {set Not in PICK block error}
  end;
