{   Commands related to loops.
}
module escr_cmd_loop;
define escr_cmd_loop;
define escr_loop_iter;
define escr_cmd_endloop;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_LOOP (E, STAT)
*
*   /LOOP
*   /LOOP SYMBOLS varname
}
procedure escr_cmd_loop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  loop_p: escr_loop_p_t;               {pointer to loop descriptor}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  name: string_var80_t;                {variable name}
  tk: string_var32_t;                  {scratch token}
  name_p: string_var_p_t;              {scratch string pointer}
  sym_p: escr_sym_p_t;                 {pointer to symbol name}
  sym_pp: escr_sym_pp_t;               {pointer to data in symbol table entry}
  slist_p: string_list_p_t;            {points to strings list}
  pos: string_hash_pos_t;              {symbol table position handle}
  found: boolean;                      {symbol table entry found}

label
  loop_from, err_missing;

begin
  name.max := size_char(name.str);     {init local var string}
  tk.max := size_char(tk.str);
  sys_error_none (stat);               {init to no error occurred}

  escr_exblock_new (e);                {create new execution block state}
  e.exblock_p^.start_p :=              {save pointer to starting line of this block}
    e.exblock_p^.prev_p^.inpos_p^.last_p;
  e.exblock_p^.bltype := escr_exblock_loop_k; {indicate LOOP ... ENDLOOP type}
  escr_exblock_inline_set (            {set next source line to execute}
    e, e.exblock_p^.prev_p^.inpos_p^.line_p);

  if e.inhibit_p^.inh then return;     {execution inhibited ?}

  escr_get_keyword (e, 'SYMBOLS FOR FROM N', pick, stat); {get looptype keyword}
  if sys_error(stat) then return;

  util_mem_grab (                      {allocate loop descriptor}
    sizeof(loop_p^), e.exblock_p^.mem_p^, false, loop_p);
  loop_p^.looptype := escr_looptype_unc_k; {init loop descriptor to default}
  loop_p^.var_p := nil;                {init to no loop variable}

  case pick of
{
******************************
*
*   /LOOP with no parameters.
}
0: begin
  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  end;
{
******************************
*
*   /LOOP SYMBOLS
}
1: begin
  loop_p^.looptype := escr_looptype_sym_k; {looping over list of symbols}

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then goto err_missing;
  if not escr_get_end (e, stat) then return; {abort on extra parameter}
{
*   Create the local list of symbol names.
}
  util_mem_grab (                      {allocate symbol names list}
    sizeof(slist_p^), e.exblock_p^.mem_p^, false, slist_p);
  loop_p^.sym_list_p := slist_p;       {save pointer to names list}

  string_list_init (slist_p^, e.exblock_p^.mem_p^); {init names list}
  slist_p^.deallocable := false;       {not individually deacllocatable}

  string_hash_pos_first (e.sym_var.hash, pos, found);
  while found do begin                 {once for each symbol in the symbol table}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get info from this table entry}
    sym_p := sym_pp^;                  {get pointer to this symbol}
    slist_p^.size := sym_p^.name_p^.len; {set size of this new string}
    string_list_line_add (slist_p^);   {create the new names list entry}
    string_copy (sym_p^.name_p^, slist_p^.str_p^); {fill in this list entry}
    string_hash_pos_next (pos, found); {advance to next symbol table entry}
    end;

  string_list_sort (                   {sort the list of symbol names}
    slist_p^,                          {the list to sort}
    [string_comp_num_k]);              {compare numeric fields numerically}
{
*   Initialze the loop state to the first names list entry.
}
  escr_sym_new_var (                   {create the loop variable}
    e,                                 {state for this use of the ESCR system}
    name,                              {name of variable to create}
    escr_dtype_str_k,                  {variable's data type}
    escr_max_namelen_k,                {max length}
    false,                             {local, not global}
    sym_p,                             {returned pointer to the new variable}
    stat);
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  loop_p^.var_p := sym_p;              {save pointer to the loop variable}

  string_list_pos_abs (slist_p^, 1);   {go to first list entry}

  if slist_p^.str_p = nil then begin   {the list is empty ?}
    e.inhibit_p^.inh := true;          {inhibit execution for this block}
    return;
    end;

  string_copy (                        {init loop value to first symbol name}
    slist_p^.str_p^, loop_p^.var_p^.var_val.str);
  end;
{
******************************
*
*   /LOOP FOR var FROM n TO m [BY k]
}
2: begin
  if not escr_get_token (e, name)      {get the variable name into NAME}
    then goto err_missing;

  escr_get_keyword (e, 'FROM', pick, stat);
  if sys_error(stat) then return;
loop_from:                             {common code with /LOOP FROM}
  if not escr_get_int (e, loop_p^.for_start, stat)
    then goto err_missing;

  escr_get_keyword (e, 'TO', pick, stat);
  if sys_error(stat) then return;
  if not escr_get_int (e, loop_p^.for_end, stat)
    then goto err_missing;

  if loop_p^.for_end >= loop_p^.for_start {set default increment from loop direction}
    then loop_p^.for_inc := 1
    else loop_p^.for_inc := -1;

  escr_get_keyword (e, 'BY', pick, stat);
  if sys_error(stat) then return;
  if pick = 1 then begin               {BY clause exists ?}
    if not escr_get_int (e, loop_p^.for_inc, stat)
      then goto err_missing;
    if not escr_get_end (e, stat) then return; {abort on extra parameter}
    end;
  {
  *   The /LOOP command line has been parsed.  NAME is the name of the temporary
  *   variable to create as the loop counter, or it is the empty string to
  *   incidate no variabe is to be created.
  *
  *   The loop descriptor fields FOR_START, FOR_END, and FOR_INC have been
  *   filled in.
  }
  if loop_p^.for_inc = 0 then begin    {invalid iteration increment ?}
    escr_err_atline (e, 'pic', 'err_loopinc0', nil, 0);
    end;
  loop_p^.looptype := escr_looptype_for_k; {FOR loop}
  loop_p^.for_curr := loop_p^.for_start; {init value for first iteration}

  if
      (loop_p^.for_inc >= 0) and       {counting up ?}
      (loop_p^.for_curr > loop_p^.for_end) {already past the end ?}
      then begin
    e.inhibit_p^.inh := true;          {inhibit execution for this block}
    end;
  if
      (loop_p^.for_inc < 0) and        {counting down ?}
      (loop_p^.for_curr < loop_p^.for_end) {already past the end ?}
      then begin
    e.inhibit_p^.inh := true;          {inhibit execution for this block}
    end;
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if name.len > 0 then begin           {need to create loop variable ?}
    escr_sym_new_var (                 {create the loop variable}
      e,                               {state for this use of the ESCR system}
      name,                            {name of the variable to create}
      escr_dtype_int_k,                {data type will be integer}
      0,                               {additional length parameter, unused}
      false,                           {variable will be local, not global}
      sym_p,                           {returned pointer to the new variable symbol}
      stat);
    escr_err_atline_abort (e, stat, '', '', nil, 0);
    loop_p^.var_p := sym_p;            {save pointer to the loop variable}
    sym_p^.var_val.int := loop_p^.for_curr; {init to value for first iteration}
    end;
  end;
{
******************************
*
*   /LOOP FROM n TO m [BY k]
}
3: begin
  name.len := 0;                       {indicate to not create a loop variable}
  goto loop_from;                      {to common code with /LOOP FOR}
  end;
{
******************************
*
*   /LOOP N n
}
4: begin
  loop_p^.looptype := escr_looptype_for_k; {this will be FOR loop with no variable}
  loop_p^.for_start := 1;              {starting value}
  loop_p^.for_curr := 1;               {value for first iteration}
  loop_p^.for_inc := 1;                {increment each iteration}

  if not escr_get_int (e, loop_p^.for_end, stat) {get number of times to loop}
    then goto err_missing;

  if loop_p^.for_end < 1 then begin    {will do 0 iterations ?}
    e.inhibit_p^.inh := true;          {inhibit execution for this block}
    return;
    end;
  end;
{
******************************
}
    end;                               {end of looptype cases}

  e.exblock_p^.loop_p := loop_p;       {add loop descriptor to this execution block}
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  if sys_error(stat) then return;      {return with any previous hard error}
  sys_stat_set (escr_subsys_k, escr_err_missingparm_k, stat);
  end;
{
********************************************************************************
*
*   Function LOOP_ITER
*
*   Advance the loop state to the next iteration.  If the terminating condition
*   is met, then the function returns FALSE and the execution location is not
*   altered.  If the terminating condition is not met, then the execution point
*   is set to the first line within the current execution block.
}
function escr_loop_iter (              {advance to next loop iteration}
  in out  e: escr_t)                   {state for this use of the ESCR system}
  :boolean;                            {looped back, not terminated}
  val_param;

var
  loop_p: escr_loop_p_t;               {pointer to loop descriptor}
  sym_p: escr_sym_p_t;                 {scratch symbol descriptor pointer}

label
  loop;

begin
  escr_loop_iter := false;             {init to loop terminated}

  if e.exblock_p^.bltype <> escr_exblock_loop_k {not in explicit loop block ?}
    then goto loop;                    {loop back unconditionally}
  loop_p := e.exblock_p^.loop_p;       {get pointer to loop descriptor}
  if loop_p = nil then goto loop;      {no loop data ?}
  case loop_p^.looptype of             {what kind of loop is this ?}
{
******************************
*
*   Unconditional loop.  There is no loop state to advance and no terminating
*   condition to check.
}
escr_looptype_unc_k: ;
{
******************************
*
*   Looping thru a previously saved list of symbol names.
}
escr_looptype_sym_k: begin             {looping thru list of symbols}
  while true do begin
    string_list_pos_rel (loop_p^.sym_list_p^, 1); {advance to next symbols list entry}
    if loop_p^.sym_list_p^.str_p = nil {hit end of list ?}
      then return;

    escr_sym_find (                    {try to find the symbol}
      e, loop_p^.sym_list_p^.str_p^, e.sym_var, sym_p);
    if sym_p = nil then next;          {this symbol got deleted, skip it}

    string_copy (                      {copy new name into loop variable}
      loop_p^.sym_list_p^.str_p^, loop_p^.var_p^.var_val.str);
    exit;                              {successfully found next symbol name}
    end;
  end;
{
******************************
*
*   Integer counted loop.
}
escr_looptype_for_k: begin
  loop_p^.for_curr :=                  {advance loop value to next iteration}
    loop_p^.for_curr + loop_p^.for_inc;

  if                                   {done counting up ?}
      (loop_p^.for_end >= loop_p^.for_start) and {counting up ?}
      (loop_p^.for_curr > loop_p^.for_end) {this iteration would be past end ?}
    then return;                       {terminate the loop}
  if                                   {done counting down ?}
      (loop_p^.for_end < loop_p^.for_start) and {counting up ?}
      (loop_p^.for_curr < loop_p^.for_end) {this iteration would be past end ?}
    then return;                       {terminate the loop}

  sym_p := loop_p^.var_p;              {get pointer to loop variable, if any}
  if sym_p <> nil then begin           {there is a loop variable ?}
    sym_p^.var_val.int := loop_p^.for_curr; {update variable for this iteration}
    end;
  end;
{
******************************
}
otherwise                              {unimplemented loop type}
    writeln ('INTERNAL ERROR: Unexpected loop type encountered in LOOP_ITER.');
    escr_err_atline (e, '', '', nil, 0);
    end;                               {end of loop type cases}

loop:                                  {loop execution back to start of block}
  escr_exblock_repeat (e);             {jump back to start of block}
  escr_loop_iter := true;              {indicate execution was looped back}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ENDLOOP (E, STAT)
}
procedure escr_cmd_endloop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

label
  del_block;

begin
  sys_error_none (stat);               {init to no error occurred}

  if e.exblock_p^.bltype <> escr_exblock_loop_k then begin {not in LOOP block type ?}
    escr_err_atline (e, 'pic', 'err_endblock_type', nil, 0);
    end;
  if e.exblock_p^.inpos_p^.prev_p <> nil then begin {block ended in include file ?}
    escr_err_atline (e, 'pic', 'err_endblock_include', nil, 0);
    end;
  if e.inhibit_p^.inh then goto del_block; {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  if escr_loop_iter(e) then return;    {back to do next loop iteration ?}

del_block:                             {delete this block}
  e.exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    e.exblock_p^.inpos_p^.line_p;
  escr_exblock_close (e);              {end this execution block}
  end;
