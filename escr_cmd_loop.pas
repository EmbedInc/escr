{   Commands related to loops.
}
module escr_cmd_loop;
define escr_cmd_loop;
define loop_iter;
define escr_cmd_endloop;
%include 'escr.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_CMD_LOOP (STAT)
*
*   /LOOP
*   /LOOP SYMBOLS varname
}
procedure escr_cmd_loop (
  out     stat: sys_err_t);
  val_param;

var
  loop_p: loop_p_t;                    {pointer to loop descriptor}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  name: string_var80_t;                {variable name}
  tk: string_var32_t;                  {scratch token}
  name_p: string_var_p_t;              {scratch string pointer}
  sym_p: sym_p_t;                      {pointer to symbol name}
  sym_pp: sym_pp_t;                    {pointer to data in symbol table entry}
  slist_p: string_list_p_t;            {points to strings list}
  pos: string_hash_pos_t;              {symbol table position handle}
  found: boolean;                      {symbol table entry found}

label
  loop_from;

begin
  name.max := size_char(name.str);     {init local var string}
  tk.max := size_char(tk.str);
  sys_error_none (stat);               {init to no error occurred}

  exblock_new;                         {create new execution block state}
  exblock_p^.start_p :=                {save pointer to starting line of this block}
    exblock_p^.prev_p^.inpos_p^.last_p;
  exblock_p^.bltype := exblock_loop_k; {indicate LOOP ... ENDLOOP type}
  exblock_inline_set (                 {set next source line to execute}
    exblock_p^.prev_p^.inpos_p^.line_p);

  if inhibit_p^.inh then return;       {execution inhibited ?}

  get_keyword ('SYMBOLS FOR FROM N', pick); {get looptype keyword}
  util_mem_grab (                      {allocate loop descriptor}
    sizeof(loop_p^), exblock_p^.mem_p^, false, loop_p);
  loop_p^.looptype := looptype_unc_k;  {init loop descriptor to default}
  loop_p^.var_p := nil;                {init to no loop variable}

  case pick of
{
******************************
*
*   /LOOP with no parameters.
}
0: begin
  get_end;                             {no more command parameters allowed}
  end;
{
******************************
*
*   /LOOP SYMBOLS
}
1: begin
  loop_p^.looptype := looptype_sym_k;  {looping over list of symbols}

  if not get_token (name)              {get the variable name into NAME}
    then err_parm_missing ('', '', nil, 0);
  get_end;                             {no more command parameters allowed}
{
*   Create the local list of symbol names.
}
  util_mem_grab (                      {allocate symbol names list}
    sizeof(slist_p^), exblock_p^.mem_p^, false, slist_p);
  loop_p^.sym_list_p := slist_p;       {save pointer to names list}

  string_list_init (slist_p^, exblock_p^.mem_p^); {init names list}
  slist_p^.deallocable := false;       {not individually deacllocatable}

  string_hash_pos_first (sym, pos, found);
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
  sym_new_var (                        {create the loop variable}
    name,                              {name of variable to create}
    dtype_str_k,                       {variable's data type}
    max_namelen_k,                     {max length}
    false,                             {local, not global}
    sym_p);                            {returned pointer to the new variable}
  loop_p^.var_p := sym_p;              {save pointer to the loop variable}

  string_list_pos_abs (slist_p^, 1);   {go to first list entry}

  if slist_p^.str_p = nil then begin   {the list is empty ?}
    inhibit_p^.inh := true;            {inhibit execution for this block}
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
  if not get_token (name)              {get the variable name into NAME}
    then err_parm_missing ('', '', nil, 0);

  get_keyword ('FROM', pick);
loop_from:                             {common code with /LOOP FROM}
  if not get_int (loop_p^.for_start)
    then err_parm_missing ('', '', nil, 0);

  get_keyword ('TO', pick);
  if not get_int (loop_p^.for_end)
    then err_parm_missing ('', '', nil, 0);

  if loop_p^.for_end >= loop_p^.for_start {set default increment from loop direction}
    then loop_p^.for_inc := 1
    else loop_p^.for_inc := -1;

  get_keyword ('BY', pick);
  if pick = 1 then begin               {BY clause exists ?}
    if not get_int (loop_p^.for_inc)
      then err_parm_missing ('', '', nil, 0);
    get_end;                           {nothing more allowed on the command line}
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
    err_atline ('pic', 'err_loopinc0', nil, 0);
    end;
  loop_p^.looptype := looptype_for_k;  {FOR loop}
  loop_p^.for_curr := loop_p^.for_start; {init value for first iteration}

  if
      (loop_p^.for_inc >= 0) and       {counting up ?}
      (loop_p^.for_curr > loop_p^.for_end) {already past the end ?}
      then begin
    inhibit_p^.inh := true;            {inhibit execution for this block}
    end;
  if
      (loop_p^.for_inc < 0) and        {counting down ?}
      (loop_p^.for_curr < loop_p^.for_end) {already past the end ?}
      then begin
    inhibit_p^.inh := true;            {inhibit execution for this block}
    end;
  if inhibit_p^.inh then return;       {execution is inhibited ?}

  if name.len > 0 then begin           {need to create loop variable ?}
    sym_new_var (                      {create the loop variable}
      name,                            {name of the variable to create}
      dtype_int_k,                     {data type will be integer}
      0,                               {additional length parameter, unused}
      false,                           {variable will be local, not global}
      sym_p);                          {returned pointer to the new variable symbol}
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
  loop_p^.looptype := looptype_for_k;  {this will be FOR loop with no variable}
  loop_p^.for_start := 1;              {starting value}
  loop_p^.for_curr := 1;               {value for first iteration}
  loop_p^.for_inc := 1;                {increment each iteration}

  if not get_int (loop_p^.for_end)     {get number of times to loop}
    then err_parm_missing ('', '', nil, 0);

  if loop_p^.for_end < 1 then begin    {will do 0 iterations ?}
    inhibit_p^.inh := true;            {inhibit execution for this block}
    return;
    end;
  end;
{
******************************
}
    end;                               {end of looptype cases}

  exblock_p^.loop_p := loop_p;         {add loop descriptor to this execution block}
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
function loop_iter                     {advance to next loop iteration}
  :boolean;                            {looped back, not terminated}
  val_param;

var
  loop_p: loop_p_t;                    {pointer to loop descriptor}
  sym_p: sym_p_t;                      {scratch symbol descriptor pointer}

label
  loop;

begin
  loop_iter := false;                  {init to loop terminated}

  if exblock_p^.bltype <> exblock_loop_k {not in explicit loop block ?}
    then goto loop;                    {loop back unconditionally}
  loop_p := exblock_p^.loop_p;         {get pointer to loop descriptor}
  if loop_p = nil then goto loop;      {no loop data ?}
  case loop_p^.looptype of             {what kind of loop is this ?}
{
******************************
*
*   Unconditional loop.  There is no loop state to advance and no terminating
*   condition to check.
}
looptype_unc_k: ;
{
******************************
*
*   Looping thru a previously saved list of symbol names.
}
looptype_sym_k: begin                  {looping thru list of symbols}
  while true do begin
    string_list_pos_rel (loop_p^.sym_list_p^, 1); {advance to next symbols list entry}
    if loop_p^.sym_list_p^.str_p = nil {hit end of list ?}
      then return;

    sym_find (loop_p^.sym_list_p^.str_p^, sym_p); {try to find the symbol}
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
looptype_for_k: begin
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
    err_atline ('', '', nil, 0);
    end;                               {end of loop type cases}

loop:                                  {loop execution back to start of block}
  exblock_repeat;                      {jump back to start of block}
  loop_iter := true;                   {indicate execution was looped back}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_ENDLOOP (STAT)
}
procedure escr_cmd_endloop (
  out     stat: sys_err_t);
  val_param;

label
  del_block;

begin
  sys_error_none (stat);               {init to no error occurred}

  if exblock_p^.bltype <> exblock_loop_k then begin {not in LOOP block type ?}
    err_atline ('pic', 'err_endblock_type', nil, 0);
    end;
  if exblock_p^.inpos_p^.prev_p <> nil then begin {block ended in include file ?}
    err_atline ('pic', 'err_endblock_include', nil, 0);
    end;
  if inhibit_p^.inh then goto del_block; {execution is inhibited ?}

  get_end;                             {no command parameters allowed}
  if loop_iter then return;            {back to do next loop iteration ?}

del_block:                             {delete this block}
  exblock_p^.prev_p^.inpos_p^.line_p := {restart previous block after this command}
    exblock_p^.inpos_p^.line_p;
  exblock_close;                       {end this execution block}
  end;
