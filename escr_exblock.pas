{   Routines that manipulate execution blocks.  A execution block is the context
*   for one nested block of executable code, like a subroutine or a macro.  A
*   root block is created for the top level code, so all code is always inside
*   a execution block.
}
module escr_exblock;
define escr_exblock_new;
define escr_exblock_close;
define escr_exblock_refsym;
define escr_exblock_ulab_init;
define escr_exblock_inline_set;
define escr_exblock_inline_push;
define escr_exblock_arg_addn;
define escr_exblock_arg_add;
define escr_exblock_arg_get_bl;
define escr_exblock_arg_get;
define escr_exblock_repeat;
define escr_exblock_quit;
define escr_exblock_parse_save;
define escr_exblock_locals_off;
define escr_exblock_locals_on;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_NEW (E, STAT)
*
*   Create a new execution block, initialize it, and set it as current.  The
*   block type will be initialized to TOP, which must be changed for all except
*   the top block.
}
procedure escr_exblock_new (           {create and install new execution block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  mem_p: util_mem_context_p_t;         {mem context for block, deleted when block closed}
  bl_p: escr_exblock_p_t;              {points to new execution block created}
  nlev: sys_int_machine_t;             {new block nesting level, top = 0}

begin
  sys_error_none (stat);               {init to no error encountered}

  if e.exblock_p = nil
    then begin                         {creating top level block}
      nlev := 0;                       {set nesting level for top block}
      util_mem_context_get (e.mem_p^, mem_p); {create mem context for top block}
      end
    else begin                         {creating nested block}
      nlev := e.exblock_p^.level + 1;  {nesting level one more than parent block}
      if nlev > escr_max_blklev_k then begin {new block would be nested too deep ?}
        sys_stat_set (escr_subsys_k, escr_err_bltoomany_k, stat);
        sys_stat_parm_int (escr_max_blklev_k, stat);
        return;
        end;
      util_mem_context_get (e.exblock_p^.mem_p^, mem_p); {create new mem context for block}
      end
    ;
  util_mem_grab (                      {allocate memory for execution block descriptor}
    sizeof(bl_p^), mem_p^, false, bl_p);

  bl_p^.prev_p := e.exblock_p;         {link back to parent execution block}
  bl_p^.level := nlev;                 {set 0-N nesting level of this block}
  bl_p^.start_p := nil;                {indicate definition start line not set yet}
  bl_p^.sym_p := nil;                  {init to no symbol exists to represent this block}
  bl_p^.sym_curr_p := nil;             {init version of symbol to restore to current on exit}
  bl_p^.mem_p := mem_p;                {save memory context to delete when block closed}
  bl_p^.arg_p := nil;                  {init arguments list to empty}
  bl_p^.arg_last_p := nil;
  bl_p^.nargs := 0;
  bl_p^.locsym_p := nil;               {init to no local symbols created this block}
  bl_p^.inpos_p := nil;                {indicate source reading position not filled in}
  bl_p^.previnh_p := e.inhibit_p;      {save pointer to inhibit before this block}
  bl_p^.loop_p := nil;                 {init to block is not a explicit loop}
  bl_p^.parse_p := nil;                {init to no saved input parsing state}
  bl_p^.bltype := escr_exblock_top_k;  {init to top block type}
  bl_p^.ulab := nil;                   {init to no list of unique labels}
  bl_p^.args := false;                 {init to this block does not take arguments}
  bl_p^.iter1 := true;                 {init to executing the first iteration}

  e.exblock_p := bl_p;                 {make the new block current}
  escr_inh_new (e);                    {make top execution inhbit for this block}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_CLOSE (E, STAT)
*
*   Close the current execution block, deallocate any associated resources, and
*   make the previous execution block current.
}
procedure escr_exblock_close (         {close curr execution block and delete temp state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  mem_p: util_mem_context_p_t;         {points to mem context for the block}
  sym_p: escr_sym_p_t;                 {pointer to symbol to delete}
  par_p: escr_parse_p_t;               {pointer to saved input parsing state}

begin
  sys_error_none (stat);               {init to no error encountered}
  if e.exblock_p = nil then return;    {nothing to close ?}
{
*   Delete the local symbols.  SYM_DEL will delete a symbol and all later
*   versions of it.  It will also delete the entry for that symbol from the
*   local symbols list.
}
  while e.exblock_p^.locsym_p <> nil do begin {loop until local symbols list gone}
    sym_p := e.exblock_p^.locsym_p^.sym_p; {get pointer to this symbol}
    escr_sym_del (e, sym_p, stat);     {delete it and the local symbols list entry}
    if sys_error(stat) then return;
    end;

  if e.exblock_p^.sym_p <> nil then begin {block envoked by symbol reference ?}
    e.exblock_p^.sym_p^.ent_p^.curr_p := {restore previous current version}
      e.exblock_p^.sym_curr_p;
    end;

  e.inhibit_p := e.exblock_p^.previnh_p; {restore to inhibit before this block}

  par_p := e.exblock_p^.parse_p;       {get pointer to any saved input parsing state}
  if par_p <> nil then begin           {this block has its own parsing state ?}
    e.parse_p := par_p^.prev_p;        {pop back to the previous parsing state}
    end;

  mem_p := e.exblock_p^.mem_p;         {save memory context for this block}
  e.exblock_p := e.exblock_p^.prev_p;  {make parent execution block current}
  util_mem_context_del (mem_p);        {deallocate all dynamic memory of the block}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_REFSYM (E, SYM)
*
*   Set the referencing symbol of the current execution block.  SYM is the
*   version of the symbol that references this block.
*
*   Inside a named block, the current version of the name symbol is always the
*   previous from the version that references the block.  This means, for
*   example, that calling the subroutine currently in with its unqualified
*   name results in chaining to the next older version, not recursion.
*
*   The current version of the name symbol is restored to its current value when
*   this block is deleted.
}
procedure escr_exblock_refsym (        {set referencing symbol of current block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym: escr_sym_t);            {version of symbol referencing this block}
  val_param;

begin
  e.exblock_p^.sym_p := addr(sym);     {save pointer to referencing symbol version}
  e.exblock_p^.sym_curr_p := sym.ent_p^.curr_p; {save pointer to current version}
  sym.ent_p^.curr_p := sym.prev_p;     {make previous version, if any, current}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_ULAB_INIT (E)
*
*   Create the list of unique labels for this execution block and initialize the
*   list to empty.  This routine should be called at most once per execution
*   block.  This routine is intended to be called when the execution block is
*   created, if it is the type of block that has its own unique labels context.
*
*   The unique labels list is initialized to non-existant when the bare block is
*   first created.
*
*   It is a hard error if this routine is called with the unique labels list
*   already existing.
}
procedure escr_exblock_ulab_init (     {create unique labels list in this block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  if e.exblock_p^.ulab <> nil then begin {unique labels list already exists ?}
    escr_err_atline (e, 'escr', 'err_loclab_exist', nil, 0);
    end;

  string_hash_create (                 {create the unique labels hash table}
    e.exblock_p^.ulab,                 {returned handle to the unique labels table}
    escr_ulab_nbuck_k,                 {number of hash buckets to create}
    escr_ulab_maxlen_k,                {max length of table entry names}
    sizeof(string_var_p_t),            {size of data stored for each entry}
    [string_hashcre_nodel_k],          {won't individually deallocate entries}
    e.exblock_p^.mem_p^);              {pointer to parent memory context}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_INLINE_SET (E, LINE_P, STAT)
*
*   Set a new source input stream position for the current block.  The previous
*   position will be lost.  This is more like a GOTO, whereas
*   EXBLOCK_INLINE_PUSH is more like a CALL.
}
procedure escr_exblock_inline_set (    {go to new input source position in curr block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to next input line to use}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  if e.exblock_p^.inpos_p = nil
    then begin                         {no position set yet at all for this block}
      escr_exblock_inline_push (e, line_p, stat); {create new position descriptor and set it}
      if sys_error(stat) then return;
      end
    else begin                         {position descriptor already exists}
      e.exblock_p^.inpos_p^.line_p := line_p; {jump to the new source stream position}
      end
    ;

  if e.exblock_p^.start_p = nil then begin {line starting this block not set yet ?}
    e.exblock_p^.start_p := line_p;    {set this line as block starting line}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_INLINE_PUSH (E, LINE_P, STAT)
*
*   Set the next input line that will be processed by the current execution
*   block.  The new position will be nested under the previous position.  The
*   previous position will be restored when the new position state is deleted.
}
procedure escr_exblock_inline_push (   {push new source line location for exec block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to next input line to use}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  pos_p: escr_inpos_p_t;               {pointer to new nested input position state}
  level: sys_int_machine_t;            {new input file nesting level, top = 0}

begin
  sys_error_none (stat);               {init to no error encountered}

  level := 0;                          {init to this is top input file this block}
  if e.exblock_p^.inpos_p <> nil then begin {there is a previous input file this block ?}
    level := e.exblock_p^.inpos_p^.level + 1; {make nesting level of new input file}
    if level > escr_max_inclev_k then begin {would exceed input file nesting level ?}
      sys_stat_set (escr_subsys_k, escr_err_inftoomany_k, stat);
      sys_stat_parm_int (escr_max_inclev_k, stat);
      return;
      end;
    end;

  util_mem_grab (                      {allocate new input position descriptor}
    sizeof(pos_p^), e.exblock_p^.mem_p^, true, pos_p);
  pos_p^.prev_p := e.exblock_p^.inpos_p; {point back to previous input stream position}
  pos_p^.level := level;               {set nesting level of this position state}
  pos_p^.line_p := line_p;             {point to next input line to use}
  if pos_p^.prev_p = nil
    then begin                         {no previous input location exists this block}
      pos_p^.last_p := line_p;         {make the new line the last read line}
      end
    else begin                         {a previous input line was read this block}
      pos_p^.last_p := pos_p^.prev_p^.last_p; {init pointer to last read line}
      end
    ;
  e.exblock_p^.inpos_p := pos_p;       {make new input position current}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_ARG_ADDN (E, STR, N)
*
*   Add a new argument at the end of the arguments list of the current block.
*   STR is the argument string.  N is the argument number.  Normal arguments
*   are numbered sequentially starting at 1.  Results are undefined if argument
*   N is already defined.
}
procedure escr_exblock_arg_addn (      {add argument to current block, specific number}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t;  {argument string}
  in      n: sys_int_machine_t);       {argument number}
  val_param;

var
  arg_p: escr_arg_p_t;                 {points to newly created argument descriptor}

begin
  e.exblock_p^.args := true;           {this block definitely takes arguments}

  util_mem_grab (                      {allocate memory for the new argument}
    sizeof(arg_p^), e.exblock_p^.mem_p^, false, arg_p);
  arg_p^.next_p := nil;                {this will be last argument in the list}
  arg_p^.argn := n;                    {set number of this argument}
  string_alloc (                       {allocate mem for the arg string and init it}
    str.len, e.exblock_p^.mem_p^, false, arg_p^.val_p);
  string_copy (str, arg_p^.val_p^);    {set the argument value}

  if e.exblock_p^.arg_last_p = nil
    then begin                         {this is first argument in list}
      e.exblock_p^.arg_p := arg_p;
      end
    else begin                         {add to end of existing list}
      e.exblock_p^.arg_last_p^.next_p := arg_p;
      end
    ;
  e.exblock_p^.arg_last_p := arg_p;    {update pointer to last argument in list}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_ARG_ADD (E, STR)
*
*   Add a new argument at the end of the arguments list of the current block.
*   STR is the argument string.
}
procedure escr_exblock_arg_add (       {add argument to current block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {argument string}
  val_param;

begin
  e.exblock_p^.nargs := e.exblock_p^.nargs + 1; {count one more argument this block}
  escr_exblock_arg_addn (e, str, e.exblock_p^.nargs); {add argument with this new number}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_ARG_GET_BL (E, BL, N, VAL_P)
*
*   Return the pointer to the value of argument N of the execution block BL.
*   VAL_P is returned NIL if argument N does not exist.  Arguments are numbered
*   sequentially starting at 1.
}
procedure escr_exblock_arg_get_bl (    {get value of execution block argument}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      bl: escr_exblock_t;          {execution block to get argument of}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param;

var
  arg_p: escr_arg_p_t;                 {pointer to current argument descriptor}

begin
  val_p := nil;                        {init to argument does not exist}

  arg_p := bl.arg_p;                   {init pointer to first argument in list}
  while arg_p <> nil do begin          {scan the list of arguments}
    if arg_p^.argn = n then begin      {found argument N ?}
      val_p := arg_p^.val_p;           {pass back pointer to the argument value}
      return;
      end;
    arg_p := arg_p^.next_p;            {advance to next argument in the list}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_ARG_GET (E, N, VAL_P)
*
*   Get the value of argument N as visible from the current execution block.
}
procedure escr_exblock_arg_get (       {get value of currently visible argument}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param;

var
  bl_p: escr_exblock_p_t;              {pointer to execution block with arguments}

begin
  val_p := nil;                        {init to argument doesn't exist}

  bl_p := e.exblock_p;                 {init to current execution block}
  while not bl_p^.args do begin        {skip over blocks that don't take arguments}
    bl_p := bl_p^.prev_p;              {go to parent execution block}
    if bl_p = nil then return;         {no arguments anywhere ?}
    end;

  escr_exblock_arg_get_bl (e, bl_p^, n, val_p); {resolve the argument value}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_REPEAT (E, STAT)
*
*   Unconditionally loop back to the start of the current execution block.
}
procedure escr_exblock_repeat (        {loop back to start of block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  while e.inhibit_p^.prev_p <> e.exblock_p^.previnh_p do begin {back to base block inhibit}
    escr_inh_end (e, stat);            {delete this inhibit}
    if sys_error(stat) then return;
    end;

  escr_exblock_inline_set (e, e.exblock_p^.start_p, stat); {jump back to block start command}
  if sys_error(stat) then return;
  escr_infile_skipline (e);            {skip block definition, to first executable line}
  e.exblock_p^.iter1 := false;         {not in first iteration anymore}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_QUIT (E)
*
*   Effectively leave the current execution block.  Since we don't know where
*   the block ends, all the execution inhibits within this block are turned on.
*   This will cause us to just scan to the end of block command, then pop the
*   block at that time.
}
procedure escr_exblock_quit (          {stop executing in the current block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  inh_p: escr_inh_p_t;                 {scratch pointer to execution inhibit state}

begin
  inh_p := e.inhibit_p;                {init to current execution inhibit}
  while true do begin                  {loop to base execution inhibit this block}
    inh_p^.inh := true;                {disable execution at this level}
    if inh_p^.prev_p = e.exblock_p^.previnh_p then exit; {at base inhibit for the block ?}
    inh_p := inh_p^.prev_p;            {back to previous execution inhibit}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_PARSE_SAVE (E, STAT)
*
*   Save the current input stream parsing state such that it will be restored to
*   this state when the block is closed.  The input parsing state can only be
*   saved once per execution block.  This is usually done after a new block is
*   created, before any code is run in it.  It is a error if the input parsing
*   state was previously saved in this block.
}
procedure escr_exblock_parse_save (    {save parsing state, will be restored on block end}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  par_p: escr_parse_p_t;               {points to new saved parsing state}

begin
  sys_error_none (stat);               {init to no error encountered}

  if e.exblock_p = nil then begin      {not in a execution block ?}
    sys_stat_set (escr_subsys_k, escr_err_noblock_k, stat);
    return;
    end;
  if e.exblock_p^.parse_p <> nil then begin {parse state already saved this block ?}
    sys_stat_set (escr_subsys_k, escr_err_parsesaved_k, stat);
    return;
    end;

  util_mem_grab (                      {allocate memory for the saved parsing state}
    sizeof(par_p^),                    {amount of memory to allocate}
    e.exblock_p^.mem_p^,               {mem context to allocate under}
    false,                             {will not be individually deallocated}
    par_p);                            {returned pointer to the new memory}
  if par_p = nil then begin            {failed to allocate the memory ?}
    sys_stat_set (escr_subsys_k, escr_err_nomem_k, stat);
    sys_stat_parm_int (sizeof(par_p^), stat);
    return;
    end;

  escr_parse_init (par_p^);            {init the new parsing state descriptor}
  par_p^.prev_p := e.parse_p;          {link to previous parsing state}
  e.exblock_p^.parse_p := par_p;       {link this block to the new parsing state}
  e.parse_p := par_p;                  {make the new parsing state current}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_LOCALS_OFF (E)
*
*   Make the current version of all symbols the one before any local versions.
*   This effectively makes the current versions those that were the latest when
*   this execution block was entered.  These would be the parent versions of
*   local symbols inside the block.
}
procedure escr_exblock_locals_off (    {current version of symbols parent, not local}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  lsym_p: escr_sylist_p_t;             {points to current local symbol}

begin
  if e.exblock_p = nil then return;

  lsym_p := e.exblock_p^.locsym_p;     {init to first local symbol in the list}
  while lsym_p <> nil do begin         {loop over the local symbols}
    lsym_p^.sym_p^.ent_p^.curr_p :=    {make curr version one before local}
      lsym_p^.sym_p^.prev_p;
    lsym_p := lsym_p^.next_p;          {advance to next local symbol in list}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXBLOCK_LOCALS_ON (E)
*
*   Make the local versions of symbols the current version.  This is the normal
*   state inside a execution block.  The routine effectively undoes what
*   ESCR_EXBLOCK_LOCALS_OFF does.
}
procedure escr_exblock_locals_on (     {make local versions of symbols current}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  lsym_p: escr_sylist_p_t;             {points to current local symbol}

begin
  if e.exblock_p = nil then return;

  lsym_p := e.exblock_p^.locsym_p;     {init to first local symbol in the list}
  while lsym_p <> nil do begin         {loop over the local symbols}
    lsym_p^.sym_p^.ent_p^.curr_p :=    {make curr version the local version}
      lsym_p^.sym_p;
    lsym_p := lsym_p^.next_p;          {advance to next local symbol in list}
    end;
  end;
