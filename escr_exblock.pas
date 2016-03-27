{   Routines that manipulate execution blocks.  A execution block is the context
*   for one nested block of executable code, like a subroutine or a macro.  A
*   root block is created for the top level code, so all code is always inside
*   a execution block.
}
module escr_exblock;
define escr_exblock_new;
define escr_exblock_close;
define escr_exblock_loclab_init;
define escr_exblock_inline_set;
define escr_exblock_inline_push;
define escr_exblock_arg_addn;
define escr_exblock_arg_add;
define escr_exblock_arg_get_bl;
define escr_exblock_arg_get;
define escr_exblock_repeat;
define escr_exblock_quit;
%include 'escr.ins.pas';
{
********************************************************************************
*
*   Subroutine EXBLOCK_NEW
*
*   Create a new execution block, initialize it, and set it as current.  The
*   block type will be initialized to TOP, which must be changed for all except
*   the top block.
}
procedure escr_exblock_new;            {create and install new execution block}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  mem_p: util_mem_context_p_t;         {mem context for block, deleted when block closed}
  bl_p: exblock_p_t;                   {points to new execution block created}
  nlev: sys_int_machine_t;             {new block nesting level, top = 0}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if e.exblock_p = nil
    then begin                         {creating top level block}
      util_mem_context_get (e.mem_top_p^, mem_p); {create mem context for top block}
      nlev := 0;                       {set nesting level for top block}
      end
    else begin                         {creating nested block}
      util_mem_context_get (e.exblock_p^.mem_p^, mem_p); {create new mem context for block}
      nlev := e.exblock_p^.level + 1;  {nesting level one more than parent block}
      if nlev > max_blklev_k then begin {new block would be nested too deep ?}
        sys_msg_parm_int (msg_parm[1], max_blklev_k);
        escr_err_atline ('pic', 'err_maxnest_block', msg_parm, 1);
        end;
      end
    ;
  util_mem_grab (                      {allocate memory for execution block descriptor}
    sizeof(bl_p^), mem_p^, false, bl_p);

  bl_p^.prev_p := e.exblock_p;         {link back to parent execution block}
  bl_p^.level := nlev;                 {set 0-N nesting level of this block}
  bl_p^.start_p := nil;                {indicate definition start line not set yet}
  bl_p^.sym_p := nil;                  {init to no symbol exists to represent this block}
  bl_p^.mem_p := mem_p;                {save memory context delete when block closed}
  bl_p^.arg_p := nil;                  {init arguments list to empty}
  bl_p^.arg_last_p := nil;
  bl_p^.nargs := 0;
  bl_p^.locsym_p := nil;               {init to no local symbols created this block}
  bl_p^.inpos_p := nil;                {indicate source reading position not filled in}
  escr_inh_new;                        {make execution inhibit state for this block}
  bl_p^.inh_p := e.inhibit_p;          {save pointer top inhibit for this block}
  bl_p^.loop_p := nil;                 {init to block is not a explicit loop}
  bl_p^.bltype := exblock_top_k;       {init to top block type}
  bl_p^.loclab := nil;                 {init to no list of local labels}
  bl_p^.args := false;                 {init to this block does not take arguments}
  bl_p^.iter1 := true;                 {init to executing the first iteration}

  e.exblock_p := bl_p;                 {make the new block current}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_CLOSE
*
*   Close the current execution block, deallocate any associated resources, and
*   make the previous execution block current.
}
procedure escr_exblock_close;          {close curr execution block and delete temp state}
  val_param;

var
  mem_p: util_mem_context_p_t;         {points to mem context for the block}
  sym_p: sym_p_t;                      {pointer to symbol to delete}
  inhprev_p: inh_p_t;                  {inhibit to restore to}

begin
{
*   Delete the local symbols.  SYM_DEL will delete a symbol and all later
*   versions of it.  It will also delete the entry for that symbol from the
*   local symbols list.
}
  while e.exblock_p^.locsym_p <> nil do begin {loop until local symbols list gone}
    sym_p := e.exblock_p^.locsym_p^.sym_p; {get pointer to this symbol}
    escr_sym_del (sym_p);              {delete it and the local symbols list entry}
    end;

  inhprev_p := e.exblock_p^.inh_p^.prev_p; {get pointer to inhibit to restore to}
  while e.inhibit_p <> inhprev_p do begin {scan back to inibit to restore to}
    escr_inh_end;                      {delete top inhibit}
    if e.inhibit_p = nil then begin    {didn't find inhibit to restore to ?}
      writeln ('INTERNAL ERROR: Inhibit to restore to not found in BLOCK_CLOSE.');
      escr_err_atline ('', '', nil, 0);
      end;
    end;

  if e.exblock_p^.loclab <> nil then begin {delete local labels list if exists}
    string_hash_delete (e.exblock_p^.loclab);
    end;

  mem_p := e.exblock_p^.mem_p;         {save memory context for this block}
  e.exblock_p := e.exblock_p^.prev_p;  {make parent execution block current}
  util_mem_context_del (mem_p);        {deallocate all dynamic memory of the block}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_LOCLAB_INIT
*
*   Create the list of local labels for this execution block and initialize the
*   list to empty.  This routine should be called at most once per execution
*   block.  This routine is intended to be called when the execution block is
*   created, if it is the type of block that has its own local labels context.
*
*   The local labels list is initialized to non-existant when the bare block is
*   first created.
*
*   It is a hard error if this routine is called with the local labels list
*   already existing.
}
procedure escr_exblock_loclab_init;    {create and init local symbols list in this block}
  val_param;

begin
  if e.exblock_p^.loclab <> nil then begin {local labels list already exists ?}
    escr_err_atline ('pic', 'err_loclab_exist', nil, 0);
    end;

  string_hash_create (                 {create the label names hash table}
    e.exblock_p^.loclab,               {returned handle to the loc labels table}
    lab_nbuck_k,                       {number of hash buckets to create}
    lab_maxlen_k,                      {max length of table entry names}
    sizeof(string_var_p_t),            {size of data stored for each entry}
    [string_hashcre_nodel_k],          {won't individually deallocate entries}
    e.exblock_p^.mem_p^);              {pointer to parent memory context}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_INLINE_SET (LINE_P)
*
*   Set a new source input stream position for the current block.  The previous
*   position will be lost.  This is more like a "GOTO", whereas EXBLOCK_INLINE_PUSH
*   is more like a "CALL".
}
procedure escr_exblock_inline_set (    {go to new input source position in curr block}
  in      line_p: inline_p_t);         {pointer to next input line to use}
  val_param;

begin
  if e.exblock_p^.inpos_p = nil
    then begin                         {no position set yet at all for this block}
      escr_exblock_inline_push (line_p); {create new position descriptor and set it}
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
*   Subroutine EXBLOCK_INLINE_PUSH (LINE_P)
*
*   Set the next input line that will be processed by the current execution
*   block.  The new position will be nested under the previous position.  The
*   previous position will be restored when the new position state is deleted.
}
procedure escr_exblock_inline_push (   {push new source line location for exec block}
  in      line_p: inline_p_t);         {pointer to next input line to use}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  pos_p: inpos_p_t;                    {pointer to new nested input position state}
  level: sys_int_machine_t;            {new input file nesting level, top = 0}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  level := 0;                          {init to this is top input file this block}
  if e.exblock_p^.inpos_p <> nil then begin {there is a previous input file this block ?}
    level := e.exblock_p^.inpos_p^.level + 1; {make nesting level of new input file}
    if level > max_inclev_k then begin {would exceed input file nesting level ?}
      sys_msg_parm_int (msg_parm[1], max_inclev_k);
      escr_err_atline ('pic', 'err_maxnext_file', msg_parm, 1);
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
*   Subroutine EXBLOCK_ARG_ADDN (STR, N)
*
*   Add a new argument at the end of the arguments list of the current block.
*   STR is the argument string.  N is the argument number.  Normal arguments
*   are numbered sequentially starting at 1.  Results are undefined if argument
*   N is already defined.
}
procedure escr_exblock_arg_addn (      {add argument to current block, specific number}
  in      str: univ string_var_arg_t;  {argument string}
  in      n: sys_int_machine_t);       {argument number}
  val_param;

var
  arg_p: arg_p_t;                      {points to newly created argument descriptor}

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
*   Subroutine EXBLOCK_ARG_ADD (STR)
*
*   Add a new argument at the end of the arguments list of the current block.
*   STR is the argument string.
}
procedure escr_exblock_arg_add (       {add argument to current block}
  in      str: univ string_var_arg_t); {argument string}
  val_param;

begin
  e.exblock_p^.nargs := e.exblock_p^.nargs + 1; {count one more argument this block}
  escr_exblock_arg_addn (str, e.exblock_p^.nargs); {add argument with this new number}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_ARG_GET_BL (BL, N, VAL_P)
*
*   Return the pointer to the value of argument N of the execution block BL.
*   VAL_P is returned NIL if argument N does not exist.  Arguments are numbered
*   sequentially starting at 1.
}
procedure escr_exblock_arg_get_bl (    {get value of execution block argument}
  in      bl: exblock_t;               {execution block to get argument of}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param;

var
  arg_p: arg_p_t;                      {pointer to current argument descriptor}

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
*   Subroutine EXBLOCK_ARG_GET (N, VAL_P)
*
*   Get the value of argument N as visible from the current execution block.
}
procedure escr_exblock_arg_get (       {get value of currently visible argument}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param;

var
  bl_p: exblock_p_t;                   {pointer to execution block with arguments}

begin
  val_p := nil;                        {init to argument doesn't exist}

  bl_p := e.exblock_p;                 {init to current execution block}
  while not bl_p^.args do begin        {skip over blocks that don't take arguments}
    bl_p := bl_p^.prev_p;              {go to parent execution block}
    if bl_p = nil then return;         {no arguments anywhere ?}
    end;

  escr_exblock_arg_get_bl (bl_p^, n, val_p); {resolve the argument value}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_REPEAT
*
*   Unconditionally loop back to the start of the current execution block.
}
procedure escr_exblock_repeat;         {loop back to start of block}
  val_param;

begin
  while e.inhibit_p <> e.exblock_p^.inh_p do begin {pop back to base block inhibit}
    escr_inh_end;                      {delete this inhibit}
    end;

  escr_exblock_inline_set (e.exblock_p^.start_p); {jump back to block start command}
  escr_infile_skipline;                {skip block definition, to first executable line}
  e.exblock_p^.iter1 := false;         {not in first iteration anymore}
  end;
{
********************************************************************************
*
*   Subroutine EXBLOCK_QUIT
*
*   Effectively leave the current execution block.  Since we don't know where
*   the block ends, all the execution inhibits within this block are turned on.
*   This will cause us to just scan to the end of block command, then pop the
*   block at that time.
}
procedure escr_exblock_quit;
  val_param;

var
  inh_p: inh_p_t;                      {scratch pointer to execution inhibit state}

begin
  inh_p := e.inhibit_p;                {init to top execution inhibit}
  while true do begin                  {loop to base execution inhibit this block}
    inh_p^.inh := true;                {disable execution at this level}
    if inh_p = e.exblock_p^.inh_p then exit; {at base inhibit for the block ?}
    inh_p := inh_p^.prev_p;            {back to previous execution inhibit}
    end;
  end;
