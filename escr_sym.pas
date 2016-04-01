{   Symbol handling routines.
}
module escr_sym;
define escr_sym_name;
define escr_sym_new;
define escr_sym_new_const;
define escr_sym_new_var;
define escr_sym_find;
define escr_sym_del;
define escr_sym_del_name;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Function ESCR_SYM_NAME (E, NAME)
*
*   Returns TRUE iff NAME is a valid symbol name.
}
function escr_sym_name (               {check for valid symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
  val_param;

var
  i: sys_int_machine_t;                {scratch loop counter}
  c: char;                             {scratch character}

begin
  escr_sym_name := false;              {init to not a valid symbol name}

  if name.len > escr_max_namelen_k then return; {name is too long ?}
  if name.len < 1 then return;         {name is too short ?}

  for i := 1 to name.len do begin      {once for each character in name}
    c := name.str[i];                  {get this symbol name character}
    if (c >= 'A') and (c <= 'Z') then next; {upper case letter ?}
    if (c >= 'a') and (c <= 'z') then next; {lower case letter ?}
    if i > 1 then begin                {other than the first character ?}
      if (c >= '0') and (c <= '9') then next; {decimal digit ?}
      if c = '_' then next;            {underscore ?}
      end;
    return;                            {character didn't match any of the rules}
    end;                               {back and check next name character}

  escr_sym_name := true;               {NAME is a valid symbol name}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW (E, NAME, SZ, GLOBAL, SYTABLE, SYM_P, STAT)
*
*   Low level routine to add or create a new version of a symbol in a specific
*   symbol table.
*
*   NAME is the name of the symbol to create or add a new version of.  NAME must
*   be a valid symbol name, but that is not checked here.
*
*   SZ is the size of the whole symbol descriptor to create.  This varies
*   depending on the type of symbol and the data associated with it.
*
*   GLOBAL indicates that the new symbol is to have global scope.  When false, a
*   local symbol is created that will automatically be deleted when the current
*   execution block ends.  All symbols in the top block are created global
*   regardless of the value of GLOBAL.
*
*   SYTABLE is the symbol table to create the new symbol in.  For example, E.SYM
*   is the symbol table for variables and constants.
*
*   SYM_P is returned pointing to the new symbol descriptor.  It will be filled
*   in except for the symbol type and fields that depend on the symbol type.
*   SYM_P is returned NIL on any error.  In that case STAT will be set to
*   indicate the error.
*
*   STAT is always the normal status when returning with a new symbol, as
*   indicated by SYM_P not being the NIL pointer.  STAT will always indicate a
*   error when SYM_P is NIL.
}
procedure escr_sym_new (               {create new symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      sz: sys_int_adr_t;           {size of the whole symbol descriptor}
  in      global: boolean;             {create global, not local symbol}
  in out  sytable: escr_sytable_t;     {symbol table to add symbol to}
  out     sym_p: escr_sym_p_t;         {returned pointer to symbol info}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  name_p: string_var_p_t;              {pointer to name in symbol table}
  sym_pp: escr_sym_pp_t;               {pointer to symbol pointer in table entry}
  prev_p: escr_sym_p_t;                {pointer to previous version of symbol}
  vern: sys_int_machine_t;             {1-N version number of the new symbol}
  lsym_p: escr_sylist_p_t;             {pointer to local symbol list entry for ex block}
  pos: string_hash_pos_t;              {handle to position in symbol table}
  found: boolean;                      {TRUE if name found in symbol table}

label
  local;

begin
  sym_p := nil;                        {init to not returning with new symbol}

  string_hash_pos_lookup (             {find position of name in symbol table}
    sytable.hash,                      {hash table to find position in}
    name,                              {name to find position of}
    pos,                               {returned position}
    found);                            {TRUE if name found in table}
  if found
    then begin                         {a previous version of this symbol exists}
      string_hash_ent_atpos (pos, name_p, sym_pp); {look up existing symbol info}
      prev_p := sym_pp^;               {get pointer to curr symbol of this name}
      vern := prev_p^.vern + 1;        {make version number of this new symbol}
      if vern > escr_max_symvers_k then begin {already at maximum allowed version ?}
        sys_stat_set (escr_subsys_k, escr_err_sytoomany_k, stat);
        sys_stat_parm_vstr (name, stat);
        return;
        end;
      end
    else begin                         {no symbol if this name currently exists}
      string_hash_ent_add (pos, name_p, sym_pp); {create new symbol table entry}
      prev_p := nil;                   {indicate no previous version exists}
      vern := 1;                       {this will be version 1 of this symbol name}
      end
    ;
{
*   The symbol table entry for this name exists, SYM_PP points to the pointer
*   in the symbol table that points to the last version of the symbol, NAME_P
*   points to the symbol name string in the symbol table, and PREV_P points
*   to the current last version of the symbol.  PREV_P is NIL if there is no
*   current version.  VERN is the 1-N version number of the new version of this
*   symbol name that is being created.
*
*   Now create the new symbol descriptor and install it.
}
    util_mem_grab (                    {allocate memory for new symbol descriptor}
      sz, sytable.mem_p^, true, sym_p);
    if sym_p = nil then begin
      sys_stat_set (escr_subsys_k, escr_err_nomem_k, stat);
      sys_stat_parm_int (sz, stat);
      return;
      end;
    sys_error_none (stat);             {will return with new symbol}

    sym_p^.table_p := addr(sytable);   {point to symbol table this symbol is in}
    sym_p^.prev_p := prev_p;           {point back to previous version of this symbol}
    sym_p^.next_p := nil;              {init to no following symbol of this name}
    sym_p^.name_p := name_p;           {point to name string in symbol table}
    sym_p^.vern := vern;               {set 1-N version number}
    sym_p^.scope_p := nil;             {init to global symbol}
    sym_pp^ := sym_p;                  {point symbol table to this new version}

    if e.exblock_p^.prev_p = nil then return; {don't make local if in top block}
    if not global then goto local;     {create as local symbol ?}
    if                                 {check for previous version is local}
        (prev_p <> nil) and then       {previous version of this symbol exists ?}
        (prev_p^.scope_p <> nil)       {and that version is local ?}
      then goto local;                 {then make this new version local too}

    sym_p^.scope_p := nil;             {indicate this is a global symbol}
    return;
{
*   Create the new symbol as a local symbol.  The symbol will be flagged as
*   belonging to the current execution block, and will be added to the list of
*   symbols to delete when that execution block is ended.
}
local:                                 {create symbol as local}
  sym_p^.scope_p := e.exblock_p;       {indicate local symbol and point to owning block}

  util_mem_grab (                      {create new local symbol list entry}
    sizeof(lsym_p^), e.exblock_p^.mem_p^, true, lsym_p);
  lsym_p^.next_p := e.exblock_p^.locsym_p; {point to rest of local symbols chain}
  lsym_p^.sym_p := sym_p;              {point to the local symbol}
  e.exblock_p^.locsym_p := lsym_p;     {link in at start of local symbols list}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_CONST (E, NAME, DTYPE, LEN, GLOBAL, SYM_P, STAT)
*
*   Create a new symbol for a constant.  NAME is the name of the new symbol,
*   DTYPE is the data type, and LEN is an extra argument for the data type.
*   See the VAL_SIZE description for details of LEN.  The new symbol will be a
*   global symbol when GLOBAL is TRUE, and local to the current execution block
*   when GLOBAL is FALSE.  SYM_P is the returned pointer to the new symbol.  The
*   data value will be initialized to its default value.
}
procedure escr_sym_new_const (         {create new symbol for a constant}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: escr_dtype_k_t;       {data type of the constant}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to symbol info}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  sz: sys_int_adr_t;                   {total symbol descriptor size required}

begin
  sz := offset(escr_sym_t.const_val) + {total symbol descriptor size}
    escr_val_size (e, dtype, len);
  escr_sym_new (                       {create the basic symbol}
    e, name, sz, global, e.sym_var, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.stype := escr_sym_const_k;    {set symbol type}
  sym_p^.const_val.dtype := dtype;     {set data type of this constant}
  case dtype of                        {what is the data type ?}
escr_dtype_bool_k: begin               {boolean}
      sym_p^.const_val.bool := false;
      end;
escr_dtype_int_k: begin                {integer}
      sym_p^.const_val.int := 0;
      end;
escr_dtype_fp_k: begin                 {floating point}
      sym_p^.const_val.fp := 0.0;
      end;
escr_dtype_str_k: begin                {string}
      sym_p^.const_val.str.max := len;
      sym_p^.const_val.str.len := 0;
      sym_p^.const_val.str.str[1] := chr(0);
      end;
escr_dtype_time_k: begin               {time}
      sym_p^.const_val.time := sys_clock;
      end;
otherwise
    escr_err_dtype_unimp (e, dtype, 'ESCR_SYM_NEW_CONST');
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_VAR (E, NAME, DTYPE, LEN, GLOBAL, SYM_P, STAT)
*
*   Create a new symbol for a variable.  NAME is the name of the new symbol,
*   DTYPE is the data type, and LEN is an extra argument for the data type.
*   See the VAL_SIZE description for details of LEN.  The new symbol will be a
*   global symbol when GLOBAL is TRUE, and local to the current execution block
*   when GLOBAL is FALSE.  SYM_P is the returned pointer to the new symbol.  The
*   data value will be initialized to its default value.
}
procedure escr_sym_new_var (           {create new symbol for a variable}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: escr_dtype_k_t;       {data type of the variable}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to symbol info}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  sz: sys_int_adr_t;                   {total symbol descriptor size required}

begin
  sz := offset(escr_sym_t.var_val) +   {total symbol descriptor size}
    escr_val_size (e, dtype, len);
  escr_sym_new (                       {create the basic symbol}
    e, name, sz, global, e.sym_var, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.stype := escr_sym_var_k;      {set symbol type}
  sym_p^.var_val.dtype := dtype;       {set data type of this variable}
  case dtype of                        {what is the data type ?}
escr_dtype_bool_k: begin               {boolean}
      sym_p^.var_val.bool := false;
      end;
escr_dtype_int_k: begin                {integer}
      sym_p^.var_val.int := 0;
      end;
escr_dtype_fp_k: begin                 {floating point}
      sym_p^.var_val.fp := 0.0;
      end;
escr_dtype_str_k: begin                {string}
      sym_p^.var_val.str.max := len;
      sym_p^.var_val.str.len := 0;
      sym_p^.var_val.str.str[1] := chr(0);
      end;
escr_dtype_time_k: begin               {time}
      sym_p^.var_val.time := sys_clock_from_fp_rel (0.0);
      end;
otherwise
    escr_err_dtype_unimp (e, dtype, 'SYM_NEW_VAR');
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_FIND (E, NAME, SYTABLE, SYM_P)
*
*   Look up a name in the symbol table SYTABLE.  If a symbol table entry with
*   that name exists, then SYM_P is returned pointing to the symbol descriptor.
*   If the named symbol does not exist, then SYM_P is returned nil.
}
procedure escr_sym_find (              {look up symbol in symbol table}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in out  sytable: escr_sytable_t;     {symbol table to look up name in}
  out     sym_p: escr_sym_p_t);        {returned pointer to symbol, NIL if not found}
  val_param;

var
  name_p: string_var_p_t;              {pointer to name in symbol table}
  sym_pp: escr_sym_pp_t;               {pointer to symbol pointer in table entry}

begin
  string_hash_ent_lookup (             {look up name in symbol table}
    sytable.hash,                      {hash table to look up name in}
    name,                              {the name to look up}
    name_p,                            {returned pointer to name in table entry}
    sym_pp);                           {returned pointer to table entry data area}
  if sym_pp = nil
    then begin                         {no such symbol exists}
      sym_p := nil;
      end
    else begin                         {the symbol was found}
      sym_p := sym_pp^;                {return pointer to the symbol descriptor}
      end
    ;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL (E, SYM_P)
*
*   Delete the symbol pointed to by SYM_P.  SYM_P is returned NIL.  All symbols
*   of the same name stacked later than the indicated symbol are also deleted.
}
procedure escr_sym_del (               {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t);        {pointer to symbol to delete, returned NIL}
  val_param;

var
  s_p, s2_p: escr_sym_p_t;             {scratch symbol pointers}
  lsym_p: escr_sylist_p_t;             {pointer to one local symbols list entry}
  lsym_pp: escr_sylist_pp_t;           {pointer to link to current local symbol list entry}
  name_p: string_var_p_t;              {pointer to name in symbol table}
  sym_pp: escr_sym_pp_t;               {pointer to symbol pointer in table entry}
  tbl_p: escr_sytable_p_t;             {pointer to symbol table this symbol is in}
  pos: string_hash_pos_t;              {handle to position in symbol table}
  found: boolean;                      {TRUE if name found in symbol table}

begin
{
*   Make sure all symbols created after this version are deleted first.
}
  s_p := sym_p^.next_p;                {get pointer to next newer symbol}
  if s_p <> nil then begin             {there is at least one newer version ?}
    while s_p^.next_p <> nil           {loop to find end of versions chain}
      do s_p := s_p^.next_p;

    while s_p <> sym_p do begin        {from latest version back to this one}
      s2_p := s_p^.prev_p;             {save pointer to next older version}
      escr_sym_del (e, s_p);           {delete this newest version}
      s_p := s2_p;                     {go to the next older, now the newest}
      end;                             {back to do this new end of versions entry}
    end;
{
*   The symbol at SYM_P is the newest of that name.
*
*   If this is a local symbol, delete its entry from the local symbol list for
*   its execution block.
}
  if sym_p^.scope_p <> nil then begin  {this is a local symbol ?}
    lsym_pp := addr(sym_p^.scope_p^.locsym_p); {init pointer to start of chain link}
    while true do begin                {scan local symbols list looking for this symbol}
      lsym_p := lsym_pp^;              {get pointer to this local symbols list entry}
      if lsym_p = nil then begin       {exhausted local symbols list ?}
        writeln ('INTERNAL ERROR: Symbol "', sym_p^.name_p^.str:sym_p^.name_p^.len, '"');
        writeln ('  not found in local symbols list.  (ESCR_SYM_DEL)');
        sys_bomb;
        end;
      if lsym_p^.sym_p = sym_p then exit; {found local symbols list entry for this sym ?}
      lsym_pp := addr(lsym_p^.next_p); {advance to next list entry}
      end;                             {back to check this next list entry}
    {
    *   LSYM_P is pointing to the local symbols list entry for this symbol, and
    *   LSYM_PP is pointing to the forward link to this local symbols list entry.
    }
    lsym_pp^ := lsym_p^.next_p;        {unlink this entry from local symbols list}
    util_mem_ungrab (lsym_p, sym_p^.scope_p^.mem_p^); {deallocate list entry}
    end;
{
*   Done deleting local symbols list entry, if any.
}
  tbl_p := sym_p^.table_p;             {get pointer to the symbol table}

  string_hash_pos_lookup (             {find position of symbol table entry}
    tbl_p^.hash, sym_p^.name_p^, pos, found);
  if not found then begin
    writeln ('INTERNAL ERROR: Symbol "', sym_p^.name_p^.str:sym_p^.name_p^.len, '"');
    writeln ('  not found in symbol table.  (ESCR_SYM_DEL)');
    sys_bomb;
    end;

  s_p := sym_p^.prev_p;                {get pointer to next older version of this symbol}
  if s_p = nil
    then begin                         {this is last version of this symbol name}
      string_hash_ent_del (pos);       {delete the symbol table entry}
      end
    else begin                         {at least one older version exists}
      s_p^.next_p := nil;              {delete forward reference to this symbol}
      string_hash_ent_atpos (pos, name_p, sym_pp); {get info about symbol table entry}
      sym_pp^ := s_p;                  {make previous version the current version}
      end
    ;

  util_mem_ungrab (sym_p, tbl_p^.mem_p^); {deallocate the symbol descriptor}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL_NAME (E, SYTABLE, NAME, STAT)
*
*   Delete the most recently created version of the symbol of name NAME from the
*   symbol table SYTABLE.
}
procedure escr_sym_del_name (          {delete symbol by name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sytable: escr_sytable_t;     {symbol table to delete symbol from}
  in      name: univ string_var_arg_t; {symbol name}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  name_p: string_var_p_t;              {pointer to name in symbol table}
  sym_pp: escr_sym_pp_t;               {pointer to symbol pointer in table entry}
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor to delete}
  pos: string_hash_pos_t;              {handle to position in symbol table}
  found: boolean;                      {TRUE if name found in symbol table}

begin
  sys_error_none (stat);               {init to no error encountered}

  string_hash_pos_lookup (             {find position of name in symbol table}
    sytable.hash,                      {hash table to find position in}
    name,                              {name to find position of}
    pos,                               {returned position}
    found);                            {TRUE if name found in table}
  if not found then begin              {name not found ?}
    sys_stat_set (escr_subsys_k, escr_err_nfsym_k, stat); {symbol not found}
    sys_stat_parm_vstr (name, stat);   {symbol name}
    return;
    end;

  string_hash_ent_atpos (pos, name_p, sym_pp); {get info from symbol table entry}
  sym_p := sym_pp^;                    {get pointer to symbol descriptor}
  escr_sym_del (e, sym_p);             {delete the symbol}
  end;
