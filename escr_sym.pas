{   Symbol handling routines.
}
module escr_sym;
define escr_sytable_init;
define escr_sytable_scan_start;
define escr_sytable_scan;
define escr_sym_sytype_name;
define escr_sym_name_sytype;
define escr_sym_name;
define escr_sym_name_bare;
define escr_sym_name_bare_check;
define escr_sym_name_parse;
define escr_sym_new;
define escr_sym_new_const;
define escr_sym_new_var;
define escr_sym_lookup_curr;
define escr_sym_lookup_ver;
define escr_sym_lookup_qual;
define escr_sym_lookup_sym;
define escr_sym_find_curr;
define escr_sym_find;
define escr_sym_find_type;
define escr_sym_del_pos;
define escr_sym_del;
define escr_sym_del_name;
define escr_sym_del_name_type;
define escr_sym_curr_sym;
define escr_sym_curr_sym;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Local subroutine ESCR_SYTABLE_INIT (E, SYTABLE, STAT)
*
*   Initialize the symbol table SYTABLE.
}
procedure escr_sytable_init (          {initialize a symbol table}
  in out  mem: util_mem_context_t;     {parent memory context}
  out     sytable: escr_sytable_t;     {the symbol table to initialize}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  util_mem_context_get (mem, sytable.mem_p); {create mem context for sym table}
  if sytable.mem_p = nil then begin    {didn't get new memory context ?}
    sys_stat_set (escr_subsys_k, escr_err_nomcontext_k, stat);
    return;
    end;

  string_hash_create (                 {create the symbol names hash table}
    sytable.hash,                      {handle to the hash table}
    escr_sym_nbuck_k,                  {number of hash buckets}
    escr_max_namelen_k,                {max characters for hash table entry name}
    sizeof(escr_sytable_data_t),       {size of data to store each hash table entry}
    [string_hashcre_memdir_k],         {use mem context directly, allow deleting entries}
    sytable.mem_p^);                   {memory context to use for table data}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYTABLE_SCAN_START (TABLE, SCAN)
*
*   Set up for scanning all entries in a symbol table.  TABLE is the table to
*   scan.  SCAN is the private state for this scan.  The next call to
*   ESCR_SYTABLE_SCAN with this SCAN will return the first entry in the table.
}
procedure escr_sytable_scan_start (    {start scanning all entries in a symbol table}
  in      table: escr_sytable_t;       {the symbol table to scan}
  out     scan: escr_sytable_scan_t);  {returned initialized scanning state}
  val_param;

begin
  string_hash_pos_first (              {get position of first entry}
    table.hash,                        {the hash table}
    scan.pos,                          {returned position of first entry}
    scan.valid);                       {TRUE if first entry exists}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYTABLE_SCAN (SCAN, NAME_P, ENT_P)
*
*   Gets the next symbol table entry in a scan of a symbol table.
*
*   NAME_P is returned pointing to the symbol name string, and ENT_P is returned
*   pointing to the data for that symbol stored in the symbol table.  Both
*   NAME_P and ENT_P are returned NIL if the end of the table is encountered.
*
*   SCAN is the symbol table scanning state.  It must have been originally
*   initialized with ESCR_SYTABLE_SCAN_START.
}
procedure escr_sytable_scan (          {get next symbol table entry}
  in out  scan: escr_sytable_scan_t;   {symbol table scanning state}
  out     name_p: string_var_p_t;      {returned pointer to symbol name in table}
  out     ent_p: escr_sytable_data_p_t); {returned pointer to next entry, NIL at end}
  val_param;

begin
  ent_p := nil;                        {init to not returning with new entry}
  name_p := nil;
  if not scan.valid then return;       {previously hit end of table ?}

  string_hash_ent_atpos (              {get the table info at this position}
    scan.pos,                          {position of entry to get info of}
    name_p,                            {returned pointing to entry name}
    ent_p);                            {returned pointing to entry data}

  string_hash_pos_next (               {advance to the next hash table entry}
    scan.pos,                          {current position in hash table}
    scan.valid);                       {TRUE if new position exists}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_SYTYPE_NAME (SYTYPE, NAME)
*
*   Get the name of the user-visible symbol type SYTYPE into NAME.  NAME is the
*   same name as is used to specify the type in a qualified symbol name.
}
procedure escr_sym_sytype_name (       {get name of user-visible symbol type}
  in      sytype: escr_sytype_k_t;     {user-visible type to get name of}
  in out  name: univ string_var_arg_t); {returned type name}
  val_param;

begin
  case sytype of
escr_sytype_unsp_k: string_vstring (name, 'UNSP'(0), -1);
escr_sytype_var_k: string_vstring (name, 'VAR'(0), -1);
escr_sytype_const_k: string_vstring (name, 'CONST'(0), -1);
escr_sytype_vcon_k: string_vstring (name, 'VCON'(0), -1);
escr_sytype_subr_k: string_vstring (name, 'SUBR'(0), -1);
escr_sytype_macro_k: string_vstring (name, 'MACRO'(0), -1);
escr_sytype_func_k: string_vstring (name, 'FUNC'(0), -1);
escr_sytype_cmd_k: string_vstring (name, 'CMD'(0), -1);
escr_sytype_label_k: string_vstring (name, 'LABEL'(0), -1);
otherwise
    writeln ('INTERNAL ERROR: Unexpected symbol type ID of ',
      ord(sytype), ' in SYM_SYTYPE_NAME');
    sys_bomb;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_SYM_NAME_SYTYPE (NAME)
*
*   Interpret NAME as a symbol type keyword and return the indicated symbol type
*   ID.  The valid symbol type keywords are:
*
*     VAR, CONST, VCON, SUBR, MACRO, FUNC, CMD, LABEL
*
*   If NAME is not one of the above, then the function returns
*   ESCR_SYTYPE_UNSP_K.
}
function escr_sym_name_sytype (        {interpret symbol type keyword}
  in      name: univ string_var_arg_t) {the keyword, must be upper case}
  :escr_sytype_k_t;                    {symbol type ID or ESCR_SYTYPE_UNSP_K}
  val_param;

var
  pick: sys_int_machine_t;             {number of keyword picked from list}

begin
  escr_sym_name_sytype := escr_sytype_unsp_k; {init to symbol type not recognized}

  string_tkpick80 (name,               {pick the type name keyword from list}
    'VAR CONST VCON SUBR MACRO FUNC CMD LABEL',
    pick);                             {1-N number of keyword picked from list}
  case pick of                         {which keyword is it ?}
1:  escr_sym_name_sytype := escr_sytype_var_k;
2:  escr_sym_name_sytype := escr_sytype_const_k;
3:  escr_sym_name_sytype := escr_sytype_vcon_k;
4:  escr_sym_name_sytype := escr_sytype_subr_k;
5:  escr_sym_name_sytype := escr_sytype_macro_k;
6:  escr_sym_name_sytype := escr_sytype_func_k;
7:  escr_sym_name_sytype := escr_sytype_cmd_k;
8:  escr_sym_name_sytype := escr_sytype_label_k;
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NAME (SYM, NAME)
*
*   Returns the fully qualified name of the symbol version SYM.
}
procedure escr_sym_name (              {make fully qualified name of a symbol}
  in      sym: escr_sym_t;             {symbol version to make full name of}
  in out  name: univ string_var_arg_t); {returned fully qualified name}
  val_param;

begin
  string_copy (sym.name_p^, name);     {init qualified name with bare symbol name}

  string_append1 (name, ':');          {add symbol type}
  case sym.stype of
escr_sym_var_k: string_appends (name, 'VAR'(0));
escr_sym_const_k: string_appends (name, 'CONST'(0));
escr_sym_subr_k, escr_sym_isubr_k: string_appends (name, 'SUBR'(0));
escr_sym_cmd_k, escr_sym_icmd_k: string_appends (name, 'CMD'(0));
escr_sym_func_k, escr_sym_ifunc_k: string_appends (name, 'FUNC'(0));
escr_sym_macro_k, escr_sym_imacro_k: string_appends (name, 'MACRO'(0));
escr_sym_label_k, escr_sym_src_k: string_appends (name, 'LABEL'(0));
    end;

  string_append1 (name, ':');          {add absolute version number}
  string_append_intu (name, sym.vern, 0);
  end;
{
********************************************************************************
*
*   Function ESCR_SYM_NAME_BARE (NAME)
*
*   Returns TRUE iff NAME is a valid bare symbol name.  Bare names do not
*   include any qualifiers, like the symbol type or version number.
}
function escr_sym_name_bare (          {check for valid symbol name}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
  val_param;

var
  ii: sys_int_machine_t;               {scratch loop counter}
  c: char;                             {scratch character}
  ccode: sys_int_machine_t;            {character code of C}

begin
  escr_sym_name_bare := false;         {init to not a valid symbol name}

  if name.len > escr_max_namelen_k then return; {name is too long ?}
  if name.len < 1 then return;         {name is too short ?}

  for ii := 1 to name.len do begin     {once for each character in name}
    c := name.str[ii];                 {get this symbol name character}
    ccode := ord(c);
    if ccode <= 32 then return;        {space or control character ?}
    if ccode > 126 then return;        {past 7 bit printable range ?}
    if c = ':' then return;            {colon not allowed in bare symbol name}
    end;                               {back and check next name character}

  escr_sym_name_bare := true;          {NAME is a valid symbol name}
  end;
{
********************************************************************************
*
*   ESCR_SYM_NAME_BARE_CHECK (NAME, STAT)
*
*   Set STAT according to whether NAME is a valid bare symbol name or not.  STAT
*   is set to no error when NAME is a valid bare symbol name, and a appropriate
*   error when not.
}
procedure escr_sym_name_bare_check (   {check for valid bare symbol name}
  in      name: univ string_var_arg_t; {the name to check}
  out     stat: sys_err_t);            {error iff NAME is not valid bare symbol name}
  val_param;

begin
  if escr_sym_name_bare (name)
    then begin                         {is a valid symbol name}
      sys_error_none (stat);           {return no error}
      end
    else begin                         {not a valid symbol name}
      sys_stat_set (escr_subsys_k, escr_err_badsym_k, stat); {return with error}
      sys_stat_parm_vstr (name, stat);
      end
    ;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NAME_PARSE (NAME, RAWNAME, SYTYPE, SYVER, STAT)
*
*   Parse the qualified symbol name NAME into its separate pieces of
*   information.  A qualified symbol name has the syntax:
*
*     name:type:version
*
*   NAME is required, and is the bare name of the symbol.  TYPE is optional, but
*   must be one of the following if it exists:
*
*     VAR
*     CONST
*     VCON
*     SUBR
*     MACRO
*     FUNC
*     CMD
*     LABEL
*
*   The default is that the type is not specified.
*
*   VERSION is a integer string that may be preceeded by "+" or "-".  Without
*   the leading "+" or "-", VERSION specifies the absolute version number of
*   the symbol.  With the leading "+" or "-", VERSION is the relative version
*   number from the current version.  Absolute version numbers start at 1 and
*   increase by 1 for each newer version of the same name.  Relative versions
*   with leading "-" therefore specify older versions of the symbol.  The
*   relative versions "+0" and "-0" are equivalent, and specify the current
*   version of the symbol.  The default is the current version.
*
*   If TYPE or VERSION is omitted, then its leading colon may also be omitted.
*   Therefore, a bare symbol name interpreted as a qualified symbol name leaves
*   the symbol type unspecified, and selects the current version of the symbol.
*
*   STAT is returned with error iff NAME is not a valid qualified symbol name.
}
procedure escr_sym_name_parse (        {parse qualified symbol name}
  in      name: univ string_var_arg_t; {input qualified symbol name}
  in out  rawname: univ string_var_arg_t; {returned bare symbol name}
  out     sytype: escr_sytype_k_t;     {user-visible symbol type ID}
  out     syver: escr_syver_t;         {symbol version information}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  p: string_index_t;                   {NAME parse index}
  tk: string_var132_t;                 {token parsed from NAME}
  ii: sys_int_machine_t;               {scratch integer}

label
  done_type, try_ver, done_ver, error;

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error encountered}
  rawname.len := 0;                    {init returned data to defaults}
  sytype := escr_sytype_unsp_k;
  syver.ver := 0;
  syver.set := false;
  syver.abs := false;

  p := 1;                              {init input name parse index}
{
*   Get the bare symbol name.
}
  string_token_anyd (                  {get bare name token}
    name,                              {input string to parse}
    p,                                 {parse index}
    ':', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special handling}
    tk,                                {returned parsed token}
    ii,                                {number of ending delimiter (not used)}
    stat);
  if sys_error(stat) then goto error;

  if not escr_sym_name_bare (tk)       {bare name is not valid ?}
    then goto error;

  string_copy (tk, rawname);           {return the bare symbol name}
{
*   Get the symbol type, if specified.
}
  string_token_anyd (                  {get next token in qualified symbol name}
    name,                              {input string to parse}
    p,                                 {parse index}
    ':', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special handling}
    tk,                                {returned parsed token}
    ii,                                {number of ending delimiter (not used)}
    stat);
  if string_eos(stat) then return;     {no more tokens in qualified name ?}
  if tk.len <= 0 then goto done_type;  {type left empty ?}

  string_upcase (tk);                  {make upper case for keyword matching}
  sytype := escr_sym_name_sytype (tk); {get symbol type ID indicated by token}
  if sytype = escr_sytype_unsp_k then begin {not a recognized symbol type name ?}
    goto try_ver;
    end;
done_type:                             {done getting symbol type}
{
*   Get the symbol version information.
}
  string_token_anyd (                  {get next token in qualified symbol name}
    name,                              {input string to parse}
    p,                                 {parse index}
    ':', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special handling}
    tk,                                {returned parsed token}
    ii,                                {number of ending delimiter (not used)}
    stat);
  if string_eos(stat) then return;     {no more tokens in qualified name ?}
  if tk.len <= 0 then goto done_ver;   {version left empty ?}

try_ver:                               {try to interpret TK as version specifier}
  string_t_int (tk, ii, stat);         {convert token to integer in II}
  if sys_error(stat) then goto error;  {not a valid version specifier ?}
  syver.ver := ii;                     {save relative or absolute version number}
  syver.set := true;                   {version info is definitely set}
  syver.abs :=                         {absolute, no "+" or "-" ?}
    (not (tk.str[1] = '+')) and
    (not (tk.str[1] = '-'));
  if syver.abs and (syver.ver <= 0) then goto error; {invalid absolute version ?}

done_ver:                              {done getting symbol version info}
{
*   Verify that there is nothing left to parse from the input name.
}
  if p <= name.len then goto error;    {extraneous characters in the input name ?}
  return;                              {done parsing everything, no errors}

error:                                 {error parsing symbol, not a valid symbol name}
  sys_stat_set (escr_subsys_k, escr_err_badsym_k, stat);
  sys_stat_parm_vstr (name, stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW (E, NAME, SZ, GLOBAL, SYTABLE, SYM_P, STAT)
*
*   Low level routine to add or create a new version of a symbol in a specific
*   symbol table.  If the symbol previously exists, the new version will be
*   added immediately after the current version, and made current.  If the
*   existing current version was not the last, then later versions are
*   renumbered so that each newer version is numbered 1 more than the previous
*   version.
*
*   NAME is the name of the symbol to create or add a new version of.  It must
*   be a valid bare symbol name.
*
*   SZ is the size of the whole symbol descriptor to create.  This varies
*   depending on the type of symbol and the data associated with it.
*
*   GLOBAL indicates that the new symbol is to have global scope.  When false, a
*   local symbol is created that will automatically be deleted when the current
*   execution block ends.  All symbols in the top block are created global
*   regardless of the value of GLOBAL.
*
*   SYTABLE is the symbol table to create the new symbol in.  For example,
*   E.SYM_VAR is the symbol table for variables and constants.
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
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sz: sys_int_adr_t;           {size of the whole symbol descriptor}
  in      global: boolean;             {create global, not local symbol}
  in out  sytable: escr_sytable_t;     {symbol table to add symbol to}
  out     sym_p: escr_sym_p_t;         {returned pointer to symbol info}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  name_p: string_var_p_t;              {pointer to name in symbol table}
  ent_p: escr_sytable_data_p_t;        {pointer to data in symbol table}
  vern: sys_int_machine_t;             {1-N version number of the new symbol}
  sy_p: escr_sym_p_t;                  {scratch pointer to symbol descriptor}
  ii: sys_int_machine_t;               {scratch integer}
  lsym_p: escr_sylist_p_t;             {pointer to local symbol list entry for ex block}
  pos: string_hash_pos_t;              {handle to position in symbol table}
  found: boolean;                      {TRUE if name found in symbol table}

label
  local;

begin
  sym_p := nil;                        {init to not returning with new symbol}

  if not escr_sym_name_bare (name) then begin {not a valid bare symbol name ?}
    sys_stat_set (escr_subsys_k, escr_err_badsym_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;

  string_hash_pos_lookup (             {find position of name in symbol table}
    sytable.hash,                      {hash table to find position in}
    name,                              {name to find position of}
    pos,                               {returned position}
    found);                            {TRUE if name found in table}
  if found
    then begin                         {a previous version of this symbol exists}
      string_hash_ent_atpos (pos, name_p, ent_p); {look up existing symbol info}

      if ent_p^.last_p^.vern >= escr_max_symvers_k then begin {would be too many versions ?}
        sys_stat_set (escr_subsys_k, escr_err_sytoomany_k, stat);
        sys_stat_parm_vstr (name, stat);
        return;
        end;

      if ent_p^.curr_p = nil
        then begin                     {new symbol will be added to start of chain}
          vern := 1;                   {absolute version of new symbol}
          sy_p := ent_p^.first_p;      {point to first symbol to renumber}
          end
        else begin                     {curr vers exists, new symbol will be added after}
          vern := ent_p^.curr_p^.vern + 1; {new version is one after current}
          sy_p := ent_p^.curr_p^.next_p; {point to first symbol to renumber, if any}
          end
        ;

      ii := vern + 1;                  {init number to assign to next version}
      while sy_p <> nil do begin       {loop over versions to renumber}
        sy_p^.vern := ii;              {update number of this version}
        ii := ii + 1;                  {update number to assign to next version}
        sy_p := sy_p^.next_p;          {advance to next version}
        end;
      end
    else begin                         {no symbol of this name currently exists}
      string_hash_ent_add (pos, name_p, ent_p); {create new symbol table entry}
      ent_p^.first_p := nil;           {init table entry data}
      ent_p^.curr_p := nil;
      ent_p^.last_p := nil;
      vern := 1;                       {this will be version 1 of this symbol name}
      end
    ;
{
*   The symbol table entry for this name exists.  NAME_P is pointing to the name
*   string in the symbol table and ENT_P to the data.  VERN is the 1-N version
*   of the symbol to create.
*
*   Now create the new version of the symbol, link it into the chain of existing
*   versions immediately after the current version, and make it the current
*   version.  VERN is the number of the new version.  Other versions have
*   already been renumbered as necessary.
}
    {
    *   Create the new symbol version descriptor.
    }
    util_mem_grab (                    {allocate memory for new symbol descriptor}
      sz, sytable.mem_p^, true, sym_p);
    if sym_p = nil then begin
      sys_stat_set (escr_subsys_k, escr_err_nomem_k, stat);
      sys_stat_parm_int (sz, stat);
      return;
      end;
    sys_error_none (stat);             {will return with new symbol}
    {
    *   Fill in the new descriptor.
    }
    sym_p^.table_p := addr(sytable);   {point to symbol table this symbol is in}
    sym_p^.ent_p := ent_p;             {point to symbol table data for this symbol}
    sym_p^.prev_p := ent_p^.curr_p;    {point back to previous version of this symbol}
    sym_p^.name_p := name_p;           {point to name in symbol table}
    sym_p^.vern := vern;               {set the 1-N version number}
    sym_p^.scope_p := nil;             {init to global scope}
    {
    *   Link this symbol into the chain after PREV_P^.
    }
    if sym_p^.prev_p = nil
      then begin                       {install at start of chain}
        sym_p^.next_p := ent_p^.first_p;
        ent_p^.first_p := sym_p;
        end
      else begin                       {install after current version}
        sym_p^.next_p := sym_p^.prev_p^.next_p;
        sym_p^.prev_p^.next_p := sym_p;
        end
      ;
    if sym_p^.next_p = nil
      then begin                       {installing at end of chain}
        ent_p^.last_p := sym_p;
        end
      else begin                       {not at end of chain}
        sym_p^.next_p^.prev_p := sym_p;
        end
      ;
    ent_p^.curr_p := sym_p;            {this new version is now the current}

    if e.exblock_p = nil then return;  {no block, can't be local to block ?}
    if not global then goto local;     {create as local symbol ?}
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
*   Local subroutine ESCR_SYM_LOOKUP (NAME, SYTABLE, HPOS, ENT_P)
*
*   Look up a bare name in the symbol table SYTABLE.  If a symbol table entry
*   with that name exists, then HPOS is set to the symbol table position for
*   this symbol, and ENT_P is returned pointing to the data for that symbol.  If
*   the name does not exist in the table, then HPOS is invalid and ENT_P is
*   returned NIL.
}
procedure escr_sym_lookup (            {look up bare name in specific symbol table}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sytable: escr_sytable_t;     {symbol table to look up name in}
  out     hpos: string_hash_pos_t;     {returned position of name in symbol table}
  out     ent_p: escr_sytable_data_p_t); {pointer to data in symbol table, NIL not found}
  val_param;

var
  found: boolean;                      {name was found in symbol table}
  name_p: string_var_p_t;              {pointer to name in symbol table}

begin
  ent_p := nil;                        {init to symbol not found}

  string_hash_pos_lookup (             {find name in symbol table}
    sytable.hash,                      {handle to table to look up in}
    name,                              {name to look up}
    hpos,                              {returned position in the table}
    found);                            {TRUE iff name found in table}
  if not found then return;            {name not found ?}

  string_hash_ent_atpos (              {get the data from the symbol table}
    hpos,                              {handle to position in the table}
    name_p,                            {pointer to name string stored in table}
    ent_p);                            {pointer to data stored for this symbol}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_LOOKUP_VER (E, NAME, SYTYPE, SYVER, HPOS, SYM_P)
*
*   Find a particular version of a symbol.  NAME is the bare symbol name.  SYTPE
*   specifies the user-visible symbol type, and SYVER its version.
*
*   HPOS is the returned position of the symbol name in the symbol table.  This
*   is only valid when SYM_P is not NIL.  SYM_P is returned pointing to the
*   specific version of the symbol.  SYM_P is NIL if the symbol or version does
*   not exist.
}
procedure escr_sym_lookup_ver (        {get specific version of a symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  in      syver: escr_syver_t;         {version specifier}
  out     hpos: string_hash_pos_t;     {returned position of name in symbol table}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param;

const
  max_msg_args = 2;                    {max arguments we can pass to a message}

var
  ent_p: escr_sytable_data_p_t;        {pointer to symbol data in table}
  absver: sys_int_machine_t;           {absolute version number looking for}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;

label
  found, haver, err_nover;

begin
  sym_p := nil;                        {init to symbol version not found}

  case sytype of                       {what user-visible symbol type is it ?}

escr_sytype_unsp_k: begin              {type not specified, search in default order}
    escr_sym_lookup (name, e.sym_var, hpos, ent_p); {variables and constants}
    if ent_p <> nil then goto found;
    escr_sym_lookup (name, e.sym_sub, hpos, ent_p); {subroutines}
    if ent_p <> nil then goto found;
    escr_sym_lookup (name, e.sym_mac, hpos, ent_p); {macros}
    if ent_p <> nil then goto found;
    escr_sym_lookup (name, e.sym_fun, hpos, ent_p); {functions}
    if ent_p <> nil then goto found;
    escr_sym_lookup (name, e.sym_cmd, hpos, ent_p); {commands}
    if ent_p <> nil then goto found;
    escr_sym_lookup (name, e.sym_lab, hpos, ent_p); {labels}
    end;

escr_sytype_var_k,                     {variable}
escr_sytype_const_k,                   {constant}
escr_sytype_vcon_k: begin              {variable or constant}
    escr_sym_lookup (name, e.sym_var, hpos, ent_p); {variables and constants}
    end;

escr_sytype_subr_k: begin              {subroutine}
    escr_sym_lookup (name, e.sym_sub, hpos, ent_p);
    end;

escr_sytype_macro_k: begin             {macro}
    escr_sym_lookup (name, e.sym_mac, hpos, ent_p);
    end;

escr_sytype_func_k: begin              {function}
    escr_sym_lookup (name, e.sym_fun, hpos, ent_p);
    end;

escr_sytype_cmd_k: begin               {command}
    escr_sym_lookup (name, e.sym_cmd, hpos, ent_p);
    end;

escr_sytype_label_k: begin             {label}
    escr_sym_lookup (name, e.sym_lab, hpos, ent_p);
    end;

otherwise
    writeln ('INTERNAL ERROR: Unexpected user-visible symbol type in ESCR_SYM_LOOKUP_VER.');
    writeln ('  Type ID = ', ord(sytype));
    escr_err_atline (e, '', '', nil, 0);
    end;

  if ent_p = nil then return;          {named symbol does not exist}

found:                                 {symbol with matching type and name was found}
{
*   ENT_P is pointing to the symbol table data for the symbol.
*
*   Now find the right version of the symbol.
}
  if
      syver.abs or                     {absolute version specified directly ?}
     (ent_p^.curr_p = nil)             {current version is virtual version 0 ?}
    then begin
      absver := syver.ver;             {set absolute version to look for}
      end
    else begin
      absver := ent_p^.curr_p^.vern + syver.ver; {make absolute version from current}
      end
    ;

  if absver < 1 then return;           {definitely invalid absolute version ?}
  if absver > ent_p^.last_p^.vern then return;

  if ent_p^.curr_p = nil
    then sym_p := ent_p^.first_p       {start searching at first when no current}
    else sym_p := ent_p^.curr_p;       {start searching at current version}

  if sym_p^.vern = absver              {already at the desired version ?}
    then goto haver;
  if absver > sym_p^.vern
    then begin                         {look for later version}
      repeat
        sym_p := sym_p^.next_p;        {to next newer version}
        if sym_p = nil then goto err_nover;
        until sym_p^.vern = absver;
      end
    else begin                         {look for earlier version}
      repeat
        sym_p := sym_p^.prev_p;        {to next older version}
        if sym_p = nil then goto err_nover;
        until sym_p^.vern = absver;
      end
    ;

haver:                                 {SYM_P is pointing to the specified version}
{
*   If a specific symbol type was specified and other types can be in the same
*   symbol table, then check that the final version is of the right type.  If
*   not, then the version effectively doesn't exist, so SYM_P is returned NIL.
}
  case sytype of                       {what type of symbol was specified ?}
escr_sytype_var_k: begin               {variable}
      if sym_p^.stype <> escr_sym_var_k
        then sym_p := nil;
      end;
escr_sytype_const_k: begin             {constant}
      if sym_p^.stype <> escr_sym_const_k
        then sym_p := nil;
      end;
    end;                               {end of special handling symbol types}
  return;

err_nover:
  sys_msg_parm_int (msg_parm[1], absver);
  sys_msg_parm_vstr (msg_parm[2], name);
  escr_err_atline (e, 'escr', 'err_lookupver_nover', msg_parm, 2);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_LOOKUP_QUAL (E, NAME, HPOS, SYM_P, STAT)
*
*   Find a symbol version from the qualified symbol name NAME.  SYM_P is
*   returned pointing to the indicated version of the symbol.  HPOS is returned
*   the position of the symbol name in the symbol table.
*
*   If no such symbol or version exists, then HPOS is returned invalid and SYM_P
*   is returned NIL.  When a error is encountered, STAT is set to indicate the
*   error, and SYM_P is returned NIL.  HPOS is undefined on error.
}
procedure escr_sym_lookup_qual (       {get symbol version from qualified name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  out     hpos: string_hash_pos_t;     {returned position of name in symbol table}
  out     sym_p: escr_sym_p_t;         {returned pointer to version, NIL if not found}
  out     stat: sys_err_t);            {completion status, SYM_P = NIL on error}
  val_param;

var
  rawname: string_var80_t;             {bare symbol name}
  sytype: escr_sytype_k_t;             {user-visible symbol type}
  syver: escr_syver_t;                 {symbol version information}

begin
  rawname.max := size_char(rawname.str); {init local var string}
  sym_p := nil;                        {init to no such symbol found}

  escr_sym_name_parse (                {extract info from qualified symbol name}
    name,                              {qualified symbol name input}
    rawname,                           {returned bare symbol name}
    sytype,                            {returned user-visible symbol type}
    syver,                             {returned version information}
    stat);
  if sys_error(stat) then return;      {invalid qualified symbol name ?}

  escr_sym_lookup_ver (                {get the specific version of the symbol}
    e,                                 {ESCR library use state}
    rawname,                           {base symbol name}
    sytype,                            {user-visible symbol type}
    syver,                             {absolute or relative version description}
    hpos,                              {returned position in symbol table}
    sym_p);                            {returned pointer to specific version}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_LOOKUP_SYM (E, SYM, HPOS)
*
*   Look up the position in the symbol table of the symbol for which SYM is one
*   version.  HPOS is the returned position in the symbol table for the symbol
*   name.
}
procedure escr_sym_lookup_sym (        {look up symbol table position of symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      sym: escr_sym_t;             {symbol to look up name of}
  out     hpos: string_hash_pos_t);    {returned position of name in symbol table}
  val_param;

var
  found: boolean;

begin
  string_hash_pos_lookup (             {get table position from name}
    sym.table_p^.hash,                 {hash table to look up in}
    sym.name_p^,                       {name to look up}
    hpos,                              {returned position in the table}
    found);                            {TRUE if table entry exists}
  if not found then begin              {no such name in the symbol table ?}
    writeln ('INTERNAL ERROR: Could not find symbol ',
      sym.name_p^.str:sym.name_p^.len, ' in symbol table.');
    escr_err_atline (e, '', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_FIND_CURR (E, NAME, SYTYPE, SYM_P)
*
*   Find the current version of the symbol NAME of the user-visible type SYTYPE.
*   SYM_P is returned pointing to the specific version of the symbol.  SYM_P is
*   returned NIL when the symbol is not found.
}
procedure escr_sym_find_curr (         {find current version of a symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param;

var
  syver: escr_syver_t;                 {symbol version specifier}
  hpos: string_hash_pos_t;             {position in symbol table}

begin
  sym_p := nil;                        {init to symbol not found}

  syver.ver := 0;                      {specify relative version 0 (current vers)}
  syver.set := true;
  syver.abs := false;

  escr_sym_lookup_ver (                {look up this version of this symbol}
    e,                                 {ESCR library use state}
    name,                              {symbol name}
    sytype,                            {user-visibly symbol type ID}
    syver,                             {version specifier}
    hpos,                              {returned position in symbol table, unused}
    sym_p);                            {returned pointer to symbol version}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_FIND (E, NAME, SYM_P)
*
*   Find the symbol NAME.  NAME may be fully qualified.  SYM_P is returned
*   pointing to the specific version of the symbol.  SYM_P is returned NIL when
*   the symbol is not found.
}
procedure escr_sym_find (              {find version from qualified symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {qualified symbol name}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param;

var
  hpos: string_hash_pos_t;             {position in symbol table}
  stat: sys_err_t;

begin
  escr_sym_lookup_qual (               {get symbol version from qualified name}
    e,                                 {ESCR library use state}
    name,                              {qualified symbol name}
    hpos,                              {returned position in symbol table, unused}
    sym_p,                             {returned pointer to symbol version}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_FIND_TYPE (E, NAME, SYTYPE, SYM_P, STAT)
*
*   Find the symbol NAME of the user-visible type SYTYPE.  NAME may be a
*   qualified symbol version name.  It is a error if NAME includes a specific
*   symbol type, and that type does not match SYTYPE.
*
*   SYM_P is returned pointing to the specified version of the symbol.  If the
*   symbol or version do not exist or on error, SYM_P is returned NIL.
}
procedure escr_sym_find_type (         {find symbol of specific type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be qualified}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  out     sym_p: escr_sym_p_t;         {returned pointer to version, NIL if not found}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  rawname: string_var80_t;             {bare symbol name specified in NAME}
  namtype: escr_sytype_k_t;            {symbol type specified in NAME}
  syver: escr_syver_t;                 {version specified in NAME}
  hpos: string_hash_pos_t;             {symbol table position}

label
  typeok;

begin
  rawname.max := size_char(rawname.str); {init local var string}
  sym_p := nil;                        {init to not returning with symbol version}

  escr_sym_name_parse (                {get info from qualified symbol name}
    name,                              {qualified name to parse}
    rawname,                           {raw symbol name}
    namtype,                           {symbol type}
    syver,                             {version specification}
    stat);
  if sys_error(stat) then return;      {invalid qualified symbol name ?}

  if
      (sytype <> escr_sytype_unsp_k) and {a particular symbol type is required ?}
      (namtype <> escr_sytype_unsp_k) and {a particular type specified in NAME ?}
      (namtype <> sytype)              {the two don't match ?}
      then begin
    case sytype of                     {what type of symbol is required ?}
escr_sytype_vcon_k: begin
        if (namtype = escr_sytype_var_k) or (namtype = escr_sytype_const_k)
          then goto typeok;
        end;
      end;
    sys_stat_set (escr_subsys_k, escr_err_sytype_k, stat);
    escr_sym_sytype_name (sytype, rawname);
    sys_stat_parm_vstr (rawname, stat);
    escr_sym_sytype_name (namtype, rawname);
    sys_stat_parm_vstr (rawname, stat);
    return;                            {return with symbol type mismatch error}
    end;
typeok:                                {the sym type in NAME is acceptable}

  escr_sym_lookup_ver (                {look up the specific version of the symbol}
    e,                                 {library use state}
    rawname,                           {bare name of the symbol}
    sytype,                            {user-visible symbol type}
    syver,                             {version specifier}
    hpos,                              {returned symbol table position}
    sym_p);                            {returned pointer to the symbol version}
  end;
{
********************************************************************************
*
*   Local subroutine SYMVER_DEALLOC (SYM_P)
*
*   Deallocate the memory for the symbol at SYM_P, and return SYM_P NIL.  If
*   SYM_P^ is a local symbol, then it is also deleted from the local symbols
*   list of its execution block.  The symbol descriptor will not be un-linked,
*   just deallocated.
}
procedure symver_dealloc (             {deallocate sym version mem, remove from locals list}
  in out  sym_p: escr_sym_p_t);        {pointer to symbol version to deallocate}
  val_param; internal;

var
  lsym_p: escr_sylist_p_t;             {pointer to one local symbols list entry}
  lsym_pp: escr_sylist_pp_t;           {pointer to link to current local symbol list entry}
  tbl_p: escr_sytable_p_t;             {pointer to symbol table this symbol is in}

begin
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

  tbl_p := sym_p^.table_p;             {get pointer to parent symbol table}
  util_mem_ungrab (sym_p, tbl_p^.mem_p^); {deallocate the symbol descriptor}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL_POS (E, SYM_P, HPOS, STAT)
*
*   Delete the symbol version pointed to by SYM_P.  SYM_P is returned NIL.  HPOS
*   is the hash table position entry for the symbol.
*
*   Versions with higher numbers are re-numbered to keep all versions
*   sequentially numbered starting at 1.
}
procedure escr_sym_del_pos (           {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t;         {pointer to version to delete, returned NIL}
  in out  hpos: string_hash_pos_t;     {table pos for this symbol, invalid if deleted}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  exb_p: escr_exblock_p_t;             {pointer to execution block in hierarchy}
  name_p: string_var_p_t;              {pointer to name in symbol table}
  ent_p: escr_sytable_data_p_t;        {pointer to data in symbol table}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  sy_p: escr_sym_p_t;                  {scratch pointer to a symbol version}

begin
  sys_error_none (stat);               {init to no error encountered}
{
*   Check for attempting to delete the name symbol of a execution block, or the
*   version of a symbol to restore to being the current version when a block is
*   closed.  Allowing these to be deleted would result in dangling pointers.
*   Instead of trying to fix up the pointers, we simply make it illegal to
*   delete these versions of the symbols.
}
  exb_p := e.exblock_p;                {init to lowest level execution block}
  while exb_p <> nil do begin          {scan up the hierarchy of execution blocks}
    if
        (sym_p = exb_p^.sym_p) or      {name symbol of this block ?}
        (sym_p = exb_p^.sym_curr_p)    {version to restore to current after this block ?}
        then begin
      sys_stat_set (escr_subsys_k, escr_err_delsymblk_k, stat);
      sys_stat_parm_vstr (sym_p^.name_p^, stat);
      return;
      end;
    exb_p := exb_p^.prev_p;            {up to previous block in the hierarchy}
    end;
{
*   Get the information about this symbol from the symbol table.
}
  string_hash_ent_atpos (              {get table data for this symbol}
    hpos,                              {position in symbol table to get data on}
    name_p,                            {returned pointer to symbol name string}
    ent_p);                            {returned pointer to symbol data in table}
{
*   Renumber all subsequent versions of this symbol.
}
  ii := sym_p^.vern;                   {init number to assign to next version}
  sy_p := sym_p^.next_p;               {init to first version to renumber}
  while sy_p <> nil do begin           {scan the remaining versions}
    sy_p^.vern := ii;                  {assign new number to this version}
    ii := ii + 1;                      {make next number to assign}
    sy_p := sy_p^.next_p;              {advance to next newer version}
    end;
{
*   Unlink this version from the chain of versions.
}
  if sym_p = ent_p^.curr_p then begin  {deleting the current version ?}
    ent_p^.curr_p := sym_p^.prev_p;    {make the next previous version current, if any}
    end;

  if sym_p^.prev_p = nil
    then begin                         {at start of chain}
      ent_p^.first_p := sym_p^.next_p;
      end
    else begin                         {there is a older version}
      sym_p^.prev_p^.next_p := sym_p^.next_p;
      end
    ;

  if sym_p^.next_p = nil
    then begin                         {at end of chain}
      ent_p^.last_p := sym_p^.prev_p;
      end
    else begin                         {there is a newer version}
      sym_p^.next_p^.prev_p := sym_p^.prev_p;
      end
    ;
{
*   Deallocate the unliked version.  Delete the whole symbol if this was the
*   only version.
}
  symver_dealloc (sym_p);              {deallocate version, delete from local sym list}

  if ent_p^.first_p = nil then begin   {no versions of this symbol are left ?}
    string_hash_ent_del (hpos);        {delete the symbol table entry for this name}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL (E, SYM_P, STAT)
*
*   Delete the version of the symbol pointed to by SYM_P.  SYM_P is returend
*   NIL.
}
procedure escr_sym_del (               {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t;         {pointer to version to delete, returned NIL}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  hpos: string_hash_pos_t;             {position of symbol name in symbol table}

begin
  escr_sym_lookup_sym (e, sym_p^, hpos); {get position of name in symbol table}
  escr_sym_del_pos (e, sym_p, hpos, stat); {delete this version of the symbol}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL_NAME (E, NAME, STAT)
*
*   Delete a version of a symbol.  NAME is the name of the symbol, and may be
*   fully qualified.  It is a error if no such version or symbol exists.
}
procedure escr_sym_del_name (          {delete symbol version by name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor to delete}
  hpos: string_hash_pos_t;             {handle to position in symbol table}

begin
  escr_sym_lookup_qual (               {find symbol version from qualified name}
    e,                                 {library use state}
    name,                              {qualified name of symbol version}
    hpos,                              {returned position in symbol table}
    sym_p,                             {returned pointer to specified version}
    stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {not found ?}
    sys_stat_set (escr_subsys_k, escr_err_nfsym_k, stat); {symbol not found}
    sys_stat_parm_vstr (name, stat);   {symbol name}
    return;
    end;

  escr_sym_del_pos (e, sym_p, hpos, stat); {delete the symbol version}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_DEL_NAME_TYPE (E, NAME, SYTYPE, STAT)
*
*   Delete a version of a symbol.  NAME is the name of the symbol, and may be
*   fully qualified.  SYTYPE specifies the type of the symbol.  It is a error
*   if a symbol with the indicated name and type does not exist.
}
procedure escr_sym_del_name_type (     {delete symbol version by name, specific type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  in      sytype: escr_sytype_k_t;     {symbol must be this type}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  rawname: string_var80_t;             {bare symbol name}
  natype: escr_sytype_k_t;             {symbol type specified in NAME}
  naver: escr_syver_t;                 {symbol version specified in NAME}
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor to delete}
  hpos: string_hash_pos_t;             {handle to position in symbol table}

begin
  rawname.max := size_char(rawname.str); {init local var string}

  escr_sym_name_parse (                {extract info from qualified symbol name}
    name,                              {qualified symbol name input}
    rawname,                           {returned bare symbol name}
    natype,                            {returned user-visible symbol type}
    naver,                             {returned version information}
    stat);
  if sys_error(stat) then return;      {invalid qualified symbol name ?}

  while true do begin                  {resolve required symbol type in NATYPE}
    if natype = sytype then exit;      {parsed type matches user request ?}
    if sytype = escr_sytype_unsp_k then exit; {caller is OK with any type ?}
    if natype = escr_sytype_unsp_k then begin {name doesn't specify a type ?}
      natype := sytype;                {use the caller's specified type}
      exit;
      end;
    if
        (sytype = escr_sytype_vcon_k) and ( {user wants variable or constant ?}
          (natype = escr_sytype_var_k) or {is a variable ?}
          (natype = escr_sytype_const_k) {is a constant ?}
          )
      then exit;                       {qualified name matches user request}
    sys_stat_set (escr_subsys_k, escr_err_sytypemm_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;                            {qualified type doesn't match required type}
    end;

  escr_sym_lookup_ver (                {look up the specific symbol}
    e,                                 {ESCR library use state}
    rawname,                           {base symbol name}
    natype,                            {symbol type}
    naver,                             {symbol version}
    hpos,                              {returned position in symbol table}
    sym_p);                            {returned pointer to the symbol}
  if sym_p = nil then begin            {not found ?}
    sys_stat_set (escr_subsys_k, escr_err_nfsym_k, stat); {symbol not found}
    sys_stat_parm_vstr (name, stat);   {symbol name}
    return;
    end;

  escr_sym_del_pos (e, sym_p, hpos, stat); {delete the symbol version}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_CURR_SYM (SYM)
*
*   Make the version of the symbol SYM the current version.
}
procedure escr_sym_curr_sym (          {set current version of symbol}
  in out  sym: escr_sym_t);            {the version to make current}
  val_param;

begin
  sym.ent_p^.curr_p := addr(sym);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_CURR_PREV (SYM)
*
*   Make the previous version of a symbol current.  SYM is the version that will
*   be one newer than the new previous.  Put another way, after this call, the
*   current version will be the previous version to SYM.
}
procedure escr_sym_curr_prev (         {make previous version of symbol current}
  in out  sym: escr_sym_t);            {make previous of this version current}
  val_param;

begin
  sym.ent_p^.curr_p := sym.prev_p;
  end;
