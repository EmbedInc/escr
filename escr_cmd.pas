{   Module of routines that implement individual commands.  Some larger command
*   routines are in their own modules, called ESCR_CMD_<name>.  The smaller
*   command routines are collected here.
*
*   All the command subroutines have a common interface:
*
*     escr_cmd_<name> (E, STAT)
*
*   The input line is in IBUF and IP is the parse index after the command name.
*   The upper case command name is in CMD.  The program will abort and indicate
*   the current source line if STAT is returned other than normal status.  An
*   error is also automatically generated if the input line is not exhausted.
*   STAT is initialized to normal status before the command routine is called.
*
*   The variable OBUF can be used to write lines to the output file.  Data is
*   written to OBUF directly, which is then written to the output file using
*   subroutine WRITE_OBUF.  OBUF is automatically reset to empty when its
*   contents is written.  OBUF starts empty when a command routine is called.
*   If anything is left in OBUF when the command routine returns, it will be
*   written to the output file.
}
module escr_cmd;
define escr_cmd_var;
define escr_cmd_const;
define escr_cmd_set;
define escr_cmd_append;
define escr_cmd_del;
define escr_cmd_sylist;
define escr_cmd_include;
define escr_cmd_write;
define escr_cmd_show;
define escr_cmd_writepush;
define escr_cmd_writepop;
define escr_cmd_stop;
define escr_cmd_run;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   VAR (NEW, EXIST, LOCAL) name [dtype] [= value]
}
procedure escr_cmd_var (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

type
  scmd_k_t = (                         {subcommand ID}
    scmd_new_k,                        {NEW}
    scmd_exist_k);                     {EXIST}

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: escr_sym_p_t;                 {pointer to variable symbol}
  scmd: scmd_k_t;                      {subcommand ID}
  dtype: escr_dtype_k_t;               {variable's data type}
  name: string_var80_t;                {variable name}
  tk: string_var80_t;                  {scratch token}
  p: string_index_t;                   {string parse index}
  val: escr_val_t;                     {scratch variable value descriptor}
  global: boolean;                     {create global, not local symbol}
  hval: boolean;                       {initial value was specified}

label
  done_cmdline, make_new, err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);
{
*   Read and validate the command parameters.
}
  global := true;                      {init to create global symbol}
  escr_get_keyword (e,                 {get subcommand choice and set SCMD}
    'NEW EXIST LOCAL',
     pick, stat);
  case pick of
1:  scmd := scmd_new_k;                {NEW}
2:  scmd := scmd_exist_k;              {EXISTS}
3:  begin                              {LOCAL}
      if not e.exblock_p^.iter1 then begin {not first iteration of a loop ?}
        e.parse_p^.ip := e.parse_p^.ibuf.len + 1; {indicate whole input line used}
        return;
        end;
      scmd := scmd_new_k;
      global := false;
      end;
otherwise
    return;
    end;

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then goto err_missing;

  dtype := escr_dtype_str_k;           {init to default data type}
  hval := false;                       {init to no initial value specified}
  p := e.parse_p^.ip;                  {save parse index before getting next token}
  if escr_get_token (e, tk) then begin {some token exists after NAME ?}
    if                                 {not the "=" keyword, must be data type ?}
        (tk.len <> 1) or
        (tk.str[1] <> '=')
        then begin
      e.parse_p^.ip := p;              {restore parse position to before token}
      discard( escr_get_dtype (e, dtype, stat) ); {get the optional data type}
      if sys_error(stat) then return;
      if not escr_get_token (e, tk) then goto done_cmdline; {hit end of command line ?}
      if (tk.len <> 1) or (tk.str[1] <> '=') then begin {not "=" keyword as expected ?}
        sys_stat_set (escr_subsys_k, escr_err_badparm_k, stat);
        sys_stat_parm_vstr (e.parse_p^.cmd, stat);
        sys_stat_parm_vstr (tk, stat);
        return;
        end;
      end;
    hval := true;                      {indicate initial value parameter still left}
    end;
done_cmdline:                          {done reading command line except initial value}
{
*   Don't do anything if the subcommand is EXIST and a symbol with the
*   proper parameters already exists.
}
  if not (scmd = scmd_exist_k) then goto make_new; {not asked to re-use existing ?}
  escr_sym_find_curr (                 {look for existing variable of this name}
    e, name, escr_sytype_var_k, sym_p);
  if sym_p = nil then goto make_new;   {no such variable ?}
  if sym_p^.var_val.dtype <> dtype then goto make_new; {symbol isn't right dtype ?}

  if hval then begin                   {still need to read initial value parameter ?}
    val.dtype := dtype;                {indicate data type initial value must have}
    if not escr_get_val_dtype (e, val, stat) {read initial value only to verify it}
      then goto err_missing;
    end;
  return;                              {use the existing symbol}
{
*   Create a new symbol according to the specifications.
}
make_new:                              {a new symbol needs to be created}
  escr_sym_new_var (                   {create the variable}
    e, name, dtype, escr_string_var_len_k, global, sym_p, stat);
  if sys_error(stat) then return;
  if hval then begin                   {set new variable to the specified initial value ?}
    if not escr_get_val_dtype (e, sym_p^.var_val, stat) {get value and convert to var's data type}
      then goto err_missing;
    end;
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   SET name value
}
procedure escr_cmd_set (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor}
  name: string_var80_t;                {symbol name}

label
  err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then goto err_missing;

  escr_sym_find_curr (e, name, escr_sytype_vcon_k, sym_p);
  if sym_p = nil then begin            {no such symbol ?}
    escr_stat_sym_nfound (name, stat);
    return;
    end;
  if sym_p^.stype <> escr_sym_var_k then begin {symbol is not a variable}
    sys_stat_set (escr_subsys_k, escr_err_sym_nvar_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;

  if not escr_get_val_dtype (e, sym_p^.var_val, stat) {get value and convert to var's dtype}
    then goto err_missing;
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   APPEND name arg ... arg
*
*   Append the concatenation of the ARG arguments to the string in variable
*   NAME.
}
procedure escr_cmd_append (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor}
  name: string_var80_t;                {symbol name}
  str: string_var8192_t;               {the string to append}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  str.max := size_char(str.str);

  if not escr_get_token (e, name) then begin {get the variable name into NAME}
    escr_stat_cmd_noarg (e, stat);
    return;
    end;

  escr_sym_find_curr (e, name, escr_sytype_vcon_k, sym_p);
  if sym_p = nil then begin            {no such symbol ?}
    escr_stat_sym_nfound (name, stat);
    return;
    end;
  if sym_p^.stype <> escr_sym_var_k then begin {symbol is not a variable ?}
    sys_stat_set (escr_subsys_k, escr_err_sym_nvar_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;
  if sym_p^.var_val.dtype <> escr_dtype_str_k then begin {variable not a string ?}
    sys_stat_set (escr_subsys_k, escr_err_var_nstring_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;
{
*   SYM_P is pointing to the string variable to append to.
}
  escr_get_args_str (e, str, stat);    {get concatenation of remaining arguments}
  if sys_error(stat) then return;
  string_append (sym_p^.var_val.str, str); {do the append}
  end;
{
********************************************************************************
*
*   CONST name [dtype] = value
}
procedure escr_cmd_const (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: escr_sym_p_t;                 {pointer to constant symbol}
  p: string_index_t;                   {saved parse index}
  val: escr_val_t;                     {value of the new constant}
  len: sys_int_machine_t;              {extra length argument for data type}
  name: string_var80_t;                {constant name}
  tk: string_var80_t;                  {scratch token}

label
  err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);
{
*   Get the symbol name.
}
  if not escr_get_token (e, name)      {get the constant name into NAME}
    then goto err_missing;
{
*   Get the data type, if present, and read up to the "=".
}
  val.dtype := escr_dtype_str_k;       {init to default data type}
  p := e.parse_p^.ip;                  {save parse index before this token}
  if not escr_get_token (e, tk)        {get DTYPE or "=" token}
    then goto err_missing;
  if not string_equal (tk, string_v('=')) then begin {not "=", assume dtype ?}
    e.parse_p^.ip := p;                {restore parse index to before dtype token}
    if not escr_get_dtype (e, val.dtype, stat) {get the explicit data type}
      then goto err_missing;
    escr_get_keyword (e, '=', pick, stat); {parse the "=" token}
    if pick <= 0 then goto err_missing;
    end;
  escr_val_init (e, val.dtype, val);   {set up VAL for the indicated data type}
  if not escr_get_val_dtype (e, val, stat) {get the constant's value}
    then goto err_missing;

  len := 0;                            {init data type length parameter to arbitrary value}
  case val.dtype of                    {check for data types that use LEN}
escr_dtype_str_k: begin                {STRING}
      len := val.str.len;              {number of characters in the string}
      end;
    end;
  escr_sym_new_const (                 {create the new constant symbol}
    e, name, val.dtype, len, true, sym_p, stat);
  if sys_error(stat) then return;
  escr_val_copy (e, val, sym_p^.const_val, stat); {copy the value into the constant descriptor}
  if sys_error(stat) then return;
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   DEL name
*
*   Delete a version of the symbol NAME.  NAME may be a fully qualified symbol
*   name, indicating a particular symbol type and version.
}
procedure escr_cmd_del (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {symbol name}

label
  err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then goto err_missing;
  if not escr_get_end (e, stat) then return; {abort on extra parameter}
  escr_sym_del_name (e, name, stat);   {delete the symbol version}
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   SYLIST
*
*   List all user-defined symbols as comments to the output file.
}
procedure escr_cmd_sylist (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;
{
********************
*
*   Local subroutine WRITE_VAL (VAL)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Write a representation of the data value VAL into the output line buffer.
}
procedure write_val (                  {write data value to output line buffer}
  in      val: escr_val_t);            {the value}
  val_param; internal;

var
  tk: string_var32_t;                  {scratch token for number conversion}

begin
  tk.max := size_char(tk.str);         {init local var string}

  case val.dtype of                    {what is the data type ?}

escr_dtype_bool_k: begin               {BOOLEAN}
      string_appends (e.obuf, 'BOOL = '(0));
      if val.bool
        then string_appends (e.obuf, 'TRUE'(0))
        else string_appends (e.obuf, 'FALSE'(0));
      end;

escr_dtype_int_k: begin
      string_appends (e.obuf, 'INTEGER = '(0));
      string_f_int32h (tk, val.int);
      string_append (e.obuf, tk);
      string_append1 (e.obuf, 'h');
      end;

escr_dtype_fp_k: begin
      string_appends (e.obuf, 'REAL = '(0));
      escr_str_from_fp (e, val.fp, tk);
      string_append (e.obuf, tk);
      end;

escr_dtype_str_k: begin
      string_appends (e.obuf, 'STRING = "'(0));
      string_append (e.obuf, val.str);
      string_appends (e.obuf, '"'(0));
      end;

escr_dtype_time_k: begin
      string_appends (e.obuf, 'TIME = '(0));
      escr_str_from_time (e, val.time, tk);
      string_append (e.obuf, tk);
      end;

otherwise
    string_appends (e.obuf, '(data type '(0));
    string_f_int (tk, ord(val.dtype));
    string_append (e.obuf, tk);
    string_appends (e.obuf, ' unknown)'(0));
    end;                               {end of data type cases}
  end;
{
********************
*
*   Local subroutine WRITE_SYMBOL (SYM, STAT)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Write the information for symbol SYM.
}
procedure write_symbol (               {write info about all versions of a symbol}
  in      sym: escr_sym_t;             {the last version of the symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; internal;

var
  sym_p: escr_sym_p_t;                 {pointer to individual symbol descriptor}
  tk: string_var32_t;                  {scratch token for number conversion}

begin
  tk.max := size_char(tk.str);         {init local var string}

  sym_p := addr(sym);                  {init pointer to last version of symbol}
  while sym_p <> nil do begin          {once for each version of this symbol name}
    string_appends (e.obuf, '; '(0));
    string_append (e.obuf, sym_p^.name_p^); {symbol name}
    string_appends (e.obuf, ':'(0));
    string_f_int (tk, sym_p^.vern);    {symbol version number}
    string_append (e.obuf, tk);
    string_append1 (e.obuf, ' ');
    case sym_p^.stype of               {what is the symbol type ?}

escr_sym_var_k: begin                  {symbol is a variable}
        string_appends (e.obuf, ' variable '(0));
        write_val (sym_p^.var_val);
        end;

escr_sym_const_k: begin                {symbol is a constant}
        string_appends (e.obuf, ' constant '(0));
        write_val (sym_p^.const_val);
        end;

escr_sym_subr_k: begin                 {user-defined subroutine}
        string_appends (e.obuf, ' subroutine, line '(0));
        string_f_int (tk, sym_p^.subr_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.subr_line_p^.file_p^.tnam);
        end;

escr_sym_isubr_k: begin                {intrinsic subroutine}
        string_appends (e.obuf, ' intrinsic subroutine'(0));
        end;

escr_sym_macro_k: begin                {user-defined macro}
        string_appends (e.obuf, ' macro, line '(0));
        string_f_int (tk, sym_p^.macro_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.macro_line_p^.file_p^.tnam);
        end;

escr_sym_imacro_k: begin               {intrinsic macro}
        string_appends (e.obuf, ' intrinsic macro'(0));
        end;

escr_sym_func_k: begin                 {user-defined function}
        string_appends (e.obuf, ' function, line '(0));
        string_f_int (tk, sym_p^.func_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.func_line_p^.file_p^.tnam);
        end;

escr_sym_ifunc_k: begin                {intrinsic function}
        string_appends (e.obuf, ' intrinsic function'(0));
        end;

escr_sym_cmd_k: begin                  {user-defined command}
        string_appends (e.obuf, ' command, line '(0));
        string_f_int (tk, sym_p^.cmd_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.cmd_line_p^.file_p^.tnam);
        end;

escr_sym_icmd_k: begin                 {intrinsic command}
        string_appends (e.obuf, ' intrinsic command'(0));
        end;

escr_sym_label_k: begin                {label}
        string_appends (e.obuf, ' label, line '(0));
        string_f_int (tk, sym_p^.label_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.label_line_p^.file_p^.tnam);
        end;

escr_sym_src_k: begin                  {label for source code snippet}
        string_appends (e.obuf, ' source code block, lines ');
        string_append_intu (e.obuf, sym_p^.src_p^.lfirst_p^.lnum, 0);
        string_appends (e.obuf, ' to ');
        string_append_intu (e.obuf, sym_p^.src_p^.llast_p^.lnum, 0);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.src_p^.tnam);
        end;

otherwise
      string_appends (e.obuf, '(symbol type '(0));
      string_f_int (tk, ord(sym_p^.stype));
      string_append (e.obuf, tk);
      string_appends (e.obuf, ' unknown)'(0));
      end;                             {end of symbol type cases}
    escr_write_obuf (e, stat);         {write output line for this symbol}
    if sys_error(stat) then return;
    sym_p := sym_p^.prev_p;            {switch to previous version of this symbol}
    end;                               {back and show previous symbol version}
  end;
{
********************
*
*   Local subroutine DO_SYTABLE (TBL, STAT)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Show the data for all symbols in the symbol table TBL.
}
procedure do_sytable (                 {show symbols in specific symbol table}
  in      tbl: escr_sytable_t;         {the symbol table to show symbols of}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  scan: escr_sytable_scan_t;           {state for scanning symbol table}
  name_p: string_var_p_t;              {pointer to symbol name string}
  ent_p: escr_sytable_data_p_t;        {pointer to data about current symbol}

begin
  escr_sytable_scan_start (tbl, scan); {init for scanning symbol table}

  while true do begin                  {loop over the symbol table entries}
    escr_sytable_scan (scan, name_p, ent_p); {get this next symbol table entry}
    if ent_p = nil then exit;          {hit end of symbol table ?}
    write_symbol (ent_p^.last_p^, stat); {write info about current version}
    if sys_error(stat) then return;
    end;                               {back to process this new symbol table entry}
  end;
{
********************
*
*   Executable code for routine ESCR_CMD_SYLIST.
}
begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {abort on extra parameter}

  string_appends (e.obuf, ';'(0));     {leave blank comment line before list}
  escr_write_obuf (e, stat);
  if sys_error(stat) then return;

  do_sytable (e.sym_var, stat);
  if sys_error(stat) then return;
  do_sytable (e.sym_sub, stat);
  if sys_error(stat) then return;
  do_sytable (e.sym_mac, stat);
  if sys_error(stat) then return;
  do_sytable (e.sym_fun, stat);
  if sys_error(stat) then return;
  do_sytable (e.sym_cmd, stat);
  if sys_error(stat) then return;
  do_sytable (e.sym_lab, stat);
  end;
{
********************************************************************************
*
*   INCLUDE string
*
*   Switch to reading from the file indicated by STRING.  Reading at the current
*   input file will resume after the end of this new file is encountered.  The
*   pathname in string is relative to the directory containing the file that
*   contains the INCLUDE directive.
}
procedure escr_cmd_include (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {new input file name}
  olddir: string_treename_t;           {old current directory}
  newdir: string_treename_t;           {new current directory where source file is in}
  file_p: escr_infile_p_t;             {pointer to info about new file}
  stat2: sys_err_t;

label
  err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var strings}
  olddir.max := size_char(olddir.str);
  newdir.max := size_char(newdir.str);

  if not escr_get_str (e, fnam, stat)  {get new file name into FNAM}
    then goto err_missing;

  if not escr_get_end (e, stat) then return; {abort on extra parameter}

  string_pathname_split (              {get source file's directory in NEWDIR}
    e.exblock_p^.inpos_p^.last_p^.file_p^.tnam, newdir, olddir);
  file_currdir_get (olddir, stat);     {get and save current directory name}
  if sys_error(stat) then return;
  file_currdir_set (newdir, stat);     {go to source file's directory for include file open}
  if sys_error(stat) then return;
  escr_infile_open (e, fnam, e.incsuff, file_p, stat); {get pointer to info about the new file}
  file_currdir_set (olddir, stat2);
  if sys_error(stat) then return;
  sys_error_abort (stat2, '', '', nil, 0);
  escr_exblock_inline_push (           {set next line to first line in new file}
    e, file_p^.lfirst_p, stat);
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   WRITE arg ... arg
*
*   Write the concatenation of the string representation of all the arguments
*   as one line to the output file.
}
procedure escr_cmd_write (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  escr_get_args_str (e, e.obuf, stat); {get all arguments concatenated as strings}
  if sys_error(stat) then return;
  escr_write_obuf (e, stat);
  end;
{
********************************************************************************
*
*   SHOW arg ... arg
*
*   Just like command WRITE, except that the line is written to standard output
*   instead of the output file.
}
procedure escr_cmd_show (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  escr_get_args_str (e, e.obuf, stat); {get all arguments concatenated as strings}
  if sys_error(stat) then return;
  escr_show_obuf (e);
  end;
{
********************************************************************************
*
*   WRITEPUSH fnam
}
procedure escr_cmd_writepush (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {name of new file to write to}

label
  err_missing;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var string}

  if not escr_get_str (e, fnam, stat)  {get new file name into FNAM}
    then goto err_missing;
  if not escr_get_end (e, stat) then return; {abort on extra parameter}

  escr_out_open (e, fnam, stat);       {save curr state, open new file}
  return;
{
*   Abort due to missing required parameter.
}
err_missing:
  escr_stat_cmd_noarg (e, stat);
  end;
{
********************************************************************************
*
*   WRITEPOP
}
procedure escr_cmd_writepop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {abort on extra parameter}

  if e.out_p = nil then begin          {no current output file at all ?}
    sys_stat_set (escr_subsys_k, escr_err_noutcl_k, stat);
    return;
    end;

  if                                   {trying to close preprocessor output file}
      (e.out_p^.prev_p = nil) and      {this is the top level output file ?}
      (escr_flag_preproc_k in e.flags) {in preprocessor mode ?}
      then begin
    sys_stat_set (escr_subsys_k, escr_err_topoutcl_k, stat);
    return;
    end;

  escr_out_close (e, false);           {close current output file, pop back to previous}
  end;
{
********************************************************************************
*
*   STOP [exstat]
}
procedure escr_cmd_stop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  ii: sys_int_max_t;
  sym_p: escr_sym_p_t;                 {pointer to variable}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if escr_get_int (e, ii, stat)
    then begin                         {exit status code set explicitly on command line}
      if not escr_get_end (e, stat) then return; {abort on extra parameters}
      e.exstat := ii;                  {set the overall script exit status code}
      end
    else begin                         {no exit status code on command line}
      if sys_error(stat) then return;  {hard error on getting command line argument ?}
      escr_sym_find_curr (             {try to find exit status variable}
        e,
        string_v('EXITSTATUS'(0)),     {name of symbol to look up}
        escr_sytype_var_k,             {looking only for a variable}
        sym_p);                        {returned pointer to symbol if found}
      if                               {get exit status from the variable ?}
          (sym_p <> nil) and then      {variable of the right name exists ?}
          (sym_p^.var_val.dtype = escr_dtype_int_k) {variable is integer ?}
          then begin
        e.exstat := sym_p^.var_val.int; {set the overall script exit status code}
        end;
      end
    ;

  while e.exblock_p^.prev_p <> nil do begin {loop until only top block left}
    escr_exblock_close (e, stat);
    if sys_error(stat) then return;
    end;

  while e.exblock_p^.inpos_p^.prev_p <> nil do begin {back to top file}
    escr_infile_pop (e);
    end;

  e.exblock_p^.inpos_p^.line_p := nil; {as if hit end of input file}
  end;
{
********************************************************************************
*
*   RUN arg ... arg
}
procedure escr_cmd_run (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  cmline: string_var8192_t;            {command line to execute}
  tf: boolean;                         {true/false returned by the program}
  exstat: sys_sys_exstat_t;            {program exist status code}
  sym_p: escr_sym_p_t;                 {pointer to variable to update}
  name: string_var32_t;                {symbol name}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  cmline.max := size_char(cmline.str); {init local var strings}
  name.max := size_char(name.str);
{
*   Get the command line to run and run it.
}
  escr_get_args_str (e, cmline, stat); {get all arguments concatenated as strings}
  if sys_error(stat) then return;
  if cmline.len <= 0 then begin
    escr_stat_cmd_noarg (e, stat);
    return;
    end;

  sys_run_wait_stdsame (               {run the command line}
    cmline,                            {command line to run}
    tf,                                {true/false returned by the program}
    exstat,                            {program exit status code}
    stat);
  if sys_error(stat) then return;
{
*   Save the program's exit status code in the integer variable EXITSTATUS.
*   This variable is created if it does not exist.
}
  string_vstring (name, 'EXITSTATUS'(0), -1); {set variable name}

  escr_sym_find_curr (                 {find EXITSTATUS variable}
    e,                                 {ESCR library use state}
    name,                              {NAME of symbol to find}
    escr_sytype_var_k,                 {symbol must be a variable}
    sym_p);                            {returned pointer to the symbol}

  if                                   {need to create the variable ?}
      (sym_p = nil) or else            {variable doesn't exist at all ?}
      (sym_p^.var_val.dtype <> escr_dtype_int_k) {variable is not integer ?}
      then begin
    escr_sym_new_var (                 {create the variable}
      e,
      name,                            {variable name}
      escr_dtype_int_k,                {integer}
      0,                               {length, unused}
      true,                            {global}
      sym_p,                           {returned pointer to new variable}
      stat);
    if sys_error(stat) then return;
    end;

  sym_p^.var_val.int := exstat;        {set to the program's exist status code}
  end;
