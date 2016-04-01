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
define escr_cmd_del;
define escr_cmd_sylist;
define escr_cmd_include;
define escr_cmd_write;
define escr_cmd_show;
define escr_cmd_writeto;
define escr_cmd_writeend;
define escr_cmd_stop;
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
  done_cmdline, make_new;

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
     pick);
  case pick of
1:  scmd := scmd_new_k;                {NEW}
2:  scmd := scmd_exist_k;              {EXISTS}
3:  begin                              {LOCAL}
      if not e.exblock_p^.iter1 then begin {not first iteration of a loop ?}
        e.ip := e.ibuf.len + 1;        {indicate whole input line used}
        return;
        end;
      scmd := scmd_new_k;
      global := false;
      end;
otherwise
    escr_err_parm_last_bad (e);
    end;

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then escr_err_parm_missing (e, '', '', nil, 0);

  dtype := escr_dtype_str_k;           {init to default data type}
  hval := false;                       {init to no initial value specified}
  p := e.ip;                           {save parse index before getting next token}
  if escr_get_token (e, tk) then begin {some token exists after NAME ?}
    if                                 {not the "=" keyword, must be data type ?}
        (tk.len <> 1) or
        (tk.str[1] <> '=')
        then begin
      e.ip := p;                       {restore parse position to before token}
      discard( escr_get_dtype (e, dtype) ); {get the optional data type}
      if not escr_get_token (e, tk) then goto done_cmdline; {hit end of command line ?}
      if (tk.len <> 1) or (tk.str[1] <> '=') {not "=" keyword as expected ?}
        then escr_err_parm_bad (e, tk);
      end;
    hval := true;                      {indicate initial value parameter still left}
    end;
done_cmdline:                          {done reading command line except initial value}
{
*   Don't do anything if the subcommand is EXIST and a symbol with the
*   proper parameters already exists.
}
  if not (scmd = scmd_exist_k) then goto make_new; {not asked to re-use existing ?}
  escr_sym_find (e, name, e.sym_var, sym_p); {look for existing symbol of this name}
  if sym_p = nil then goto make_new;   {no such symbol ?}
  if sym_p^.stype <> escr_sym_var_k then goto make_new; {symbol isn't a variable ?}
  if sym_p^.var_val.dtype <> dtype then goto make_new; {symbol isn't right dtype ?}

  if hval then begin                   {still need to read initial value parameter ?}
    val.dtype := dtype;                {indicate data type initial value must have}
    if not escr_get_val_dtype (e, val) {read initial value only to verify it}
      then escr_err_parm_missing (e, '', '', nil, 0);
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
    if not escr_get_val_dtype (e, sym_p^.var_val) {get value and convert to var's data type}
      then escr_err_parm_missing (e, '', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   /SET name value
}
procedure escr_cmd_set (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  sym_p: escr_sym_p_t;                 {pointer to symbol descriptor}
  name: string_var80_t;                {symbol name}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then escr_err_parm_missing (e, '', '', nil, 0);

  escr_sym_find (e, name, e.sym_var, sym_p); {get pointer to the symbol descriptor}
  if sym_p = nil then begin            {no such symbol ?}
    sys_msg_parm_vstr (msg_parm[1], name);
    escr_err_atline (e, 'pic', 'sym_not_found', msg_parm, 1);
    end;
  if sym_p^.stype <> escr_sym_var_k then begin {symbol is not a variable}
    sys_msg_parm_vstr (msg_parm[1], name);
    escr_err_atline (e, 'pic', 'sym_not_variable', msg_parm, 1);
    end;

  if not escr_get_val_dtype (e, sym_p^.var_val) {get value and convert to var's dtype}
    then escr_err_parm_missing (e, '', '', nil, 0);
  end;
{
********************************************************************************
*
*   /CONST name [dtype] = value
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

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);
{
*   Get the symbol name.
}
  if not escr_get_token (e, name)      {get the constant name into NAME}
    then escr_err_parm_missing (e, '', '', nil, 0);
{
*   Get the data type, if present, and read up to the "=".
}
  val.dtype := escr_dtype_str_k;       {init to default data type}
  p := e.ip;                           {save parse index before this token}
  if not escr_get_token (e, tk)        {get DTYPE or "=" token}
    then escr_err_parm_missing (e, '', '', nil, 0);
  if not string_equal (tk, string_v('=')) then begin {not "=", assume dtype ?}
    e.ip := p;                         {restore parse index to before dtype token}
    if not escr_get_dtype (e, val.dtype) {get the explicit data type}
      then escr_err_parm_missing (e, '', '', nil, 0);
    escr_get_keyword (e, '=', pick);   {parse the "=" token}
    if pick = 0 then escr_err_parm_missing (e, '', '', nil, 0);
    end;
  escr_val_init (e, val.dtype, val);   {set up VAL for the indicated data type}
  if not escr_get_val_dtype (e, val)   {get the constant's value}
    then escr_err_parm_missing (e, '', '', nil, 0);

  len := 0;                            {init data type length parameter to arbitrary value}
  case val.dtype of                    {check for data types that use LEN}
escr_dtype_str_k: begin                {STRING}
      len := val.str.len;              {number of characters in the string}
      end;
    end;
  escr_sym_new_const (                 {create the new constant symbol}
    e, name, val.dtype, len, true, sym_p, stat);
  if sys_error(stat) then return;
  escr_val_copy (e, val, sym_p^.const_val); {copy the value into the constant descriptor}
  end;
{
********************************************************************************
*
*   /DEL name
}
procedure escr_cmd_del (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {symbol name}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not escr_get_token (e, name)      {get the variable name into NAME}
    then escr_err_parm_missing (e, '', '', nil, 0);
  escr_get_end (e);                    {no more parameters allowed}

  escr_sym_del_name (e, e.sym_var, name, stat); {delete the symbol}
  end;
{
********************************************************************************
*
*   /SYLIST
*
*   List all symbols as comments to the output file.
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
*   Local subroutine WRITE_SYMBOL (SYM)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Write the information for symbol SYM.
}
procedure write_symbol (               {write info about all versions of a symbol}
  in      sym: escr_sym_t);            {the current version of the symbol}
  val_param; internal;

var
  sym_p: escr_sym_p_t;                 {pointer to individual symbol descriptor}
  tk: string_var32_t;                  {scratch token for number conversion}

begin
  tk.max := size_char(tk.str);         {init local var string}

  sym_p := addr(sym);                  {init pointer to current version of symbol}
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

escr_sym_subr_k: begin                 {symbol is a subroutine name}
        string_appends (e.obuf, ' subroutine, line '(0));
        string_f_int (tk, sym_p^.subr_line_p^.lnum);
        string_append (e.obuf, tk);
        string_appends (e.obuf, ' of '(0));
        string_append (e.obuf, sym_p^.subr_line_p^.file_p^.tnam);
        end;

otherwise
      string_appends (e.obuf, '(symbol type '(0));
      string_f_int (tk, ord(sym_p^.stype));
      string_append (e.obuf, tk);
      string_appends (e.obuf, ' unknown)'(0));
      end;                             {end of symbol type cases}
    escr_write_obuf (e);               {write output line for this symbol}
    sym_p := sym_p^.prev_p;            {switch to previous version of this symbol}
    end;                               {back and show previous symbol version}
  end;
{
********************
*
*   Local subroutine DO_SYTABLE (TBL)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Show the data for all symbols in the symbol table TBL.
}
procedure do_sytable (                 {show symbols in specific symbol table}
  in      tbl: escr_sytable_t);        {the symbol table to show symbols of}
  val_param;

var
  pos: string_hash_pos_t;              {symbol table position handle}
  found: boolean;                      {symbol table entry found}
  name_p: string_var_p_t;              {pointer to symbol name string}
  sym_pp: escr_sym_pp_t;               {pointer to data in symbol table entry}
  sym_p: escr_sym_p_t;                 {pointer to individual symbol descriptor}

begin
  string_hash_pos_first (tbl.hash, pos, found);

  while found do begin                 {once for each symbol in the symbol table}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get info from this table entry}
    sym_p := sym_pp^;                  {get pointer to current symbol}
    write_symbol (sym_p^);             {write information about this symbol}
    string_hash_pos_next (pos, found); {advance to next symbol table entry}
    end;                               {back to process this new symbol table entry}
  end;
{
********************
*
*   Executable code for routine ESCR_CMD_SYLIST.
}
begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  escr_get_end (e);                    {no command parameters allowed}

  string_appends (e.obuf, ';'(0));     {leave blank comment line before list}
  escr_write_obuf (e);

  do_sytable (e.sym_var);
  do_sytable (e.sym_sub);
  do_sytable (e.sym_mac);
  do_sytable (e.sym_fun);
  do_sytable (e.sym_cmd);
  end;
{
********************************************************************************
*
*   /INCLUDE string
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

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var strings}
  olddir.max := size_char(olddir.str);
  newdir.max := size_char(newdir.str);

  if not escr_get_str (e, fnam)        {get new file name into FNAM}
    then escr_err_parm_missing (e, '', '', nil, 0);
  escr_get_end (e);                    {no more parameters allowed}

  string_pathname_split (              {get source file's directory in NEWDIR}
    e.exblock_p^.inpos_p^.last_p^.file_p^.tnam, newdir, olddir);
  file_currdir_get (olddir, stat);     {get and save current directory name}
  if sys_error(stat) then return;
  file_currdir_set (newdir, stat);     {go to source file's directory for include file open}
  if sys_error(stat) then return;
  escr_infile_open (e, fnam, file_p, stat); {get pointer to info about the new file}
  file_currdir_set (olddir, stat2);
  if sys_error(stat) then return;
  sys_error_abort (stat2, '', '', nil, 0);
  escr_exblock_inline_push (e, file_p^.lines_p); {set next line to first line in new file}

  string_appends (e.obuf, ';--Start of file "'(0));
  string_append (e.obuf, file_p^.tnam);
  string_appends (e.obuf, '"'(0));
  escr_write_obuf (e);
  end;
{
********************************************************************************
*
*   /WRITE arg ... arg
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
  escr_get_args_str (e, e.obuf);       {get all arguments concatenated as strings}
  escr_write_obuf (e);
  end;
{
********************************************************************************
*
*   /SHOW arg ... arg
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
  escr_get_args_str (e, e.obuf);       {get all arguments concatenated as strings}
  escr_show_obuf (e);
  end;
{
********************************************************************************
*
*   /WRITETO fnam
}
procedure escr_cmd_writeto (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {name of new file to write to}
  o_p: escr_outfile_p_t;               {pointer to new output file descriptor}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var string}

  if not escr_get_str (e, fnam)        {get new file name into FNAM}
    then escr_err_parm_missing (e, '', '', nil, 0);
  escr_get_end (e);                    {no more parameters allowed}

  sys_mem_alloc (sizeof(o_p^), o_p);   {allocate new output file descriptor}
  file_open_write_text (fnam, '', o_p^.conn, stat);
  if sys_error(stat) then begin        {error opening file ?}
    sys_mem_dealloc (o_p);
    return;
    end;

  o_p^.prev_p := e.out_p;              {save pointer to old output file}
  e.out_p := o_p;                      {switch to new output file}
  end;
{
********************************************************************************
*
*   /WRITEND
}
procedure escr_cmd_writeend (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  escr_get_end (e);                    {no more parameters allowed}

  if e.out_p = nil then return;        {no current output file at all ?}
  if e.out_p^.prev_p = nil then return; {don't close the original output file}
  escr_close_out (e, false);           {close current output file, pop back to previous}
  end;
{
********************************************************************************
*
*   /STOP
}
procedure escr_cmd_stop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  escr_get_end (e);                    {no more parameters allowed}

  while e.exblock_p^.prev_p <> nil do begin {loop until only top block left}
    escr_exblock_close (e);
    end;

  while e.exblock_p^.inpos_p^.prev_p <> nil do begin {back to top file}
    escr_infile_pop (e);
    end;

  e.exblock_p^.inpos_p^.line_p := nil; {as if hit end of input file}
  end;
