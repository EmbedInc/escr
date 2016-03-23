{   Module of routines that implement individual preprocessor commands.
*   Some larger command routines are in their own modules, called
*   ESCR_CMD_<name>.  The smaller command routines are collected here.
*
*   All the command subroutines have a common interface:
*
*     escr_cmd_<name> (STAT)
*
*   The input line is in IBUF and IP is the parse index after the command
*   name.  The upper case command name is in CMD.  The program will abort
*   and indicate the current source line if STAT is returned other than
*   normal status.  An error is also automatically generated if the input
*   line is not exhausted.  STAT is initialized to normal status before
*   the command routine is called.
*
*   The global variable OBUF can be used to write lines to the output
*   file.  Data is written to OBUF directly, which is then written to
*   the output file using subroutine WRITE_OBUF.  OBUF is automatically
*   reset to empty when its contents is written.  OBUF starts empty
*   when a command routine is called.  If anything is left in OBUF
*   when the command routine returns, it will be written to the
*   output file.
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
%include '/cognivision_links/dsee_libs/pic/escr.ins.pas';
{
********************************************************************************
*
*   /VAR (NEW, EXIST, LOCAL) name [dtype] [= value]
}
procedure escr_cmd_var (
  out     stat: sys_err_t);
  val_param;

type
  scmd_k_t = (                         {subcommand ID}
    scmd_new_k,                        {NEW}
    scmd_exist_k);                     {EXIST}

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: sym_p_t;                      {pointer to variable symbol}
  scmd: scmd_k_t;                      {subcommand ID}
  dtype: dtype_k_t;                    {variable's data type}
  name: string_var80_t;                {variable name}
  tk: string_var80_t;                  {scratch token}
  p: string_index_t;                   {string parse index}
  val: val_t;                          {scratch variable value descriptor}
  global: boolean;                     {create global, not local symbol}
  hval: boolean;                       {initial value was specified}

label
  done_cmdline, make_new;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);
{
*   Read and validate the command parameters.
}
  global := true;                      {init to create global symbol}
  get_keyword ('NEW EXIST LOCAL', pick); {get subcommand choice and set SCMD}
  case pick of
1:  scmd := scmd_new_k;                {NEW}
2:  scmd := scmd_exist_k;              {EXISTS}
3:  begin                              {LOCAL}
      if not exblock_p^.iter1 then begin {not first iteration of a loop ?}
        ip := ibuf.len + 1;            {indicate whole input line used}
        return;
        end;
      scmd := scmd_new_k;
      global := false;
      end;
otherwise
    err_parm_last_bad;
    end;

  if not get_token (name)              {get the variable name into NAME}
    then err_parm_missing ('', '', nil, 0);

  dtype := dtype_str_k;                {init to default data type}
  hval := false;                       {init to no initial value specified}
  p := ip;                             {save parse index before getting next token}
  if get_token (tk) then begin         {some token exists after NAME ?}
    if                                 {not the "=" keyword, must be data type ?}
        (tk.len <> 1) or
        (tk.str[1] <> '=')
        then begin
      ip := p;                         {restore parse position to before token}
      discard( get_dtype (dtype) );    {get the optional data type}
      if not get_token (tk) then goto done_cmdline; {hit end of command line ?}
      if (tk.len <> 1) or (tk.str[1] <> '=') {not "=" keyword as expected ?}
        then err_parm_bad (tk);
      end;
    hval := true;                      {indicate initial value parameter still left}
    end;
done_cmdline:                          {done reading command line except initial value}
{
*   Don't do anything if the subcommand is EXIST and a symbol with the
*   proper parameters already exists.
}
  if not (scmd = scmd_exist_k) then goto make_new; {not asked to re-use existing ?}

  sym_find (name, sym_p);              {look for existing symbol of this name}
  if sym_p = nil then goto make_new;   {no such symbol ?}
  if sym_p^.stype <> sym_var_k then goto make_new; {symbol isn't a variable ?}
  if sym_p^.var_val.dtype <> dtype then goto make_new; {symbol isn't right dtype ?}
  if hval then begin                   {still need to read initial value parameter ?}
    val.dtype := dtype;                {indicate data type initial value must have}
    if not get_val_dtype (val)         {read initial value only to verify it}
      then err_parm_missing ('', '', nil, 0);
    end;
  return;                              {use the existing symbol}
{
*   Create a new symbol according to the specifications.
}
make_new:                              {a new symbol needs to be created}
  sym_new_var (name, dtype, string_var_len_k, global, sym_p); {create the variable}
  if hval then begin                   {set new variable to the specified initial value ?}
    if not get_val_dtype (sym_p^.var_val) {get value and convert to var's data type}
      then err_parm_missing ('', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   /SET name value
}
procedure escr_cmd_set (
  out     stat: sys_err_t);
  val_param;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  sym_p: sym_p_t;                      {pointer to symbol descriptor}
  name: string_var80_t;                {symbol name}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not get_token (name)              {get the variable name into NAME}
    then err_parm_missing ('', '', nil, 0);

  sym_find (name, sym_p);              {get pointer to the symbol descriptor}
  if sym_p = nil then begin            {no such symbol ?}
    sys_msg_parm_vstr (msg_parm[1], name);
    err_atline ('pic', 'sym_not_found', msg_parm, 1);
    end;
  if sym_p^.stype <> sym_var_k then begin {symbol is not a variable}
    sys_msg_parm_vstr (msg_parm[1], name);
    err_atline ('pic', 'sym_not_variable', msg_parm, 1);
    end;

  if not get_val_dtype (sym_p^.var_val) {get value and convert to var's dtype}
    then err_parm_missing ('', '', nil, 0);
  end;
{
********************************************************************************
*
*   /CONST name [dtype] = value
}
procedure escr_cmd_const (
  out     stat: sys_err_t);
  val_param;

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: sym_p_t;                      {pointer to constant symbol}
  p: string_index_t;                   {saved parse index}
  val: val_t;                          {value of the new constant}
  len: sys_int_machine_t;              {extra length argument for data type}
  name: string_var80_t;                {constant name}
  tk: string_var80_t;                  {scratch token}

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);
{
*   Get the symbol name.
}
  if not get_token (name)              {get the constant name into NAME}
    then err_parm_missing ('', '', nil, 0);
{
*   Get the data type, if present, and read up to the "=".
}
  val.dtype := dtype_str_k;            {init to default data type}
  p := ip;                             {save parse index before this token}
  if not get_token (tk)                {get DTYPE or "=" token}
    then err_parm_missing ('', '', nil, 0);
  if not string_equal (tk, string_v('=')) then begin {not "=", assume dtype ?}
    ip := p;                           {restore parse index to before dtype token}
    if not get_dtype (val.dtype)       {get the explicit data type}
      then err_parm_missing ('', '', nil, 0);
    get_keyword ('=', pick);           {parse the "=" token}
    if pick = 0 then err_parm_missing ('', '', nil, 0);
    end;
  val_init (val.dtype, val);           {set up VAL for the indicated data type}
  if not get_val_dtype (val)           {get the constant's value}
    then err_parm_missing ('', '', nil, 0);

  len := 0;                            {init data type length parameter to arbitrary value}
  case val.dtype of                    {check for data types that use LEN}
dtype_str_k: begin                     {STRING}
      len := val.str.len;              {number of characters in the string}
      end;
    end;
  sym_new_const (name, val.dtype, len, true, sym_p); {create the new constant symbol}
  val_copy (val, sym_p^.const_val);    {copy the value into the constant descriptor}
  end;
{
********************************************************************************
*
*   /DEL name
}
procedure escr_cmd_del (
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {symbol name}

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var string}

  if not get_token (name)              {get the variable name into NAME}
    then err_parm_missing ('', '', nil, 0);
  get_end;                             {no more parameters allowed}

  sym_del_name (name);                 {delete the symbol}
  end;
{
********************************************************************************
*
*   /SYLIST
*
*   List all symbols as comments to the output file.
}
procedure escr_cmd_sylist (
  out     stat: sys_err_t);
  val_param;

var
  pos: string_hash_pos_t;              {symbol table position handle}
  nsym: sys_int_machine_t;             {number of symbols found}
  sym_pp: sym_pp_t;                    {pointer to data in symbol table entry}
  sym_p: sym_p_t;                      {pointer to individual symbol descriptor}
  name_p: string_var_p_t;              {pointer to symbol name string}
  tk: string_var32_t;                  {scratch token for number conversion}
  found: boolean;                      {symbol table entry found}
{
********************
*
*   Local subroutine WRITE_VAL (VAL)
*   This subroutine is local to ESCR_CMD_SYLIST.
*
*   Write a representation of the data value VAL into the output line buffer.
}
procedure write_val (                  {write data value to output line buffer}
  in      val: val_t);                 {the value}
  val_param; internal;

var
  tk: string_var32_t;                  {scratch token for number conversion}

begin
  tk.max := size_char(tk.str);         {init local var string}

  case val.dtype of                    {what is the data type ?}

dtype_bool_k: begin                    {BOOLEAN}
      string_appends (obuf, 'BOOL = '(0));
      if val.bool
        then string_appends (obuf, 'TRUE'(0))
        else string_appends (obuf, 'FALSE'(0));
      end;

dtype_int_k: begin
      string_appends (obuf, 'INTEGER = '(0));
      string_f_int32h (tk, val.int);
      case lang of
lang_aspic_k: begin
          string_appends (obuf, 'h'''(0));
          string_append (obuf, tk);
          string_appends (obuf, ''''(0));
          end;
lang_dspic_k: begin
          string_appends (obuf, '0x'(0));
          string_append (obuf, tk);
          end;
otherwise
        err_lang (lang, 'ESCR_CMD', 1);
        end;                           {end of language type cases}
      end;                             {end of data type is integer case}

dtype_fp_k: begin
      string_appends (obuf, 'REAL = '(0));
      str_from_fp (val.fp, tk);
      string_append (obuf, tk);
      end;

dtype_str_k: begin
      string_appends (obuf, 'STRING = "'(0));
      string_append (obuf, val.str);
      string_appends (obuf, '"'(0));
      end;

dtype_time_k: begin
      string_appends (obuf, 'TIME = '(0));
      str_from_time (val.time, tk);
      string_append (obuf, tk);
      end;

otherwise
    string_appends (obuf, '(data type '(0));
    string_f_int (tk, ord(val.dtype));
    string_append (obuf, tk);
    string_appends (obuf, ' unknown)'(0));
    end;                               {end of data type cases}
  end;
{
********************
*
*   Executable code for routine ESCR_CMD_SYLIST.
}
begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  tk.max := size_char(tk.str);         {init local var string}

  get_end;                             {no command parameters allowed}

  string_appends (obuf, ';'(0));       {leave blank comment line before list}
  write_obuf;

  nsym := 0;                           {init number of symbols listed}
  string_hash_pos_first (sym, pos, found);

  while found do begin                 {once for each symbol in the symbol table}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get info from this table entry}
    sym_p := sym_pp^;                  {get pointer to current symbol}
    while sym_p <> nil do begin        {once for each version of this symbol name}
      string_appends (obuf, '; '(0));
      string_append (obuf, sym_p^.name_p^); {symbol name}
      string_appends (obuf, ':'(0));
      string_f_int (tk, sym_p^.vern);  {symbol version number}
      string_append (obuf, tk);
      string_appends (obuf, ' ');
      case sym_p^.stype of             {what is the symbol type ?}
sym_var_k: begin                       {symbol is a variable}
  string_appends (obuf, ' variable '(0));
  write_val (sym_p^.var_val);
  end;
sym_const_k: begin                     {symbol is a constant}
  string_appends (obuf, ' constant '(0));
  write_val (sym_p^.const_val);
  end;
sym_subr_k: begin                      {symbol is a subroutine name}
  string_appends (obuf, ' subroutine, line '(0));
  string_f_int (tk, sym_p^.subr_line_p^.lnum);
  string_append (obuf, tk);
  string_appends (obuf, ' of '(0));
  string_append (obuf, sym_p^.subr_line_p^.file_p^.tnam);
  end;
otherwise
        string_appends (obuf, '(symbol type '(0));
        string_f_int (tk, ord(sym_p^.stype));
        string_append (obuf, tk);
        string_appends (obuf, ' unknown)'(0));
        end;                           {end of symbol type cases}
      write_obuf;                      {write output line for this symbol}
      sym_p := sym_p^.prev_p;          {switch to previous version of this symbol}
      end;                             {back and show previous symbol version}
    string_hash_pos_next (pos, found); {advance to next symbol table entry}
    end;                               {back to process this new symbol table entry}
  end;
{
********************************************************************************
*
*   /INCLUDE string
*
*   Switch to reading from the file indicated by STRING.  Reading at the current
*   input file will resume after the end of this new file is encountered.
*   The pathname in string is relative to the directory containing the file
*   that contains the INCLUDE directive.
}
procedure escr_cmd_include (
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {new input file name}
  olddir: string_treename_t;           {old current directory}
  newdir: string_treename_t;           {new current directory where source file is in}
  file_p: infile_p_t;                  {pointer to info about new file}
  stat2: sys_err_t;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var strings}
  olddir.max := size_char(olddir.str);
  newdir.max := size_char(newdir.str);

  if not get_str (fnam)                {get new file name into FNAM}
    then err_parm_missing ('', '', nil, 0);
  get_end;                             {no more parameters allowed}

  string_pathname_split (              {get source file's directory in NEWDIR}
    exblock_p^.inpos_p^.last_p^.file_p^.tnam, newdir, olddir);
  file_currdir_get (olddir, stat);     {get and save current directory name}
  if sys_error(stat) then return;
  file_currdir_set (newdir, stat);     {go to source file's directory for include file open}
  if sys_error(stat) then return;
  infile_open (fnam, file_p, stat);    {get pointer to info about the new file}
  file_currdir_set (olddir, stat2);
  if sys_error(stat) then return;
  sys_error_abort (stat2, '', '', nil, 0);
  exblock_inline_push (file_p^.lines_p); {set next line to first line in new file}

  string_appends (obuf, ';--Start of file "'(0));
  string_append (obuf, file_p^.tnam);
  string_appends (obuf, '"'(0));
  write_obuf;
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
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  get_args_str (obuf);                 {get all arguments concatenated as strings}
  write_obuf;
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
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  get_args_str (obuf);                 {get all arguments concatenated as strings}
  show_obuf;
  end;
{
********************************************************************************
*
*   /WRITETO fnam
}
procedure escr_cmd_writeto (
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {name of new file to write to}
  o_p: outfile_p_t;                    {pointer to new output file descriptor}

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  fnam.max := size_char(fnam.str);     {init local var string}

  if not get_str (fnam)                {get new file name into FNAM}
    then err_parm_missing ('', '', nil, 0);
  get_end;                             {no more parameters allowed}

  sys_mem_alloc (sizeof(o_p^), o_p);   {allocate new output file descriptor}
  file_open_write_text (fnam, '', o_p^.conn, stat);
  if sys_error(stat) then begin        {error opening file ?}
    sys_mem_dealloc (o_p);
    return;
    end;

  o_p^.prev_p := out_p;                {save pointer to old output file}
  out_p := o_p;                        {switch to new output file}
  end;
{
********************************************************************************
*
*   /WRITEND
}
procedure escr_cmd_writeend (
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}

  get_end;                             {no more parameters allowed}

  if out_p = nil then return;          {no current output file at all ?}
  if out_p^.prev_p = nil then return;  {don't close the original output file}
  close_out (false);                   {close current output file, pop back to previous}
  end;
