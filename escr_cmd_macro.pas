{   Commands relating to preprocessor macros.  These are defined much like
*   preprocessor subroutines but are envoked just like assembler macros.
}
module prepic_cmd_macro;
define prepic_cmd_macro;
define prepic_cmd_endmac;
define prepic_cmd_quitmac;
define macro_run;
%include 'prepic.ins.pas';
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_MACRO (STAT)
*
*   /MACRO name
*
*   Starts the definition of a macro.  The macro definition is stored but not
*   executed now.
}
procedure prepic_cmd_macro (
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {macro name}
  sz: sys_int_adr_t;                   {size of new descriptor}
  sym_p: sym_p_t;                      {pointer to new name symbol}

begin
  name.max := size_char(name.str);     {init local var string}

  inh_new;                             {create new execution inhibit layer}
  inhibit_p^.inhty := inhty_blkdef_k;  {inhibit it due to reading block definition}
  inhibit_p^.blkdef_type := exblock_mac_k; {block type is macro}
  if inhibit_p^.inh then return;       {previously inhibited, don't define macro}
  inhibit_p^.inh := true;              {inhibit execution during macro definition}

  if not get_token (name)              {get macro name}
    then err_parm_missing ('', '', nil, 0);
  err_check_symname (name);            {check for valid symbol name}

  sz :=                                {make size of whole macro symbol}
    offset(sym_t.macro_line_p) + size_min(sym_t.macro_line_p);
  sym_new (name, sz, false, sym_p);    {create new symbol for macro name}
  sym_p^.stype := sym_macro_k;         {this symbol is a macro name}
  sym_p^.macro_line_p :=               {save pointer to macro definition line}
    exblock_p^.inpos_p^.last_p;
  end;
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_ENDMAC (STAT)
*
*   /ENDMAC
*
*   End the current macro definition.
}
procedure prepic_cmd_endmac (
  out     stat: sys_err_t);
  val_param;

begin
  if not inhibit_p^.inh then begin     {executing code normally ?}
    prepic_cmd_quitmac (stat);         {acts just like QUITMAC}
    return;
    end;

  if                                   {not defining a macro ?}
      (inhibit_p^.inhty <> inhty_blkdef_k) or {not in a block definition ?}
      (inhibit_p^.blkdef_type <> exblock_mac_k) {block is not a macro ?}
      then begin
    err_atline ('pic', 'not_in_macdef', nil, 0);
    end;

  inh_end;                             {end excution inhibit due to macro def}
  end;
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_QUITMAC (STAT)
*
*   /QUITMAC
*
*   Stop executing the innermost macro currently in.
}
procedure prepic_cmd_quitmac (
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}

  while exblock_p^.bltype <> exblock_mac_k do begin {up thru blocks until first macro}
    if exblock_p^.prev_p = nil then begin {at top execution block ?}
      err_atline ('pic', 'not_in_macro', nil, 0); {complain not in a macro}
      end;
    exblock_close;                     {end this execution block, make previous current}
    end;                               {back to check this new execution block}

  exblock_close;                       {end the macro execution block}
  end;
{
********************************************************************************
*
*   Function MACRO_RUN (STAT)
*
*   Run the macro if one is envoked from the current input line.  The input line
*   is in IBUF.  If there is no macro invocation on this line, then nothing is
*   done and this routine returns FALSE.  If a macro invocation is found, the
*   macro is run, the appropriate information is written to the output file, and
*   the routine returns TRUE.  If a hard error is encountered, STAT is set to
*   indicate the error and the function returns FALSE.
}
function macro_run (                   {run macro if invocation present}
  out     stat: sys_err_t)             {completion status}
  :boolean;

var
  label: string_var80_t;               {name of optional label on this line}
  name: string_var80_t;                {macro name}
  tk: string_var8192_t;                {scratch token}
  sym_p: sym_p_t;                      {pointer to macro symbol}
  str_p: string_var_p_t;               {pointer to input line}
  oldlen: string_index_t;              {original length of input buffer with comment}
  nclen: string_index_t;               {length of input line with comment stripped}

label
  nomac;

begin
  sys_error_none (stat);               {init to no error encountered}
  macro_run := false;                  {init to no macro executed}
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  label.max := size_char(label.str);   {init local var strings}
  name.max := size_char(name.str);
  tk.max := size_char(tk.str);

  oldlen := ibuf.len;                  {save original length of the input line}
  uptocomm (ibuf, nclen);              {get length of line with comment stripped}
  ibuf.len := nclen;                   {strip off comment from input line}
  if ibuf.len < 2 then goto nomac;     {macro invocation requires at least 2 chars}

  ip := 1;                             {init input line parse index}
  label.len := 0;                      {init to no label on this line}
  if ibuf.str[1] <> ' ' then begin     {a non-blank is in column 1 ?}
    if not get_token(label) then goto nomac; {get the label name into LABEL}
    end;
  if not get_token(name) then goto nomac; {get the opcode name into NAME}
  sym_find (name, sym_p);              {get symbol from macro name}
  if sym_p = nil then goto nomac;      {no such symbol ?}
  if sym_p^.stype <> sym_macro_k then goto nomac; {not a macro ?}
{
*   This line is a macro invocation.  SYM_P is pointing to the macro definition
*   symbol, LABEL contains the name of any label on this line, and NAME is the
*   macro name as it appeared on the invocation line.
}
  exblock_new;                         {create new execution block}
  exblock_p^.sym_p := sym_p;           {set pointer to symbol for this block}
  exblock_p^.bltype := exblock_mac_k;  {new block is a macro}
  exblock_p^.args := true;             {this block can take arguments}
  exblock_loclab_init;                 {create table for local labels}

  if label.len > 0 then begin          {label exists on this line ?}
    exblock_arg_addn (label, -1);      {label name is special argument -1}
    end;
  exblock_arg_addn (name, 0);          {macro name is special argument 0}

  while true do begin                  {loop until argument list exhausted}
    if not get_tkrawc (tk) then exit;  {get next argument}
    exblock_arg_add (tk);              {add as next argument to new execution block}
    end;
  exblock_inline_set (sym_p^.macro_line_p); {go to macro definition line}
  discard( infile_getline (str_p) );   {advance past macro definition line}
  macro_run := true;                   {indicate macro invocation processed}
  return;

nomac:                                 {no macro on this line}
  ibuf.len := oldlen;                  {restore full original input line}
  end;
