{   Commands relating to preprocessor macros.  These are defined much like
*   preprocessor subroutines but are envoked just like assembler macros.
}
module escr_cmd_macro;
define escr_cmd_macro;
define escr_cmd_endmac;
define escr_cmd_quitmac;
define escr_macro_run;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_MACRO (E, STAT)
*
*   /MACRO name
*
*   Starts the definition of a macro.  The macro definition is stored but not
*   executed now.
}
procedure escr_cmd_macro (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {macro name}
  sz: sys_int_adr_t;                   {size of new descriptor}
  sym_p: escr_sym_p_t;                 {pointer to new name symbol}

begin
  name.max := size_char(name.str);     {init local var string}

  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_blkdef_k; {inhibit it due to reading block definition}
  e.inhibit_p^.blkdef_type := escr_exblock_mac_k; {block type is macro}
  if e.inhibit_p^.inh then return;     {previously inhibited, don't define macro}
  e.inhibit_p^.inh := true;            {inhibit execution during macro definition}

  if not escr_get_token (e, name)      {get macro name}
    then escr_err_parm_missing (e, '', '', nil, 0);
  escr_err_check_symname (e, name);    {check for valid symbol name}

  sz :=                                {make size of whole macro symbol}
    offset(escr_sym_t.macro_line_p) + size_min(escr_sym_t.macro_line_p);
  escr_sym_new (e, name, sz, false, sym_p); {create new symbol for macro name}
  sym_p^.stype := escr_sym_macro_k;    {this symbol is a macro name}
  sym_p^.macro_line_p :=               {save pointer to macro definition line}
    e.exblock_p^.inpos_p^.last_p;
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ENDMAC (E, STAT)
*
*   /ENDMAC
*
*   End the current macro definition.
}
procedure escr_cmd_endmac (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if not e.inhibit_p^.inh then begin   {executing code normally ?}
    escr_cmd_quitmac (e, stat);        {acts just like QUITMAC}
    return;
    end;

  if                                   {not defining a macro ?}
      (e.inhibit_p^.inhty <> escr_inhty_blkdef_k) or {not in a block definition ?}
      (e.inhibit_p^.blkdef_type <> escr_exblock_mac_k) {block is not a macro ?}
      then begin
    escr_err_atline (e, 'pic', 'not_in_macdef', nil, 0);
    end;

  escr_inh_end (e);                    {end excution inhibit due to macro def}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_QUITMAC (E, STAT)
*
*   /QUITMAC
*
*   Stop executing the innermost macro currently in.
}
procedure escr_cmd_quitmac (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  while e.exblock_p^.bltype <> escr_exblock_mac_k do begin {up thru blocks until first macro}
    if e.exblock_p^.prev_p = nil then begin {at top execution block ?}
      escr_err_atline (e, 'pic', 'not_in_macro', nil, 0); {complain not in a macro}
      end;
    escr_exblock_close (e);            {end this execution block, make previous current}
    end;                               {back to check this new execution block}

  escr_exblock_close (e);              {end the macro execution block}
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
function escr_macro_run (              {run macro if invocation present}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {completion status}
  :boolean;

var
  label: string_var80_t;               {name of optional label on this line}
  name: string_var80_t;                {macro name}
  tk: string_var8192_t;                {scratch token}
  sym_p: escr_sym_p_t;                 {pointer to macro symbol}
  str_p: string_var_p_t;               {pointer to input line}
  oldlen: string_index_t;              {original length of input buffer with comment}
  nclen: string_index_t;               {length of input line with comment stripped}

label
  nomac;

begin
  sys_error_none (stat);               {init to no error encountered}
  escr_macro_run := false;             {init to no macro executed}
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  label.max := size_char(label.str);   {init local var strings}
  name.max := size_char(name.str);
  tk.max := size_char(tk.str);

  oldlen := e.ibuf.len;                {save original length of the input line}
  escr_uptocomm (e, e.ibuf, nclen);    {get length of line with comment stripped}
  e.ibuf.len := nclen;                 {strip off comment from input line}
  if e.ibuf.len < 2 then goto nomac;   {macro invocation requires at least 2 chars}

  e.ip := 1;                           {init input line parse index}
  label.len := 0;                      {init to no label on this line}
  if e.ibuf.str[1] <> ' ' then begin   {a non-blank is in column 1 ?}
    if not escr_get_token (e, label) then goto nomac; {get the label name into LABEL}
    end;
  if not escr_get_token (e, name) then goto nomac; {get the opcode name into NAME}
  escr_sym_find (e, name, sym_p);      {get symbol from macro name}
  if sym_p = nil then goto nomac;      {no such symbol ?}
  if sym_p^.stype <> escr_sym_macro_k then goto nomac; {not a macro ?}
{
*   This line is a macro invocation.  SYM_P is pointing to the macro definition
*   symbol, LABEL contains the name of any label on this line, and NAME is the
*   macro name as it appeared on the invocation line.
}
  escr_exblock_new (e);                {create new execution block}
  e.exblock_p^.sym_p := sym_p;         {set pointer to symbol for this block}
  e.exblock_p^.bltype := escr_exblock_mac_k; {new block is a macro}
  e.exblock_p^.args := true;           {this block can take arguments}
  escr_exblock_loclab_init (e);        {create table for local labels}

  if label.len > 0 then begin          {label exists on this line ?}
    escr_exblock_arg_addn (e, label, -1); {label name is special argument -1}
    end;
   escr_exblock_arg_addn (e, name, 0); {macro name is special argument 0}

  while true do begin                  {loop until argument list exhausted}
    if not escr_get_tkrawc (e, tk) then exit; {get next argument}
    escr_exblock_arg_add (e, tk);      {add as next argument to new execution block}
    end;
  escr_exblock_inline_set (e, sym_p^.macro_line_p); {go to macro definition line}
  discard( escr_infile_getline (e, str_p) ); {advance past macro definition line}
  escr_macro_run := true;              {indicate macro invocation processed}
  return;

nomac:                                 {no macro on this line}
  e.ibuf.len := oldlen;                {restore full original input line}
  end;
