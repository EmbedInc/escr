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
*   Command MACRO name
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
  sym_p: escr_sym_p_t;                 {pointer to new name symbol}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  error;

begin
  name.max := size_char(name.str);     {init local var string}

  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_blkdef_k; {inhibit due to reading block definition}
  e.inhibit_p^.blkdef_type := escr_exblock_mac_k; {block type is macro}
  if e.inhibit_p^.inh then return;     {previously inhibited, don't define macro}
  e.inhibit_p^.inh := true;            {inhibit execution during macro definition}

  if not escr_get_token (e, name) then begin {get macro name}
    escr_stat_cmd_noarg (e, stat);
    goto error;
    end;

  escr_sym_new (                       {create new symbol for the macro name}
    e,                                 {state for this use of the ESCR library}
    name,                              {symbol name}
    escr_sym_macro_k,                  {type of symbol to create}
    false,                             {try to make local}
    sym_p,                             {pointer to new symbol}
    stat);
  if sys_error(stat) then goto error;

  sym_p^.macro_line_p :=               {save pointer to macro definition line}
    e.exblock_p^.inpos_p^.last_p;
  return;

error:                                 {error after inhibit created, STAT set}
  escr_inh_end (e, stat2);             {delete the execution inhibit}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_ENDMAC (E, STAT)
*
*   Command ENDMAC
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
    sys_stat_set (escr_subsys_k, escr_err_notmacro_k, stat);
    return;
    end;

  escr_inh_end (e, stat);              {end excution inhibit due to macro def}
  end;
{
********************************************************************************
*
*   Subroutine  ESCR_CMD_QUITMAC (E, STAT)
*
*   Command QUITMAC
*
*   Stop executing the innermost macro currently in.
}
procedure escr_cmd_quitmac (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  while e.exblock_p <> nil do begin    {up thru the nested blocks}
    case e.exblock_p^.bltype of        {what kind of block is this ?}
escr_exblock_top_k: begin              {at top block}
        sys_stat_set (escr_subsys_k, escr_err_notmacro_k, stat);
        return;
        end;
escr_exblock_mac_k: begin              {the block to close}
        escr_exblock_close (e, stat);  {end this macro}
        return;
        end;
escr_exblock_blk_k,                    {BLOCK ... ENDBLOCK}
escr_exblock_loop_k: begin             {LOOP ... ENDLOOP}
        escr_exblock_close (e, stat);  {close this block}
        if sys_error(stat) then return;
        end;
otherwise                              {invalid block type for QUITMAC}
      sys_stat_set (escr_subsys_k, escr_err_notmacro_k, stat);
      return;
      end;
    end;                               {back to close new current block}
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

  oldlen := e.parse_p^.ibuf.len;       {save original length of the input line}
  escr_uptocomm (e, e.parse_p^.ibuf, nclen); {get length of line with comment stripped}
  e.parse_p^.ibuf.len := nclen;        {strip off comment from input line}
  if e.parse_p^.ibuf.len < 2           {macro invocation requires at least 2 chars}
    then goto nomac;

  e.parse_p^.ip := 1;                  {init input line parse index}
  label.len := 0;                      {init to no label on this line}
  if e.parse_p^.ibuf.str[1] <> ' ' then begin {a non-blank is in column 1 ?}
    if not escr_get_token (e, label) then goto nomac; {get the label name into LABEL}
    end;
  if not escr_get_token (e, name) then goto nomac; {get the opcode name into NAME}
  escr_sym_find_type (                 {get macro symbol from name}
    e, name, escr_sytype_macro_k, sym_p, stat);
  if sys_error(stat) then begin        {invalid macro symbol name ?}
    sys_error_none (stat);
    goto nomac;                        {no macro here}
    end;
  if sym_p = nil then goto nomac;      {no such symbol ?}
{
*   This line is a macro invocation.  SYM_P is pointing to the macro definition
*   symbol, LABEL contains the name of any label on this line, and NAME is the
*   macro name as it appeared on the invocation line.
}
  escr_exblock_new (e, stat);          {create new execution block, make it current}
  if sys_error(stat) then return;
  escr_exblock_refsym (e, sym_p^);     {indicate referencing symbol for this macro}
  e.exblock_p^.bltype := escr_exblock_mac_k; {new block is a macro}
  e.exblock_p^.args := true;           {this block can take arguments}
  escr_exblock_ulab_init (e);          {create table for local labels}

  if label.len > 0 then begin          {label exists on this line ?}
    escr_exblock_arg_addn (e, label, -1); {label name is special argument -1}
    end;
   escr_exblock_arg_addn (e, name, 0); {macro name is special argument 0}

  while true do begin                  {loop until argument list exhausted}
    if not escr_get_tkrawc (e, tk) then exit; {get next argument}
    escr_exblock_arg_add (e, tk);      {add as next argument to new execution block}
    end;
  escr_exblock_inline_set (            {go to macro definition line}
    e, sym_p^.macro_line_p, stat);
  if sys_error(stat) then return;
  escr_infile_skipline (e);            {skip over macro definition line}
  escr_macro_run := true;              {indicate macro invocation processed}
  return;

nomac:                                 {no macro on this line}
  e.parse_p^.ibuf.len := oldlen;       {restore full original input line}
  end;
