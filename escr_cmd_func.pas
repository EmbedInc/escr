{   Subroutine management.
}
module escr_cmd_subr;
define escr_cmd_function;
define escr_cmd_endfunc;
define escr_cmd_funcval;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_CMD_FUNC (E, STAT)
*
*   Command FUNC name
*
*   Define the start of a function.  The function definition is stored, but the
*   function is not executed now.
}
procedure escr_cmd_function (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {function name}
  sz: sys_int_adr_t;                   {size of new descriptor}
  sym_p: escr_sym_p_t;                 {pointer to new function name symbol}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  error;

begin
  name.max := size_char(name.str);     {init local var string}

  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_blkdef_k; {inhibit is due to reading block definition}
  e.inhibit_p^.blkdef_type := escr_exblock_func_k; {block type is function}
  if e.inhibit_p^.inh then return;     {previously inhibited, don't define function}
  e.inhibit_p^.inh := true;            {inhibit execution during function definition}

  if not escr_get_token (e, name) then begin {get function name}
    escr_stat_cmd_noarg (e, stat);
    goto error;
    end;

  sz :=                                {make size of whole function symbol}
    offset(escr_sym_t.func_line_p) + size_min(escr_sym_t.func_line_p);
  escr_sym_new (                       {create new symbol for function name}
    e,                                 {ESCR library use state}
    name,                              {bare symbol name}
    sz,                                {size of whole symbol descriptor}
    false,                             {make symbol local, not global}
    e.sym_fun,                         {symbol table to create new symbol in}
    sym_p,                             {returned pointer to the new symbol}
    stat);
  if sys_error(stat) then goto error;

  sym_p^.stype := escr_sym_func_k;     {this symbol is a function name}
  sym_p^.func_line_p :=                {save pointer to function definition line}
    e.exblock_p^.inpos_p^.last_p;
  return;

error:                                 {error after inhibit created, STAT set}
  escr_inh_end (e, stat2);             {delete the execution inhibit}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_ENDFUNC (E, STAT)
*
*   Command ENDFUNC
*
*   Indicate the end of a function definition.
}
procedure escr_cmd_endfunc (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if not e.inhibit_p^.inh then begin   {executing code normally ?}
    escr_cmd_return (e, stat);         {acts just like RETURN}
    return;
    end;

  if                                   {not defining a function ?}
      (e.inhibit_p^.inhty <> escr_inhty_blkdef_k) or {not in a block definition ?}
      (e.inhibit_p^.blkdef_type <> escr_exblock_func_k) {block is not a function ?}
      then begin
    sys_stat_set (escr_subsys_k, escr_err_notfuncdef_k, stat);
    return;
    end;

  escr_inh_end (e, stat);              {end excution inhibit due to function def}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_FUNCVAL (E, STAT)
*
*   Command FUNCVAL arg ... arg
*
*   Append the concatenation of all the command arguments to the innermost
*   user-defined function return value.
}
procedure escr_cmd_funcval (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  blk_p: escr_exblock_p_t;             {pointer to function excecution block}
  par_p: escr_parse_p_t;               {pointer to parse state to add values to}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  blk_p := e.exblock_p;                {init pointer to current block}
  while blk_p^.bltype <> escr_exblock_func_k do begin {not in top level of function}
    blk_p := blk_p^.prev_p;            {go to parent block}
    if blk_p = nil then begin          {no function block found at all ?}
      sys_stat_set (escr_subsys_k, escr_err_nfunc_k, stat);
      return;
      end;
    end;                               {back to check this new execution block}
{
*   BLK_P is pointing to the the execution block for the function to return the
*   value of.
}
  par_p := blk_p^.parse_p;             {point to internal parse state of func block}
  if par_p <> nil then par_p := par_p^.prev_p; {go to parse state to add func val to}
  if par_p = nil then begin            {function return value parse state not exist ?}
    sys_stat_set (escr_subsys_k, escr_err_nparse_fval_k, stat);
    return;
    end;
{
*   PAR_P is pointing to the parsing state to add the function return value to.
}
  escr_get_args_str (                  {get concatenation of all command args}
    e, e.parse_p^.funret, stat);
  if sys_error(stat) then return;
  string_append (par_p^.funret, e.parse_p^.funret); {append to function return}
  end;
