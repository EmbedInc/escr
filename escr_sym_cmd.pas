{   Routines that manipulate command name symbols.
}
module escr_sym_cmd;
define escr_sym_new_icmd;
define escr_icmd_add;
define escr_sym_new_cmd;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_ICMD (E, NAME, ROUTINE_P, GLOBAL, SYM_P, STAT)
*
*   Create a new intrinsic command name symbol, or a new version of a existing
*   one.  NAME is the name of the command, and ROUTINE_P points to the routine
*   to run to execute the command.
*
*   The new symbol will be a global symbol when GLOBAL is TRUE, and local to the
*   current execution block when GLOBAL is FALSE.  When in the top level
*   execution block, the symbol will always be global regardless of GLOBAL.
*
*   SYM_P is the returned pointer to the new symbol.
}
procedure escr_sym_new_icmd (          {create new intrinsic command symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      routine_p: escr_icmd_p_t;    {pointer to routine to command routine}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_sym_new (                       {create the basic symbol}
    e, name, escr_sym_icmd_k, global, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.icmd_p := routine_p;          {save pointer to command routine}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_ICMD_ADD (E, NAME, ROUTINE_P, STAT)
*
*   Add a global instrinsic command to the commands symbol table.  NAME is the
*   name of the command, and ROUTINE_P is the pointer to the routine that
*   implements the command.
}
procedure escr_icmd_add (              {add intrinsic command}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of the command to add}
  in      routine_p: escr_icmd_p_t;    {pointer to routine that implements the command}
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the new symbol}

begin
  escr_sym_new_icmd (                  {create new instrinsic command symbol}
    e,                                 {state for this use of the ESCR system}
    name,                              {name of the command}
    routine_p,                         {pointer to the command routine}
    true,                              {make the symbol global}
    sym_p,                             {returned pointer to the new symbol (unused)}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_CMD (E, NAME, LINE_P, GLOBAL, SYM_P, STAT)
*
*   Create a new user-defined command name symbol, or a new version of a
*   existing one.  NAME is the name of the command.  LINE_P points to the first
*   line of the command definition.
*
*   The new symbol will be a global symbol when GLOBAL is TRUE, and local to the
*   current execution block when GLOBAL is FALSE.  When in the top level
*   execution block, the symbol will always be global regardless of GLOBAL.
*
*   SYM_P is returned pointing to the new symbol
}
procedure escr_sym_new_cmd (           {create new user-defined command symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      line_p: escr_inline_p_t;     {pointer to first line of command definition}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_sym_new (                       {create the basic symbol}
    e, name, escr_sym_cmd_k, global, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.cmd_line_p := line_p;         {save pointer to first line of definition}
  end;
