{   Routines that manipulate function name symbols.
}
module escr_sym_func;
define escr_sym_new_ifunc;
define escr_ifunc_add;
define escr_sym_new_func;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_IFUNC (E, NAME, ROUTINE_P, GLOBAL, SYM_P, STAT)
*
*   Create a new intrinsic function name symbol, or a new version of a existing
*   one.  NAME is the name of the function, and ROUTINE_P points to the routine
*   to run to execute the function.
*
*   The new symbol will be a global symbol when GLOBAL is TRUE, and local to the
*   current execution block when GLOBAL is FALSE.  When in the top level
*   execution block, the symbol will always be global regardless of GLOBAL.
*
*   SYM_P is the returned pointer to the new symbol.
}
procedure escr_sym_new_ifunc (         {create new intrinsic function symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      routine_p: escr_ifunc_p_t;   {pointer to function routine}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_sym_new (                       {create the basic symbol}
    e, name, escr_sym_ifunc_k, global, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.ifunc_p := routine_p;         {save pointer to function routine}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFUNC_ADD (E, NAME, ROUTINE_P, STAT)
*
*   Add a global instrinsic function to the functions symbol table.  NAME is the
*   name of the function, and ROUTINE_P is the pointer to the routine that
*   implements the function.
}
procedure escr_ifunc_add (             {add intrinsic function}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of the function to add}
  in      routine_p: escr_ifunc_p_t;   {pointer to routine that implements the function}
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the new symbol}

begin
  escr_sym_new_ifunc (                 {create new instrinsic function symbol}
    e,                                 {state for this use of the ESCR system}
    name,                              {name of the function}
    routine_p,                         {pointer to the function routine}
    true,                              {make the symbol global}
    sym_p,                             {returned pointer to the new symbol (unused)}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYM_NEW_FUNC (E, NAME, LINE_P, GLOBAL, SYM_P, STAT)
*
*   Create a new user-defined function name symbol, or a new version of a
*   existing one.  NAME is the name of the function.  LINE_P points to the first
*   line of the function definition.
*
*   The new symbol will be a global symbol when GLOBAL is TRUE, and local to the
*   current execution block when GLOBAL is FALSE.  When in the top level
*   execution block, the symbol will always be global regardless of GLOBAL.
*
*   SYM_P is returned pointing to the new symbol
}
procedure escr_sym_new_func (          {create new user-defined function symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      line_p: fline_line_p_t;      {pointer to first line of function definition}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_sym_new (                       {create the basic symbol}
    e, name, escr_sym_func_k, global, sym_p, stat);
  if sym_p = nil then return;          {error ?}

  sym_p^.func_line_p := line_p;        {save pointer to first line of definition}
  end;
