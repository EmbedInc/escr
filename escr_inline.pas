{   Routines to process inline preprocessor functions.
}
module escr_inline;
define escr_inline_expand_line;
%include 'escr2.ins.pas';
{
*   Private routines used inside this module only.
}
procedure inline_expand_lrest (        {expand inline functions in rest of line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  in      linst: string_index_t;       {LIN index at which to start}
  out     lot: string_var8192_t;       {output string to append expansion to}
  in out  stat: sys_err_t);            {completion status}
  val_param; internal; forward;

procedure inline_expand_func (         {expand rest of line starting at inline func}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input string containing inline function}
  in      linst: string_index_t;       {LIN index after special func start syntax}
  in out  lot: string_var8192_t;       {output string to append expansion to}
  in out  stat: sys_err_t);            {completion status}
  val_param; internal; forward;
{
****************************************************************************
*
*   Subroutine ESCR_INLINE_EXPAND_LINE (E, LIN, LOT, STAT)
*
*   Copy LIN to LOT, expanding all inline functions in the process.
}
procedure escr_inline_expand_line (    {expand all inline functions of a line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  out     lot: string_var8192_t;       {output line, contains no inline functions}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}
  lot.len := 0;                        {init output line to empty}
  inline_expand_lrest (                {expand input line into the output line}
    e,                                 {state for this use of ESCR system}
    lin,                               {input line to expand}
    1,                                 {input line index to start at}
    lot,                               {output string to append expansion to}
    stat);
  end;
{
****************************************************************************
*
*   Subroutine INLINE_EXPAND_LREST (E, LIN, LINST, LOT, STAT)
*
*   Expand the rest of the input line LIN starting at LINST and append the
*   result to LOT.
}
procedure inline_expand_lrest (        {expand inline function in rest of line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  in      linst: string_index_t;       {LIN index at which to start}
  out     lot: string_var8192_t;       {output string to append expansion to}
  in out  stat: sys_err_t);            {completion status}
  val_param; internal;

var
  p: string_index_t;                   {input line parse index}
  cleft: sys_int_machine_t;            {number of characters left on input line}
  ii: sys_int_machine_t;               {scratch integer and loop counter}

label
  nextp;

begin
  p := linst;                          {init input string parse index}

  while p <= lin.len do begin          {loop until input line exhausted}
    if                                 {syntax exclusion here ?}
        escr_excl_check (              {check for exclusion}
          e,                           {state for this use of the ESCR system}
          lin,                         {input string}
          p,                           {input string parse index}
          e.syexcl_p,                  {pointer to exclusions to check}
          addr(lot),                   {pointer to output string to append excl too}
          stat)
        then begin
      if sys_error(stat) then return;  {error ?}
      next;                            {back and check again after this exclusion}
      end;
    if                                 {data file comment here ?}
        escr_excl_check (              {check for data file comment}
          e,                           {state for this use of the ESCR system}
          lin,                         {input string}
          p,                           {input string parse index}
          e.commdat_p,                 {pointer to exclusions to check}
          addr(lot),                   {pointer to output string to append excl too}
          stat)
        then begin
      if sys_error(stat) then return;  {error ?}
      next;                            {back and check again after this comment}
      end;
    {
    *   Check for function start sequence here.
    }
    cleft := lin.len - p + 1;          {number of input characters left}
    if e.syfunc.st.len > cleft then exit; {no room for function start here ?}
    for ii := 1 to e.syfunc.st.len do begin {compare input string to function start}
      if lin.str[p + ii - 1] <> e.syfunc.st.str[ii] {mismatch ?}
        then goto nextp;
      end;
    inline_expand_func (               {expand inline function and rest of line}
      e,                               {state for this use of ESCR system}
      lin,                             {input string}
      p + e.syfunc.st.len,             {start of function to interpret}
      lot,                             {string to append expansion to}
      stat);
    return;                            {all done}

nextp:                                 {advance to next input string character}
    string_append1 (lot, lin.str[p]);  {copy this character to output string}
    p := p + 1;                        {to next character}
    end;                               {back check at this new character}
  end;
{
****************************************************************************
*
*   Subroutine INLINE_EXPAND_FUNC (E, LIN, LINST, LOT, STAT)
*
*   Expand an inline function and the remainder of the input line.  The
*   input line is in LIN, and LINST is the index of the first character
*   after the special function start syntax.  The expansion of the inline
*   function and everything following it is appended to LOT.
}
procedure inline_expand_func (         {expand rest of line starting at inline func}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input string containing inline function}
  in      linst: string_index_t;       {LIN index after special func start syntax}
  in out  lot: string_var8192_t;       {output string to append expansion to}
  in out  stat: sys_err_t);            {completion status}
  val_param; internal;

var
  exp: string_var8192_t;               {expansion of remainder of line}
  fun: string_var8192_t;               {just the content of this function}
  p: string_index_t;                   {input line parse index}
  cleft: sys_int_machine_t;            {number of characters left on input line}
  ii: sys_int_machine_t;               {scratch integer and loop counter}

(*
  tk: string_var8192_t;                {TEMP DEBUG}
*)

label
  nextp;

begin
  exp.max := size_char(exp.str);       {init local var strings}
  fun.max := size_char(fun.str);

  exp.len := 0;                        {init rest of line to empty}
  inline_expand_lrest (                {expand rest of line into EXP}
    e, lin, linst, exp, stat);
  if sys_error(stat) then return;
{
*   EXP contains the remainder of the line with all other functions except this
*   one expanded.
*
*   Look for the end of this function.  The content of this function will be
*   copied to FUN with comments stripped.
}
  fun.len := 0;                        {init extracted function content}
  p := 1;                              {init EXP input string parse index}

  while p <= exp.len do begin          {loop until input string exhausted}
    if                                 {syntax exclusion here ?}
        escr_excl_check (              {check for exclusion}
          e,                           {state for this use of the ESCR system}
          exp,                         {input string}
          p,                           {input string parse index}
          e.syexcl_p,                  {pointer to exclusions to check}
          addr(fun),                   {where to copy skipped characters to}
          stat)
        then begin
      if sys_error(stat) then return;  {error ?}
      next;                            {back and check again after this exclusion}
      end;
    if                                 {data file comment here ?}
        escr_excl_check (              {check for data file comment}
          e,                           {state for this use of the ESCR system}
          exp,                         {input string}
          p,                           {input string parse index}
          e.commdat_p,                 {pointer to exclusions to check}
          nil,                         {don't copy comment to anywhere}
          stat)
        then begin
      if sys_error(stat) then return;  {error ?}
      string_append1 (fun, ' ');       {replace comment with single space}
      next;                            {back and check again after this comment}
      end;
    {
    *   Check for function end sequence here.
    }
    cleft := exp.len - p + 1;          {number of input characters left}
    if e.syfunc.en.len > cleft then exit; {no room for function end here ?}
    for ii := 1 to e.syfunc.en.len do begin {compare input string to function end}
      if exp.str[p + ii - 1] <> e.syfunc.en.str[ii] {mismatch ?}
        then goto nextp;
      end;
    {
    *   P is the EXP index of the special end of function sequence.  The
    *   funtion body (part of function between the start and end sequences) is
    *   in FUN.
    *
    *   The function is expanded and the result appended to the output string,
    *   followed by the rest of the expaneded input line after the function end
    *   sequence.
    }



(*
    {***** TEMP DEBUG *****}
    tk.max := size_char(tk.str);
    tk.len := 0;
    escr_inline_func (                 {expand function and append result to output string}
      e,                               {state for this use of the ESCR system}
      fun,                             {function string with start/end sequences removed}
      tk,                              {string to append function expansion to}
      stat);
    if sys_error(stat) then return;
    writeln ('Func: ', fun.str:fun.len, ' --> ', tk.str:tk.len);
    string_append (lot, tk);
    p := p + e.syfunc.en.len;          {skip over function end sequence}
    while p <= exp.len do begin        {copy remainder of expanded input line to output}
      string_append1 (lot, exp.str[p]);
      p := p + 1;
      end;
    return;
    {***** END DEBUG *****}
*)



    escr_inline_func (                 {expand function and append result to output string}
      e,                               {state for this use of the ESCR system}
      fun,                             {function string with start/end sequences removed}
      lot,                             {string to append function expansion to}
      stat);
    if sys_error(stat) then return;

    p := p + e.syfunc.en.len;          {skip over function end sequence}
    while p <= exp.len do begin        {copy remainder of expanded input line to output}
      string_append1 (lot, exp.str[p]);
      p := p + 1;
      end;
    return;

nextp:                                 {done with this input string char, on to next}
    string_append1 (fun, exp.str[p]);  {copy this input string char to output string}
    p := p + 1;                        {to next input string character}
    end;                               {back to check at this new character}
{
*   The function end was not found.
}
  sys_stat_set (escr_subsys_k, escr_err_funcnend_k, stat); {end of function not found}
  sys_stat_parm_vstr (fun, stat);
  end;
