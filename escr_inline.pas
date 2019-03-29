{   Routines to process inline functions.
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

procedure inline_func (                {perform inline function operation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  lot: string_var8192_t;       {string to append function expansion to}
  out     stat: sys_err_t);            {completion status}
  val_param; internal; forward;
{
********************************************************************************
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
********************************************************************************
*
*   Local subroutine INLINE_EXPAND_LREST (E, LIN, LINST, LOT, STAT)
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
    if e.syfunc_st_p = nil
      then begin                       {no app function, use syntax start string}
        cleft := lin.len - p + 1;      {number of input characters left}
        if e.syfunc.st.len > cleft then exit; {no room for function start here ?}
        for ii := 1 to e.syfunc.st.len do begin {compare input string to function start}
          if lin.str[p + ii - 1] <> e.syfunc.st.str[ii] {mismatch ?}
            then goto nextp;
          end;
        p := p + e.syfunc.st.len;      {skip over function start syntax}
        end
      else begin                       {use app routine to identify function start}
        if not e.syfunc_st_p^ (
            addr(e),                   {pointer to script system state}
            lin,                       {source line}
            p,                         {index to check for func start at, updated if found}
            stat) then begin
          if sys_error(stat) then return;
          goto nextp;                  {no function here, advance to next character}
          end;
        end
      ;
    {
    *   A function start was found.  P has been advanced to immediately after
    *   the function start syntax.
    }
    inline_expand_func (               {expand inline function and rest of line}
      e,                               {state for this use of ESCR system}
      lin,                             {input string}
      p,                               {start of function to interpret}
      lot,                             {string to append expansion to}
      stat);
    return;                            {all done}

nextp:                                 {advance to next input string character}
    string_append1 (lot, lin.str[p]);  {copy this character to output string}
    p := p + 1;                        {to next character}
    end;                               {back check at this new character}
  end;
{
********************************************************************************
*
*   Local subroutine INLINE_EXPAND_FUNC (E, LIN, LINST, LOT, STAT)
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
  p: string_index_t;                   {input line parse index}
  cleft: sys_int_machine_t;            {number of characters left on input line}
  ii: sys_int_machine_t;               {scratch integer and loop counter}

label
  nextp;

begin
  exp.max := size_char(exp.str);       {init local var string}

  exp.len := 0;                        {init rest of line to empty}
  inline_expand_lrest (                {expand rest of line into EXP}
    e, lin, linst, exp, stat);
  if sys_error(stat) then return;
{
*   EXP contains the remainder of the line with all other functions except this
*   one expanded.
*
*   Look for the end of this function.  The content of this function will be
*   copied to FUNARG with comments stripped.
}
  e.parse_p^.funarg.s.len := 0;        {init extracted function string to empty}
  p := 1;                              {init EXP input string parse index}

  while p <= exp.len do begin          {loop until input string exhausted}
    if                                 {syntax exclusion here ?}
        escr_excl_check (              {check for exclusion}
          e,                           {state for this use of the ESCR system}
          exp,                         {input string}
          p,                           {input string parse index}
          e.syexcl_p,                  {pointer to exclusions to check}
          addr(e.parse_p^.funarg.s),   {where to copy skipped characters to}
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
      string_append1 (e.parse_p^.funarg.s, ' '); {replace comment with single space}
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
    *   in FUNARG.
    *
    *   The function is expanded and the result appended to the output string,
    *   followed by the rest of the expaneded input line after the function end
    *   sequence.
    }
    e.parse_p^.funarg.p := 1;          {init to parse at start of function string}
    inline_func (                      {expand function and append result to output string}
      e,                               {state for this use of the ESCR system}
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
    string_append1 (                   {copy this input string char to output string}
      e.parse_p^.funarg.s, exp.str[p]);
    p := p + 1;                        {to next input string character}
    end;                               {back to check at this new character}
{
*   The function end was not found.
}
  sys_stat_set (escr_subsys_k, escr_err_funcnend_k, stat); {end of function not found}
  sys_stat_parm_vstr (e.parse_p^.funarg.s, stat);
  end;
{
********************************************************************************
*
*   Local subroutine INLINE_FUNC (E, LOT, STAT)
*
*   Perform the operation indicated by the inline function in FUNARG.  The
*   resulting expansion, if any, is appended to LOT.  FUNARG contains exactly
*   the function body.  This is the part just inside the "[" and "]" when the
*   default syntax is used.
}
procedure inline_func (                {perform inline function operation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  lot: string_var8192_t;       {string to append function expansion to}
  out     stat: sys_err_t);            {completion status}
  val_param; internal;

var
  sym_p: escr_sym_p_t;                 {pointer to function symbol in symbol table}
  parm: string_var32_t;                {unused function parameter}

begin
  parm.max := size_char(parm.str);     {init local var string}
{
*   Parse the function name into E.PARSE_P^.FUNAME.
}
  string_token (                       {get function name}
    e.parse_p^.funarg.s,               {source string}
    e.parse_p^.funarg.p,               {parse index, updated}
    e.parse_p^.funame,                 {returned token}
    stat);
  if string_eos(stat) then begin       {end of string, no function name ?}
    e.parse_p^.funame.len := 0;
    end;
  if sys_error(stat) then return;
{
*   Look up the function name in the functions symbol table.
}
  escr_sym_find_type (                 {find the function name symbol}
    e,                                 {state for this use of the ESCR system}
    e.parse_p^.funame,                 {name of symbol to look for}
    escr_sytype_func_k,                {the symbol must be a function}
    sym_p,                             {returned pointer to symbol in the table}
    stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {no such function ?}
    sys_stat_set (escr_subsys_k, escr_err_funcnfnd_k, stat);
    sys_stat_parm_vstr (e.parse_p^.funame, stat); {function name}
    return;                            {abort}
    end;

  e.parse_p^.funret.len := 0;          {init function expansion string to empty}
  case sym_p^.stype of                 {what kind of symbol is this ?}
{
*   Intrinsic function.  These are implemented by compiled code that has been
*   statically linked in.
}
escr_sym_ifunc_k: begin                {intrinsic function}
      sys_error_none (stat);           {init to no error encountered in function}
      sym_p^.ifunc_p^ (                {run the intrinsic function routine}
        addr(e),                       {pointer to this ESCR system use state}
        stat);                         {returned error status, initialized to no err}
      if sys_error(stat) then return;

      string_append (lot, e.parse_p^.funret); {append function expansion to output line}

      string_token (                   {try to get another function parameter}
        e.parse_p^.funarg.s,           {source string}
        e.parse_p^.funarg.p,           {parse index, updated}
        parm,                          {returned token}
        stat);
      if not string_eos(stat) then begin {found unused parameter ?}
        sys_stat_set (escr_subsys_k, escr_err_exparmfun_k, stat); {extra parameter error}
        sys_stat_parm_vstr (parm, stat); {the extra parameter}
        sys_stat_parm_vstr (e.parse_p^.funame, stat); {the function name}
        return;
        end;
      end;
{
*   User-defined function.
}
escr_sym_func_k: begin                 {user-defined function}
      escr_exblock_new (e, stat);      {create new  execution block}
      if sys_error(stat) then return;
      escr_exblock_refsym (e, sym_p^); {indicate referencing symbol for this block}
      e.exblock_p^.bltype := escr_exblock_func_k; {new block is a function}
      e.exblock_p^.args := true;       {this block can take arguments}
      escr_exblock_arg_addn (          {function name is special argument 0}
        e, e.parse_p^.funame, 0);
      while true do begin              {loop until argument list exhausted}
        if not                         {try to get next function parameter}
          escr_term_raw (e, e.parse_p^.funarg.s, e.parse_p^.funarg.p, parm, stat)
          then exit;                   {exhausted function arguments ?}
        escr_exblock_arg_add (e, parm); {add as next argument to new execution block}
        end;
      escr_exblock_parse_save (e, stat); {save parsing state, create new}
      if sys_error(stat) then return;
      escr_exblock_inline_set (        {go to function definition line}
        e, sym_p^.func_line_p, stat);
      if sys_error(stat) then return;
      escr_infile_skipline (e);        {skip over command definition line}
      escr_exblock_ulab_init (e);      {create table for local labels}
      escr_run (e, stat);              {run function, block deleted when done}
      string_append (lot, e.parse_p^.funret); {append function expansion to output line}
      end;
{
*   Unexpected symbol type.
}
otherwise
    sys_stat_set (escr_subsys_k, escr_err_notfunc_k, stat);
    sys_stat_parm_vstr (e.parse_p^.funame, stat); {function name}
    return;
    end;                               {end of symbol type cases}
  end;
