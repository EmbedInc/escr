{   Routines for handling terms in expressions or function parameters.
}
module escr_term;
define escr_term_get;
%include 'escr2.ins.pas';
{
****************************************************************************
*
*   Function ESCR_TERM_GET (E, FSTR, P, VAL)
*
*   Get the next token in FSTR and interpret it as a term or inline function
*   argument.  P is the FSTR parse index and VAL is the returned value
*   of the term.  The function returns TRUE when returning with a value,
*   and FALSE if no term was found before the end of the input string.
*
*   VAL is assumed to be completely uninitialized and the full VAL_T structure
*   as defined in the include file.  VAL must not be a permanent VAL structure
*   that may have less memory allocated, such as used to hold the value of
*   a variable or constant.
}
function escr_term_get (               {get value of next term in list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index}
  out     val: escr_val_t)             {returned value of the term}
  :boolean;                            {TRUE if term was available}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: escr_sym_p_t;                 {pointer to constant or variable descriptor}
  c: char;                             {scratch character}
  tk: string_var8192_t;                {token parsed from input string}
  tku: string_var32_t;                 {scratch upper case token}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  not_sym;

begin
  tk.max := size_char(tk.str);         {init local var string}
  tku.max := size_char(tku.str);

  escr_term_get := false;              {init to no term found}
  if p > fstr.len then return;         {nothing left to parse from input string ?}
  while fstr.str[p] = ' ' do begin     {skip over blanks}
    p := p + 1;                        {advance to next input string char}
    if p > fstr.len then return;       {hit end of input string ?}
    end;                               {back to check this next input string char}
{
*   P is the index of the first character of the term token.
}
  c := fstr.str[p];                    {save the first token character}
  string_token (fstr, p, tk, stat);    {get the token contents}
  if string_eos(stat) then return;     {nothing there ?}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  escr_term_get := true;               {indicate returning with a value}
{
*   Check for token is text string.
*
*   If the token was a quoted string, then its first character must be
*   a quote or apostrophy as indicated by the character C.  The string
*   in TK already has any surrounding quotes removed by STRING_TOKEN.
}
  if (c = '''') or (c = '"') then begin {token was a quoted text string ?}
    val.dtype := escr_dtype_str_k;     {pass back value as text string}
    val.str.max := size_char(val.str.str);
    string_copy (tk, val.str);         {return the string}
    return;
    end;
{
*   Check for integer.
}
  string_t_int_max (tk, val.int, stat); {try converting to integer}
  if not sys_error(stat) then begin    {integer conversion succeeded ?}
    val.dtype := escr_dtype_int_k;
    return;
    end;
{
*   Check for floating point.
}
  string_t_fpmax (tk, val.fp, [], stat); {try converting to floating point}
  if not sys_error(stat) then begin    {floating point conversion succeeded ?}
    val.dtype := escr_dtype_fp_k;
    return;
    end;
{
*   Check for time.
}
  if escr_str_to_time (e, tk, val.time) then begin
    val.dtype := escr_dtype_time_k;
    return;
    end;
{
*   Check for boolean.
}
  string_copy (tk, tku);               {convert to upper case for keyword matching}
  string_upcase (tku);
  string_tkpick80 (tku, 'FALSE TRUE', pick);
  if pick > 0 then begin               {token matched one of the keywords ?}
    val.bool := pick = 2;
    val.dtype := escr_dtype_bool_k;
    return;
    end;
{
*   Check for symbol reference.
}
  if not escr_sym_name (e, tk) then goto not_sym; {token is not a valid symbol name}
  escr_sym_find (e, tk, e.sym_var, sym_p); {lookup name in var/const symbol table}
  if sym_p = nil then goto not_sym;    {no such symbol ?}
  case sym_p^.stype of                 {what kind of symbol is it ?}
escr_sym_var_k: begin                  {symbol is a variable}
      escr_val_init (e, sym_p^.var_val.dtype, val); {set up VAL for this data type}
      escr_val_copy (e, sym_p^.var_val, val); {return the variable's value}
      return;
      end;
escr_sym_const_k: begin                {symbol is a constant}
      escr_val_init (e, sym_p^.const_val.dtype, val); {set up VAL for this data type}
      escr_val_copy (e, sym_p^.const_val, val); {return the constant's value}
      return;
      end;
otherwise                              {this symbol can't be used as a term}
    sys_msg_parm_vstr (msg_parm[1], tk);
    escr_err_atline (e, 'pic', 'sym_nval', msg_parm, 1);
    return;
    end;
not_sym:                               {skip to here on not a symbol reference}
{
*   The token is not a recognizable term.
}
  sys_msg_parm_vstr (msg_parm[1], tk);
  escr_err_atline (e, 'pic', 'term_bad', msg_parm, 1); {bomb with error message}
  return;                              {keep compiler from complaining}
  end;
