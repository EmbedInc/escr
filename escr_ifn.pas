{   Module of helper routines for implementing intrinsic functions.
}
module escr_ifn;
define escr_ifn_get_val;
define escr_ifn_get_bool;
define escr_ifn_get_int;
define escr_ifn_get_time;
define escr_ifn_get_fp;
define escr_ifn_get_str;
define escr_ifn_get_strs;
define escr_ifn_get_keyw;
define escr_ifn_get_name;
define escr_ifn_get_var_int;
define escr_ifn_ret_bool;
define escr_ifn_ret_int;
define escr_ifn_ret_fp;
define escr_ifn_ret_str;
define escr_ifn_ret_strp;
define escr_ifn_ret_chars;
define escr_ifn_ret_charsp;
define escr_ifn_ret_char;
define escr_ifn_ret_empty;
define escr_ifn_ret_time;
define escr_ifn_ret_val;
define escr_ifn_stat_required;
define escr_ifn_bad_dtype;
define escr_ifn_bad_keyw;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Function ESCR_IFN_GET_VAL (E, VAL, STAT)
*
*   Get the next function parameter and return its value in VAL.  The function
*   value is TRUE iff a term was available.  When the function returns TRUE,
*   STAT always indicates no error.
}
function escr_ifn_get_val (            {get arbitrary value of next func parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t;             {returned term value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {term was available}
  val_param;

begin
  escr_ifn_get_val :=
    escr_term_val (e, e.parse_p^.funarg.s, e.parse_p^.funarg.p, val, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_BOOL (E, B, STAT)
*
*   Get the next function parameter as a boolean value into B.  The function
*   returns TRUE iff a boolean term was found.  STAT is returned with error if
*   a term was found but it could not be converted to a boolean value.  If no
*   term was found, the function returns FALSE and STAT does not indicate error.
}
function escr_ifn_get_bool (           {get boolean value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean;                  {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with boolean value}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_ifn_get_bool := false;          {init to not returning with value}

  if not escr_ifn_get_val (e, val, stat) {no term or error ?}
    then return;

  b := escr_val_bool (e, val, stat);   {try to convert value to boolean}
  if not sys_error(stat) then begin    {success ?}
    escr_ifn_get_bool := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_INT (E, II, STAT)
*
*   Get the next function parameter as a integer value into II.  The function
*   returns TRUE iff a integer term was found.  STAT is returned with error if
*   a term was found but it could not be converted to a integer value.  If no
*   term was found, the function returns FALSE and STAT does not indicate error.
}
function escr_ifn_get_int (            {get integer value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     ii: sys_int_max_t;           {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with integer value}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_ifn_get_int := false;           {init to not returning with value}

  if not escr_ifn_get_val (e, val, stat) {no term or error ?}
    then return;

  ii := escr_val_int (e, val, stat);   {try to convert value to integer}
  if not sys_error(stat) then begin    {success ?}
    escr_ifn_get_int := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_TIME (E, T, STAT)
*
*   Get the next function parameter as a time value into T.  The function
*   returns TRUE iff a time term was found.  STAT is returned with error if
*   a term was found but it could not be converted to a time value.  If no
*   term was found, the function returns FALSE and STAT does not indicate error.
}
function escr_ifn_get_time (           {get time value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     t: sys_clock_t;              {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with time value}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_ifn_get_time := false;          {init to not returning with value}

  if not escr_ifn_get_val (e, val, stat) {no term or error ?}
    then return;

  t := escr_val_time (e, val, stat);   {try to convert value to time}
  if not sys_error(stat) then begin    {success ?}
    escr_ifn_get_time := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_FP (E, FP, STAT)
*
*   Get the next function parameter as a floating point value into FP.  The
*   function returns TRUE iff a floating point term was found.  STAT is returned
*   with error if a term was found but it could not be converted to a floating
*   point value.  If no term was found, the function returns FALSE and STAT does
*   not indicate error.
}
function escr_ifn_get_fp (             {get floating point value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     fp: sys_fp_max_t;            {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with floating point value}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_ifn_get_fp := false;            {init to not returning with value}

  if not escr_ifn_get_val (e, val, stat) {no term or error ?}
    then return;

  fp := escr_val_fp (e, val, stat);    {try to convert value to floating point}
  if not sys_error(stat) then begin    {success ?}
    escr_ifn_get_fp := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_STR (E, STR, STAT)
*
*   Get the next function parameter as a string value into STR.  The function
*   returns TRUE iff a string term was found.  STAT is returned with error if
*   a term was found but it could not be converted to a string value.  If no
*   term was found, the function returns FALSE and STAT does not indicate error.
}
function escr_ifn_get_str (            {get string value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with string value}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_ifn_get_str := false;           {init to not returning with value}

  if not escr_ifn_get_val (e, val, stat) {no term or error ?}
    then return;

  escr_val_str (e, val, str);          {make string representation of term value}
  escr_ifn_get_str := true;            {indicate returning with value}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_GET_STRS (E, STR, STAT)
*
*   Get the concatenation of the string representations of all the remaining
*   function parameters in STR.  STR will be the empty string when there is no
*   remaining parameter, or all the parameters are the empty string.
}
procedure escr_ifn_get_strs (          {get string of all remaining parameters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned string}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  tk: string_var8192_t;                {string representation of one parameter}

begin
  tk.max := size_char(tk.str);         {init local var string}
  str.len := 0;                        {init the returned string to empty}

  while true do begin                  {back here for each new parameter}
    if not escr_ifn_get_str (e, tk, stat) then return; {no parameter or error ?}
    string_append (str, tk);           {add this parameter to end of string}
    end;                               {back to get the next parameter}
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_KEYW (E, KEYW, STAT)
*
*   Get the next function parameter as a upper case keyword in KEYW.  The
*   function returns TRUE when returning with a keyword.  If no parameter was
*   available, then the funtion returns FALSE with STAT not indicating error.
*   On error, the function returns FALSE with STAT indicating the error.
}
function escr_ifn_get_keyw (           {get next parameter as upper case keyword}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  keyw: univ string_var_arg_t; {returned keyword, upper case}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with a keyword}
  val_param;

var
  quoted: boolean;                     {term was quoted string}

begin
  escr_ifn_get_keyw := false;          {init to not returning with a keyword}

  if not escr_term_parse (             {get characters of next term}
      e,                               {ESCR library use state}
      e.parse_p^.funarg.s, e.parse_p^.funarg.p, {source string and parse index}
      keyw,                            {returned term characters}
      quoted,                          {indicates whether term was quoted}
      stat)
    then return;
  if quoted then return;               {quoted string isn't a keyword}

  string_upcase (keyw);                {convert keyword to all upper case}
  escr_ifn_get_keyw := true;           {indicate returning with a keyword}
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_NAME (E, NAME, STAT)
*
*   Get the next function parameter as a symbol name in NAME.  The function
*   returns TRUE when returning with a name.  If no parameter was available,
*   then the funtion returns FALSE with STAT not indicating error.  On error,
*   the function returns FALSE with STAT indicating the error.
}
function escr_ifn_get_name (           {get next parameter as symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  name: univ string_var_arg_t; {returned name}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with name}
  val_param;

var
  quoted: boolean;                     {term was quoted string}

begin
  escr_ifn_get_name := false;          {init to not returning with a name}

  if not escr_term_parse (             {get characters of next term}
      e,                               {ESCR library use state}
      e.parse_p^.funarg.s, e.parse_p^.funarg.p, {source string and parse index}
      name,                            {returned term characters}
      quoted,                          {indicates whether term was quoted}
      stat)
    then return;
  if quoted then return;               {quoted string isn't a name}

  escr_ifn_get_name := true;           {indicate returning with a name}
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_GET_VAR_INT (E, SYM_P, STAT)
*
*   Get the next function parameter as a integer variable name.  SYM_P is
*   returned pointing to the variable.
*
*   The function returns TRUE iff the variable name was successfully parsed, and
*   it is the name of a integer variable.  In this case SYM_P will always point
*   to the symbol descriptor.  The symbol is guaranteed to be a variable with
*   integer data type.
*
*   If there is no parameter, then the function returns FALSE, SYM_P nil, and
*   STAT not indicating error.
*
*   If any error is detected, the function returns FALSE, SYM_P is undefined,
*   and STAT indicates the error.
}
function escr_ifn_get_var_int (        {get next funct parameter as integer variable}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     sym_p: escr_sym_p_t;         {pointer to integer variable symbol descriptor}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {term was available}
  val_param;

var
  name: string_var80_t;

begin
  name.max := size_char(name.str);     {init local var string}
  escr_ifn_get_var_int := false;       {init to not returning with integer variable}

  if not escr_ifn_get_name (e, name, stat)
    then return;

  escr_sym_find_type (                 {try to find the variable}
    e,                                 {ESCR library use state}
    name,                              {name of symbol to find}
    escr_sytype_var_k,                 {symbol must be a variable}
    sym_p,                             {returned point to symbol descriptor}
    stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {no such variable ?}
    sys_stat_set (escr_subsys_k, escr_err_var_nfound_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;
  if sym_p^.var_val.dtype <> escr_dtype_int_k then begin {variable is not integer ?}
    sys_stat_set (escr_subsys_k, escr_err_var_not_int_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;

  escr_ifn_get_var_int := true;        {indicate returning with integer variable}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_BOOL (E, B)
*
*   Add the boolean value B to the function return string.
}
procedure escr_ifn_ret_bool (          {return boolean value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      b: boolean);                 {the boolean value to return}
  val_param;

begin
  if b
    then string_appendn (e.parse_p^.funret, 'TRUE', 4)
    else string_appendn (e.parse_p^.funret, 'FALSE', 5);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_INT (E, II)
*
*   Add the integer value II to the function return string.
}
procedure escr_ifn_ret_int (           {return integer value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      ii: sys_int_max_t);          {the integer value to return}
  val_param;

var
  tk: string_var32_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  string_f_int_max (tk, ii);           {make string representation}
  string_append (e.parse_p^.funret, tk); {append it to function return string}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_FP (E, FP)
*
*   Add the floating point value FP to the function return string.
}
procedure escr_ifn_ret_fp (            {return floating point value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fp: sys_fp_max_t);           {the floating point value to return}
  val_param;

var
  tk: string_var32_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_str_from_fp (e, fp, tk);        {make string representation}
  string_append (e.parse_p^.funret, tk); {append it to function return string}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_STR (E, STR)
*
*   Add the string value STR to the function return string.
}
procedure escr_ifn_ret_str (           {return string value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {the string value to return}
  val_param;

begin
  escr_str_quote (e, str, e.parse_p^.funret); {quote and append the string}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_STRP (E, STRP)
*
*   Add the Pascal string STRP to the funtion return string.
}
procedure escr_ifn_ret_strp (          {return value from Pascal string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      strp: string);               {the string value to return}
  val_param;

var
  tk: string_var80_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  string_vstring (tk, strp, 80);       {convert to var string}
  escr_str_quote (e, tk, e.parse_p^.funret); {quote and append the string}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_CHARS (E, STR)
*
*   Add the raw characters of STR to the return string.  These will not be
*   returned as a string.
}
procedure escr_ifn_ret_chars (         {return raw characters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {the characters to return}
  val_param;

begin
  string_append (e.parse_p^.funret, str);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_CHARSP (E, STRP)
*
*   Add the raw characters of STRP to the return string.  These will not be
*   returned as a string.  STRP is a Pascal string, not a var string.
}
procedure escr_ifn_ret_charsp (        {return raw characters from Pascal string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      strp: string);               {the characters to return}
  val_param;

begin
  string_appends (e.parse_p^.funret, strp);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_CHAR (E, C)
*
*   Add the string containing the single character C to the return string.
}
procedure escr_ifn_ret_char (          {return one-character string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      c: char);                    {the single character of the string}
  val_param;

var
  qsyn_p: escr_quotesyn_p_t;           {points to quoted string syntax description}

begin
  qsyn_p := e.quotesyn_p;              {init to first quoted string type}

  if qsyn_p = nil then begin           {no quoted strings defined ?}
    string_append1 (e.parse_p^.funret, c); {add just the bare character}
    return;
    end;

  string_append1 (e.parse_p^.funret, qsyn_p^.st); {write quote start char}
  string_append1 (e.parse_p^.funret, c); {write the character}
  if c = qsyn_p^.en then begin         {character is same as quote end ?}
    string_append1 (e.parse_p^.funret, c); {double will be interpreted as literal single}
    end;
  string_append1 (e.parse_p^.funret, qsyn_p^.en); {write quote end char}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_EMPTY (E)
*
*   Add the empty string to the return string.
}
procedure escr_ifn_ret_empty (         {return the empty string}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  string_append1 (e.parse_p^.funret, '"');
  string_append1 (e.parse_p^.funret, '"');
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_TIME (E, T)
*
*   Add the time value T to the function return string.
}
procedure escr_ifn_ret_time (          {return time value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      t: sys_clock_t);             {the time value to return}
  val_param;

var
  tk: string_var32_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_str_from_time (e, t, tk);       {make string representation}
  string_append (e.parse_p^.funret, tk); {append it to function return string}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_RET_VAL (E, VAL)
*
*   Add the value VAL to the function return string.
}
procedure escr_ifn_ret_val (           {return arbitrary value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t);            {the value to return}
  val_param;

begin
  case val.dtype of
escr_dtype_bool_k: begin
      escr_ifn_ret_bool (e, val.bool);
      end;
escr_dtype_int_k: begin
      escr_ifn_ret_int (e, val.int);
      end;
escr_dtype_fp_k: begin
      escr_ifn_ret_fp (e, val.fp);
      end;
escr_dtype_str_k: begin
      escr_ifn_ret_str (e, val.str);
      end;
escr_dtype_time_k: begin
      escr_ifn_ret_time (e, val.time);
      end;
otherwise
    escr_err_dtype_unimp (e, val.dtype, 'ESCR_IFN_RET_VAL');
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_STAT_REQUIRED (E, STAT)
*
*   Set STAT to indicate the most appropriate error due to a required parameter
*   being missing.  This routine is intended to be called after a routine that
*   gets a parameter, and when that routine did not return with the parameter.
*   This could be because the parmeter did not exist, or because of a hard
*   error.  In the latter case, STAT will indicate this error.
*
*   When STAT already indicates error, it is left unaltered.  When it indicates
*   no error, it is set to indicate missing parameter.
}
procedure escr_ifn_stat_required (     {set STAT according to missing required argument}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  if sys_error(stat) then return;      {STAT already indicates error ?}

  sys_stat_set (escr_subsys_k, escr_err_noparmfun_k, stat);
  sys_stat_parm_vstr (e.parse_p^.funame, stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_BAD_DTYPE (E, VAL, STAT)
*
*   Set STAT to indicate bad data type for the usage.  VAL is the value of the
*   term with the bad data type.
}
procedure escr_ifn_bad_dtype (         {set STAT for bad data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {value with the wrong data type}
  out     stat: sys_err_t);            {returned error status}
  val_param;

var
  tk: string_var80_t;                  {string representation of the term}

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_val_str (e, val, tk);           {make string from term}
  sys_stat_set (escr_subsys_k, escr_err_badtype_k, stat);
  sys_stat_parm_vstr (tk, stat);       {the offending term}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_IFN_BAD_KEYW (E, KEYW, STAT)
*
*   Set STAT do indicate a bad keyword parameter to a function.  KEYW is the bad
*   keyword.
}
procedure escr_ifn_bad_keyw (          {set STAT for bad keyword}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      keyw: univ string_var_arg_t; {the bad keyword or string}
  out     stat: sys_err_t);            {returned error status}
  val_param;

begin
  sys_stat_set (escr_subsys_k, escr_err_badparmfun_k, stat);
  sys_stat_parm_vstr (keyw, stat);
  sys_stat_parm_vstr (e.parse_p^.funame, stat);
  end;
