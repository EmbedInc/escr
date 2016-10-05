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
define escr_ifn_ret_bool;
define escr_ifn_ret_int;
define escr_ifn_ret_fp;
define escr_ifn_ret_str;
define escr_ifn_ret_char;
define escr_ifn_ret_empty;
define escr_ifn_ret_time;
define escr_ifn_stat_required;
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
  escr_ifn_get_val := escr_term_get (e, e.funarg.s, e.funarg.p, val, stat);
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

begin
  escr_ifn_get_keyw := false;          {init to not returning with a keyword}

  string_token (e.funarg.s, e.funarg.p, keyw, stat); {get the next parameter}
  if string_eos(stat) then begin       {there was no parameter ?}
    return;
    end;
  if sys_error(stat) then return;      {return with error ?}

  string_upcase (keyw);                {convert keyword to all upper case}
  escr_ifn_get_keyw := true;           {indicate returning with a keyword}
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
    then string_appendn (e.funret, 'TRUE', 4)
    else string_appendn (e.funret, 'FALSE', 5);
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
  string_append (e.funret, tk);        {append it to function return string}
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
  string_append (e.funret, tk);        {append it to function return string}
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

var
  i: sys_int_machine_t;                {string index}
  c: char;                             {scratch character}
  q: char;                             {" or ' quote character to use}

begin
  q := '"';                            {init to default quote char}
  for i := 1 to str.len do begin       {scan the input string}
    c := str.str[i];                   {get this input string character}
    if c = '''' then begin             {found apostrophy in string ?}
      q := '"';                        {use quotes}
      exit;                            {no point scanning further}
      end;
    if c = '"' then q := '''';         {use apostrophies if string contains quotes}
    end;
{
*   Q is the string quoting character to use.
}
  string_append1 (e.funret, q);        {write leading quote}
  for i := 1 to str.len do begin       {once for each string character}
    c := str.str[i];                   {get this string character}
    if c = q then begin                {this is quote character ?}
      string_append1 (e.funret, c);    {write quote character twice}
      end;
    string_append1 (e.funret, c);      {write string character}
    end;
  string_append1 (e.funret, q);        {write closing quote}
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
  q: char;                             {quote character}

begin
  q := '"';                            {init to using double quotes}
  if c = q then begin                  {the character is a double quote ?}
    q := '''';                         {use single quote instead}
    end;

  string_append1 (e.funret, q);        {write the quoted string}
  string_append1 (e.funret, c);
  string_append1 (e.funret, q);
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
  string_append1 (e.funret, '"');
  string_append1 (e.funret, '"');
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
  string_append (e.funret, tk);        {append it to function return string}
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
  sys_stat_parm_vstr (e.funame, stat);
  end;
