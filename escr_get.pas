{   Collection of subroutines for getting command parameters.
}
module escr_get;
define escr_get_token;
define escr_get_pos_save;
define escr_get_pos_restore;
define escr_get_tkraw;
define escr_get_tkrawc;
define escr_get_end;
define escr_get_keyword;
define escr_get_dtype;
define escr_get_val;
define escr_get_val_dtype;
define escr_get_bool;
define escr_get_int;
define escr_get_time;
define escr_get_fp;
define escr_get_str;
define escr_get_args_str;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Function ESCR_GET_TOKEN (E, TK)
*
*   Try to get the next input line token.  If one it available, it is returned
*   in TK and the function returns TRUE.  If not, TK is returned the empty
*   string and the function returns FALSE.
}
function  escr_get_token (             {get next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

var
  gotit: boolean;                      {got a token}
  quoted: boolean;                     {the token was a quoted string}
  stat: sys_err_t;

begin
  gotit := escr_term_parse (           {parse next token from input string}
    e,                                 {ESCR library use state}
    e.parse_p^.ibuf,                   {string to parse token from}
    e.parse_p^.ip,                     {parse index}
    e.parse_p^.lparm,                  {returned token}
    quoted,                            {token was a quoted string}
    stat);
  if gotit then begin                  {got a token, no error ?}
    string_copy (e.parse_p^.lparm, tk); {return the token into TK}
    escr_get_token := true;            {indicate returning with a token}
    return;
    end;

  escr_get_token := false;             {not returning with a token}
  if sys_error(stat) then begin        {hard error ?}
    sys_error_print (stat, '', '', nil, 0); {show the error}
    escr_err_atline (e, '', '', nil, 0); {show source location and abort}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_GET_POS_SAVE (E, POS)
*
*   Save the current command line parsing position in POS.
}
procedure escr_get_pos_save (          {save current command line parsing position}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     pos: escr_cmdpos_t);         {saved parsing position}
  val_param;

begin
  pos.p := e.parse_p^.ip;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_GET_POS_RESTORE (E, POS)
*
*   Restore the command line parsing position to the one previously saved in
*   POS.
}
procedure escr_get_pos_restore (       {restore to previously saved parsing position}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      pos: escr_cmdpos_t);         {saved parsing position to go back to}
  val_param;

begin
  e.parse_p^.ip := pos.p;
  end;
{
********************************************************************************
*
*   Function ESCR_GET_TKRAW (E, TK)
*
*   Get the next token without any interpretation.  The same syntax rules for
*   token delimiting still apply, but the token is not altered in any way.  If
*   the token is a quoted string, the quotes are not removed although internal
*   blanks are considered part of the token until the closing quote.
}
function escr_get_tkraw (              {get next raw input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

begin
  escr_get_tkraw := escr_term_raw (    {parse and get the raw token}
    e,                                 {ESCR library use state}
    e.parse_p^.ibuf,                   {source string}
    e.parse_p^.ip,                     {source string parse index}
    tk);                               {returned token}
  end;
{
********************************************************************************
*
*   Function ESCR_GET_TKRAWC (E, TK)
*
*   Like GET_TKRAW except that tokens are delimited by single commas surrounded
*   by any number of spaces.
}
function escr_get_tkrawc (             {get next raw token, comma delimited}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

begin
  escr_get_tkrawc := escr_term_rawc (  {parse and get the raw token}
    e,                                 {ESCR library use state}
    e.parse_p^.ibuf,                   {source string}
    e.parse_p^.ip,                     {source string parse index}
    tk);                               {returned token}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_GET_END (E, STAT)
*
*   Returns TRUE iff there are no more tokens on the current input line.  If a
*   token is found, STAT is set to indicate extra parameters, and includes the
*   extra parameter.  The extra parameter is also saved in E.PARSE_P^.LPARM.
}
function escr_get_end (                {check for no more tokens on input line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {set to error if token found}
  :boolean;                            {no more tokens on the input line}
  val_param;

var
  tk: string_var80_t;                  {the unexpected token}

begin
  tk.max := size_char(tk.str);         {init local var string}
  escr_get_end := false;               {init to token found}

  if escr_get_token (e, tk) then begin {found a token ?}
    sys_stat_set (escr_subsys_k, escr_err_extra_k, stat);
    sys_stat_parm_vstr (tk, stat);
    return;
    end;

  escr_get_end := true;                {indicate was at end of line}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_GET_KEYWORD (E, KLIST, PICK, STAT)
*
*   Get the next token and compare it to the list of keywords in KLIST.
*
*   PICK is returned the 1-N number of the keyword matched by the token.  PICK
*   is 0 when no token was available, and -1 when a token was available but did
*   not match any of the choices.  In the latter case, STAT is set to "bad
*   parameter" error with the token.
*
*   The keywords in KLIST must be upper case and separated by blanks.  The token
*   will be processed as being case-insensitive.
}
procedure escr_get_keyword (           {get one of list of keywords from input stream}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      klist: string;               {keywords, upper case separated by blanks}
  out     pick: sys_int_machine_t;     {1-N keyword number, 0 = no token, -1 = no match}
  out     stat: sys_err_t);            {completion status, no match or hard error}
  val_param;

var
  tk: string_var80_t;                  {the unexpected token}

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error encountered}

  if not escr_get_token (e, tk) then begin {no token available ?}
    pick := 0;
    return;
    end;

  string_upcase (tk);                  {make upper case for keyword matching}
  string_tkpick80 (tk, klist, pick);   {pick the token from the list}
  if pick <= 0 then begin              {token didn't match any keyword ?}
    pick := -1;                        {indicate no match}
    sys_stat_set (escr_subsys_k, escr_err_badparm_k, stat);
    sys_stat_parm_vstr (tk, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_GET_DTYPE (E, DTYPE, STAT)
*
*   Get the next token and return the ID of the data type it selects.  The
*   function returns TRUE if a data type was selected and FALSE with DTYPE not
*   altered if no token was available.  The function always returns FALSE on
*   error.
}
function escr_get_dtype (              {get next parameter as data type ID}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     dtype: escr_dtype_k_t;       {returned data type ID}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  pick: sys_int_machine_t;             {number of token picked from list}

begin
  escr_get_dtype := false;             {init to no token available}

  escr_get_keyword (e, 'BOOL INTEGER REAL STRING TIME', pick, stat);

  case pick of
-1: begin                              {error}
      sys_stat_set (escr_subsys_k, escr_err_baddtype_k, stat);
      sys_stat_parm_vstr (e.parse_p^.lparm, stat);
      return;
      end;
0:  return;                            {no token}
1:  dtype := escr_dtype_bool_k;
2:  dtype := escr_dtype_int_k;
3:  dtype := escr_dtype_fp_k;
4:  dtype := escr_dtype_str_k;
5:  dtype := escr_dtype_time_k;
otherwise
    writeln ('Internal error in program ESCR: Unexpected PICK value');
    writeln ('in GET_DTYPE.');
    escr_err_atline (e, '', '', nil, 0);
    end;

  escr_get_dtype := true;              {indicate returning with a data type}
  end;
{
********************************************************************************
*
*   Function ESCR_GET_VAL (E, VAL, STAT)
*
*   Get the next input term and return its value in VAL.  The function returns
*   TRUE if an input term was available, FALSE otherwise.
}
function escr_get_val (                {get value of next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t;             {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if token available}
  val_param;

begin
  escr_get_val := escr_term_val (e, e.parse_p^.ibuf, e.parse_p^.ip, val, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_GET_VAL_DTYPE (E, VAL, STAT)
*
*   Get the next input term value into VAL.  The value will be converted to the
*   data type indicated by VAL.DTYPE.
*
*   The function returns TRUE if an input term was available, FALSE otherwise.
*   The function value is always FALSE when returning with error.
}
function escr_get_val_dtype (          {get next input value, to dtype of VAL}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  val: escr_val_t;             {returned value, DTYPE specifies data type}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if token available}
  val_param;

var
  v: escr_val_t;

begin
  escr_get_val_dtype := false;         {init to no term available}

  if not escr_get_val (e, v, stat) then return; {get the term in its native format}
  escr_val_copy (e, v, val, stat);     {copy and convert term to returned value}
  if sys_error(stat) then return;      {couldn't convert to target data type ?}
  escr_get_val_dtype := true;          {indicate term was available}
  end;
{
********************************************************************************
*
*   Function ESCR_GET_BOOL (E, B, STAT)
*
*   Get the next token and return its boolean value in B.  The function returns
*   TRUE if a token was available.  If no token is available, the function
*   returns FALSE.  The function value is always FALSE when returning with
*   error.
}
function escr_get_bool (               {get next parameter as boolean}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean;                  {returned boolean value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_get_bool := false;              {init to no term available}

  if not escr_get_val (e, val, stat) then return; {no input term ?}
  b := escr_val_bool (e, val, stat);   {pass back term boolean value}
  if sys_error(stat) then return;      {couldn't convert to target data type ?}
  escr_get_bool := true;               {indicate returning with value}
  end;
{
********************************************************************************
*
*   Function ESCR_GET_INT (E, I, STAT)
*
*   Get the next token and return its integer value in I.  The function returns
*   TRUE if a token was available.  If no token is available, the function
*   returns FALSE and I is not altered.  The function always returns FALSE on
*   error.
}
function escr_get_int (                {get next parameter as an integer}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     i: sys_int_max_t;            {returned integer value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: escr_val_t;                     {term value}
  ii: sys_int_max_t;

begin
  escr_get_int := false;               {init to not returning with value}
  if not escr_get_val (e, val, stat) then return; {no input term or error ?}

  ii := escr_val_int (e, val, stat);   {try to get integer value}
  if not sys_error(stat) then begin    {success ?}
    i := ii;                           {pass back the result}
    escr_get_int := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_GET_TIME (E, TIME, STAT)
*
*   Get the next token and return its time value in TIME.  The function returns
*   TRUE if a token was available.  If no token was available, the function
*   returns FALSE and TIME is unaltered.  The function always returns FALSE on
*   error.
}
function escr_get_time (               {get the next token as a time value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     time: sys_clock_t;           {returned time value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: escr_val_t;                     {term value}
  t: sys_clock_t;

begin
  escr_get_time := false;              {init to no term available}
  if not escr_get_val (e, val, stat) then return; {no input term ?}

  t := escr_val_time (e, val, stat);   {try to get time value}
  if not sys_error(stat) then begin    {success ?}
    time := t;                         {pass back the result}
    escr_get_time := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_GET_FP (E, FP, STAT)
*
*   Get the next token and return its floating point value in FP.  The function
*   returns TRUE if a token was available.  If no token is available, the
*   function returns FALSE and FP is unaltered.  The function always returns
*   FALSE on error.
}
function escr_get_fp (                 {get next parameter as floating point}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     fp: sys_fp_max_t;            {returned floating point value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: escr_val_t;                     {term value}
  r: sys_fp_max_t;

begin
  escr_get_fp := false;                {init to no term available}
  if not escr_get_val (e, val, stat) then return; {no input term or error ?}

  r := escr_val_fp (e, val, stat);     {try to get time value}
  if not sys_error(stat) then begin    {success ?}
    fp := r;                           {pass back the result}
    escr_get_fp := true;
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_GET_STR (E, STR, STAT)
*
*   Get the next token and return its string value in STR.  The function returns
*   TRUE if a token was available.  If no token is available, the function
*   returns FALSE and STR is unaltered.  The function always returns FALSE on
*   error.
}
function escr_get_str (                {get string representation of next parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned string}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: escr_val_t;                     {term value}

begin
  escr_get_str := false;               {init to no term available}
  if not escr_get_val (e, val, stat) then return; {no input term or error ?}

  escr_val_str (e, val, str);          {pass back string value}
  escr_get_str := true;                {indicate returning with value}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_GET_ARGS_STR (E, STR, STAT)
*
*   Get the concatenation of the string represenation of all remaining
*   arguments.  The input line is guaranteed to be exhausted after this call,
*   assuming no error.
}
procedure escr_get_args_str (          {get string representation of remaining parameters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {concatenation of remaining args converted to strings}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  tk: string_var8192_t;                {string representation of one argument}

label
  loop;

begin
  tk.max := size_char(tk.str);         {init local var string}
  str.len := 0;                        {init return string to empty}

loop:                                  {back here each new input argument}
  if not escr_get_str (e, tk, stat)    {get string representation of next argument}
    then return;
  string_append (str, tk);             {append it to end of return string}
  goto loop;                           {back to get next argument}
  end;
