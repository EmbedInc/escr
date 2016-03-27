{   Collection of subroutines for getting command parameters.
}
module escr_get;
define get_token;
define get_tkraw;
define get_tkrawc;
define get_end;
define get_keyword;
define get_dtype;
define get_val;
define get_val_dtype;
define get_bool;
define get_int;
define get_time;
define get_fp;
define get_str;
define get_args_str;
%include '/cognivision_links/dsee_libs/pic/escr.ins.pas';
{
****************************************************************************
*
*   Function GET_TOKEN (TK)
*
*   Return TRUE and the next input line token in TK if one is available.
*   If not, the function returns FALSE and TK as the empty string.
}
function get_token (                   {get next input stream token}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

var
  stat: sys_err_t;

begin
  get_token := true;                   {init to returning with a token}
  string_token (e.ibuf, e.ip, e.lparm, stat); {try to get the next token}
  if not sys_error(stat) then begin    {got it ?}
    string_copy (e.lparm, tk);
    return;
    end;
  if string_eos(stat) then begin       {hit end of input line ?}
    get_token := false;                {indicate no token found}
    tk.len := 0;
    return;
    end;

  sys_error_print (stat, '', '', nil, 0); {abort with error}
  err_atline ('', '', nil, 0);
  return;                              {keep compiler from complaining}
  end;
{
********************************************************************************
*
*   Function GET_TKRAW (TK)
*
*   Get the next token without any interpretation.  The same syntax rules for
*   token delimiting still apply, but the token is not altered in any way.  If
*   the token is a quoted string, the quotes are not removed although internal
*   blanks are considered part of the token until the closing quote.  Quotes
*   can be either quote characters (") or apostrophies (').
}
function get_tkraw (                   {get next raw input stream token}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

var
  qchar: char;                         {string quote character}
  quote: boolean;                      {token is a quoted string}
  c: char;                             {current character}

begin
  get_tkraw := false;                  {init to no token available}
  tk.len := 0;

  while e.ip <= e.ibuf.len do begin    {skip over blanks at current position}
    if e.ibuf.str[e.ip] <> ' ' then exit; {at a non-blank ?}
    e.ip := e.ip + 1;                  {skip over this blank}
    end;
  if e.ip > e.ibuf.len then return;    {no token before end of input string ?}
  get_tkraw := true;                   {will be returning with a token}

  quote := false;                      {init to not within a quoted string}
  while e.ip <= e.ibuf.len do begin    {scan the remainder of the input string}
    c := e.ibuf.str[e.ip];             {fetch this character}
    if quote
      then begin                       {in a quoted string}
        string_append1 (tk, c);        {copy this char to returned token}
        if c = qchar then quote := false; {hit closed quote ?}
        end
      else begin                       {not in a quoted string}
        if c = ' ' then begin          {unquoted blank ends token}
          e.ip := e.ip + 1;            {skip over this blank}
          exit;
          end;
        string_append1 (tk, c);        {copy this char to returned token}
        if (c = '''') or (c = '"') then begin {leading quote ?}
          qchar := c;                  {set closing quote character}
          quote := true;               {indicate now in a quoted string}
          end;
        end
      ;
    e.ip := e.ip + 1;                  {make index of the next char}
    end;                               {back to process this new char}
  end;
{
********************************************************************************
*
*   Function GET_TKRAW (TK)
*
*   Like GET_TKRAW except that tokens are delimited by single commas surrounded
*   by any number of spaces.
}
function get_tkrawc (                  {get next raw token, comma delimited}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param;

var
  qchar: char;                         {string quote character}
  quote: boolean;                      {token is a quoted string}
  c: char;                             {current character}
  blcnt: sys_int_machine_t;            {number of blank characters skipped over}

begin
  get_tkrawc := false;                 {init to no token available}
  tk.len := 0;

  while e.ip <= e.ibuf.len do begin    {skip over blanks at current position}
    if e.ibuf.str[e.ip] <> ' ' then exit; {at a non-blank ?}
    e.ip := e.ip + 1;                  {skip over this blank}
    end;
  if e.ip > e.ibuf.len then return;    {no token before end of input string ?}
  get_tkrawc := true;                  {will be returning with a token}

  quote := false;                      {init to not within a quoted string}
  blcnt := 0;                          {init to no blanks skipped over}
  while e.ip <= e.ibuf.len do begin    {scan the remainder of the input string}
    c := e.ibuf.str[e.ip];             {fetch this character}
    e.ip := e.ip + 1;                  {update index to next character to fetch}
    if quote
      then begin                       {in a quoted string}
        string_append1 (tk, c);        {copy this char to returned token}
        if c = qchar then quote := false; {hit closed quote ?}
        end
      else begin                       {not in a quoted string}
        case c of                      {check for special handling characters}

'''', '"': begin                       {leading quote}
  quote := true;                       {indicate now in a quoted string}
  qchar := c;                          {set the end quote character}
  string_append1 (tk, c);              {copy current character to the returned token}
  end;

' ': begin                             {blank}
  blcnt := blcnt + 1;                  {count one more blank encountered}
  end;

',': begin                             {comma}
  return;                              {end of token delimiter found}
  end;

otherwise                              {all other characters}
  while blcnt > 0 do begin             {skipped blanks were internal to the token}
    string_append1 (tk, ' ');          {pass on this blank}
    blcnt := blcnt - 1;                {count one less skipped blank left to restore}
    end;
  string_append1 (tk, c);              {copy current character to the returned token}
  end;
        end                            {end of not in quoted string case}
      ;
    end;                               {back to process this new char}
  end;
{
****************************************************************************
*
*   Subroutine GET_END
*
*   Abort with error indicating the current input line if the input line
*   was not exhausted.
}
procedure get_end;                     {make sure no more tokens left on input line}
  val_param;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  tk: string_var80_t;                  {the unexpected token}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  if not get_token (tk) then return;   {hit end of line as expected ?}

  sys_msg_parm_vstr (msg_parm[1], e.cmd);
  sys_msg_parm_vstr (msg_parm[2], tk);
  err_atline ('pic', 'parm_extra', msg_parm, 2);
  end;
{
****************************************************************************
*
*   Subroutine GET_KEYWORD (KLIST, PICK)
*
*   Get the next token and compare it to the list of keywords in KLIST.
*   PICK is returned the 1-N number of the keyword matched by the token.
*   The token is case-insensitive.  PICK is returned 0 if no token was
*   available.  It is an error if a token is available but does not match
*   any of the keywords.
*
*   The keywords in KLIST must be upper case and separated by blanks.
}
procedure get_keyword (                {get one of list of keywords from input stream}
  in      klist: string;               {keywords, upper case separated by blanks}
  out     pick: sys_int_machine_t);    {1-N keyword number, 0 = no token available}
  val_param;

var
  tk: string_var80_t;                  {the unexpected token}

begin
  tk.max := size_char(tk.str);         {init local var string}

  if not get_token (tk) then begin     {no token available ?}
    pick := 0;
    return;
    end;

  string_upcase (tk);                  {make upper case for keyword matching}
  string_tkpick80 (tk, klist, pick);   {pick the token from the list}
  if pick <= 0 then begin              {token didn't match any keyword ?}
    err_parm_bad (tk);                 {abort with bad parameter to this command}
    end;
  end;
{
****************************************************************************
*
*   Function GET_DTYPE (DTYPE)
*
*   Get the next token and return the ID of the data type it selects.
*   The function returns TRUE if a data type was selected and FALSE
*   if no token was available.  The program is aborted on error with
*   an appropriate message if the token does not select a data type.
}
function get_dtype (                   {get data type ID from input stream}
  out     dtype: dtype_k_t)            {returned data type ID}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  pick: sys_int_machine_t;             {number of token picked from list}

begin
  get_dtype := false;                  {init to no token available}
  get_keyword ('BOOL INTEGER REAL STRING TIME', pick); {get the selected keyword}
  case pick of
0:  return;
1:  dtype := dtype_bool_k;
2:  dtype := dtype_int_k;
3:  dtype := dtype_fp_k;
4:  dtype := dtype_str_k;
5:  dtype := dtype_time_k;
otherwise
    writeln ('Internal screwup in program ESCR.  Unexpected PICK value');
    writeln ('in GET_DTYPE.');
    err_atline ('', '', nil, 0);
    end;
  get_dtype := true;                   {indicate returning with a data type}
  end;
{
****************************************************************************
*
*   Function GET_VAL (VAL)
*
*   Get the next input term and returns its value in VAL.  The function
*   returns TRUE if an input term was available, FALSE otherwise.
}
function get_val (                     {get value of next input stream token}
  out     val: val_t)                  {returned value}
  :boolean;                            {TRUE if token available}
  val_param;

begin
  get_val := term_get (e.ibuf, e.ip, val);
  end;
{
****************************************************************************
*
*   Function GET_VAL_DTYPE (VAL)
*
*   Get the next input term value into VAL.  The value will be converted
*   to the data type indicated by VAL.DTYPE.  The program will bomb with
*   error if conversion is not possible.
*
*   The function returns TRUE if an input term was available, FALSE
*   otherwise.
}
function get_val_dtype (               {get next input value, to dtype of VAL}
  in out  val: val_t)                  {returned value, DTYPE specifies data type}
  :boolean;                            {TRUE if token available}
  val_param;

var
  v: val_t;

begin
  get_val_dtype := false;              {init to no term available}
  if not get_val (v) then return;      {get the term in its native format}
  get_val_dtype := true;               {indicate term was available}
  val_copy (v, val);                   {copy and convert term to returned value}
  end;
{
****************************************************************************
*
*   Function GET_BOOL (B)
*
*   Get the next token and return its boolean value in B.  The function
*   returns TRUE if a token was available.  If no token is available,
*   the function returns FALSE and I is unaltered.  The program is aborted
*   with appropriate error message if the token can not be converted
*   to a boolean value.
}
function get_bool (                    {get the next token as a boolean}
  out     b: boolean)                  {returned boolean value}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  get_bool := false;                   {init to no term available}
  if not get_val (val) then return;    {no input term ?}
  get_bool := true;
  b := val_bool (val);                 {pass back term boolean value}
  end;
{
****************************************************************************
*
*   Function GET_INT (I)
*
*   Get the next token and return its integer value in I.  The function
*   returns TRUE if a token was available.  If no token is available,
*   the function returns FALSE and I is unaltered.  The program is aborted
*   with appropriate error message if the token can not be converted
*   to an integer.
}
function get_int (                     {get the next token as an integer}
  out     i: sys_int_max_t)            {returned integer value}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  get_int := false;                    {init to no term available}
  if not get_val (val) then return;    {no input term ?}
  get_int := true;
  i := val_int (val);                  {pass back term integer value}
  end;
{
****************************************************************************
*
*   Function GET_TIME (TIME)
*
*   Get the next token and return its time value in TIME.  The function
*   returns TRUE if a token was available.  If no token is available,
*   the function returns FALSE and TIME is unaltered.  The program is aborted
*   with appropriate error message if the token can not be converted
*   to a time.
}
function get_time (                    {get the next token as a time value}
  out     time: sys_clock_t)           {returned time value}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  get_time := false;                   {init to no term available}
  if not get_val (val) then return;    {no input term ?}
  get_time := true;
  time := val_time (val);              {pass back term time value}
  end;
{
****************************************************************************
*
*   Function GET_FP (FP)
*
*   Get the next token and return its floating point value in FP.  The function
*   returns TRUE if a token was available.  If no token is available,
*   the function returns FALSE and FP is unaltered.  The program is aborted
*   with appropriate error message if the token can not be converted
*   to a floating point value.
}
function get_fp (                      {get next token as floating point value}
  out     fp: sys_fp_max_t)            {returned floating point value}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  get_fp := false;                     {init to no term available}
  if not get_val (val) then return;    {no input term ?}
  get_fp := true;
  fp := val_fp (val);                  {pass back term floating point value}
  end;
{
****************************************************************************
*
*   Function GET_STR (STR)
*
*   Get the next token and return its string value in STR.  The function
*   returns TRUE if a token was available.  If no token is available,
*   the function returns FALSE and STR is unaltered.  All possible data
*   types have a string representation, so this routine will not fail
*   due to incompatible token data type.
}
function get_str (                     {get the next token as a string}
  in out  str: univ string_var_arg_t)  {returned string}
  :boolean;                            {TRUE if input token was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  get_str := false;                    {init to no term available}
  if not get_val (val) then return;    {no input term ?}
  get_str := true;
  val_str (val, str);                  {pass back string value}
  end;
{
****************************************************************************
*
*   Subroutine GET_ARGS_STR (STR)
*
*   Get the concatenation of the string represenation of all remaining arguments.
*   The input line is guaranteed to be exhausted after this call.
}
procedure get_args_str (               {get string representation of remaining parameters}
  in out  str: univ string_var_arg_t); {concatenation of remaining args converted to strings}
  val_param;

var
  tk: string_var8192_t;                {string representation of one argument}

label
  loop;

begin
  tk.max := size_char(tk.str);         {init local var string}

  str.len := 0;                        {init return string to empty}

loop:                                  {back here each new input argument}
  if not get_str(tk) then return;      {get string representation of next argument}
  string_append (str, tk);             {append it to end of return string}
  goto loop;                           {back to get next argument}
  end;
