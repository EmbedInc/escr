{   Module that contains intrinsic function routines.  These are the functions
*   that are pre-defined and implemented with compiled code.  Some large
*   intrinsic function routines are in their own modules, called
*   ESCR_IFUN_<name>.  The small intrinsic function routines are collected here.
*
*   All the intrinsic function routines have this interface:
*
*      escr_ifun_<name> (E, STAT)
*
*   The function invocation is in E.PARSE_P^.FUNARG.  This is the string
*   starting with the function name and including the function arguments.  It
*   does not contain whatever syntax was used to indicate the start and end of
*   the function invocation.  The parse index in FUNARG is set to the next
*   character after the delimiter after the function name.  In other words, it
*   is ready to parse the first parameter.  There is no guarantee there are any
*   parameters.  If there are no parameters, then the parse index will be past
*   the end of the string.
*
*   The function routine must write the expansion of the function to
*   E.PARSE_P^.FUNRET.  This string has been initialized to empty.
*
*   STAT has been initialized to indicate no error.  If a error is encountered,
*   STAT must be set accordingly.
}
module escr_ifun;
define escr_ifun_str;
define escr_ifun_chars;
define escr_ifun_qstr;
define escr_ifun_char;
define escr_ifun_ccode;
define escr_ifun_slen;
define escr_ifun_substr;
define escr_ifun_sindx;
define escr_ifun_ucase;
define escr_ifun_lcase;
define escr_ifun_int;
define escr_ifun_fp;
define escr_ifun_eng;
define escr_ifun_shiftr;
define escr_ifun_shiftl;
define escr_ifun_not;
define escr_ifun_inv;
define escr_ifun_and;
define escr_ifun_or;
define escr_ifun_xor;
define escr_ifun_plus;
define escr_ifun_minus;
define escr_ifun_postdec;
define escr_ifun_postinc;
define escr_ifun_predec;
define escr_ifun_preinc;
define escr_ifun_times;
define escr_ifun_divide;
define escr_ifun_div;
define escr_ifun_trunc;
define escr_ifun_rnd;
define escr_ifun_max;
define escr_ifun_min;
define escr_ifun_abs;
define escr_ifun_sqrt;
define escr_ifun_sin;
define escr_ifun_cos;
define escr_ifun_tan;
define escr_ifun_pi;
define escr_ifun_e;
define escr_ifun_exp;
define escr_ifun_log;
define escr_ifun_log2;
define escr_ifun_rdeg;
define escr_ifun_degr;
define escr_ifun_lt;
define escr_ifun_le;
define escr_ifun_eq;
define escr_ifun_ne;
define escr_ifun_ge;
define escr_ifun_gt;
define escr_ifun_now;
define escr_ifun_date;
define escr_ifun_if;
define escr_ifun_arg;
define escr_ifun_sym;
define escr_ifun_exist;
define escr_ifun_tnam;
define escr_ifun_lnam;
define escr_ifun_dir;
define escr_ifun_evar;
define escr_ifun_lab;
define escr_ifun_v;
define escr_ifun_seq;
define escr_ifun_preinc;
define escr_ifun_postinc;
define escr_ifun_predec;
define escr_ifun_postdec;
define escr_ifun_unquote;
define escr_ifun_isint;
define escr_ifun_isnum;
define escr_ifun_vnl;
define escr_ifun_runex;
define escr_ifun_runtf;
define escr_ifun_runso;
define escr_ifun_file;
define escr_ifun_qtk;
%include 'escr2.ins.pas';

const
{
*   Physical constants.  Don't mess with these.
}
  pi = 3.14159265358979323846;         {what it sounds like, don't touch}
  ek = 2.718281828;                    {ditto}
  ln2 = ln(2.0);                       {natural log of 2}

type
  th_read_p_t = ^th_read_t;
  th_read_t = record                   {data for threads that read from I/O units}
    e_p: escr_p_t;                     {pointer to ESCR library use state}
    sysconn: sys_sys_file_conn_t;      {handle to system I/O stream}
    thid: sys_sys_thread_id_t;         {ID of this thread}
    run: boolean;                      {the thread was launced successfully}
    end;
{
********************************************************************************
*
*   STR arg ... arg
*
*   The concatention of the string representation of all the arguments returned
*   as a string.
}
procedure escr_ifun_str (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var8192_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_ifn_get_strs (e, tk, stat);     {get the concatenated string}
  if sys_error(stat) then return;

  escr_ifn_ret_str (e, tk);            {return the string}
  end;
{
********************************************************************************
*
*   CHARS arg ... arg
*
*   The concatention of the string representation of all the arguments, but
*   returned as raw characters, not a string constant.
}
procedure escr_ifun_chars (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var8192_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_ifn_get_strs (e, tk, stat);     {get the concatenated string}
  if sys_error(stat) then return;

  escr_ifn_ret_chars (e, tk);          {return the raw characters}
  end;
{
********************************************************************************
*
*   QSTR <string>
*
*   Get all the characters starting one space after the function name and to the
*   end of the function invocation, and return the result as a string.
}
procedure escr_ifun_qstr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var string}

  repeat                               {scan back to find the last function name char}
    e.parse_p^.funarg.p := e.parse_p^.funarg.p - 1;
    until e.parse_p^.funarg.s.str[e.parse_p^.funarg.p] <> ' ';
  e.parse_p^.funarg.p := e.parse_p^.funarg.p + 2; {go to first raw string character}

  str.len := 0;                        {init function parameter characters}
  while                                {copy all characters to STR}
      e.parse_p^.funarg.p <= e.parse_p^.funarg.s.len
      do begin
    string_append1 (                   {copy this char}
      str, e.parse_p^.funarg.s.str[e.parse_p^.funarg.p]);
    e.parse_p^.funarg.p := e.parse_p^.funarg.p + 1; {advance to next character}
    end;
  escr_ifn_ret_str (e, str);           {return the characters as a string}
  end;
{
********************************************************************************
*
*   CHAR [ccode ... ccode]
*
*   Returns the string of the characters with the indicated character codes.
}
procedure escr_ifun_char (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  ccode: sys_int_max_t;                {character code}
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var strings}

  str.len := 0;                        {init the returned string to empty}

  while true do begin
    if not escr_ifn_get_int (e, ccode, stat) then begin {get character code into CCODE}
      if sys_error(stat) then return;
      exit;
      end;
    string_append1 (str, chr(ccode));  {append this char to end of return string}
    end;

  escr_ifn_ret_str (e, str);           {return the result}
  end;
{
********************************************************************************
*
*   CCODE char
*
*   Returns the integer characer code of the character CHAR.  CHAR must be a
*   string that is exactly one character long.
}
procedure escr_ifun_ccode (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var4_t;

begin
  str.max := size_char(str.str);       {init local var string}

  if not escr_ifn_get_str (e, str, stat) then begin {get CHAR into STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if str.len <> 1 then begin
    sys_stat_set (escr_subsys_k, escr_err_not1char_k, stat);
    sys_stat_parm_vstr (str, stat);
    return;
    end;

  escr_ifn_ret_int (e, ord(str.str[1])); {pass back result}
  end;
{
********************************************************************************
*
*   SLEN arg
*
*   Returns the number of characters in the string representation of ARG.
}
procedure escr_ifun_slen (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var string}

  if not escr_ifn_get_str (e, str, stat) then begin {get CHAR into STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ifn_ret_int (e, str.len);       {pass back result}
  end;
{
********************************************************************************
*
*   SUBSTR st ln str
*
*   Returns the substring of STR that starts at character position ST and
*   extends for LN characters.
}
procedure escr_ifun_substr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  st: sys_int_max_t;                   {start index of substring}
  ln: sys_int_max_t;                   {length of substring}
  ii: sys_int_machine_t;               {loop counter}
  str: string_var8192_t;
  substr: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var strings}
  substr.max := size_char(substr.str);

  if not escr_ifn_get_int (e, st, stat) then begin {get ST}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_int (e, ln, stat) then begin {get LN}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_str (e, str, stat) then begin {get STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  substr.len := 0;                     {init the substring to empty}
  ln := st + ln - 1;                   {make ending index instead of length}
  for ii := st to ln do begin          {scan the part of the input string to extract}
    if (ii < 1) or (ii > str.len) then next; {no character to extract here ?}
    string_append1 (substr, str.str[ii]); {add this char to substring}
    end;

  escr_ifn_ret_str (e, substr);        {return the extracted substring}
  end;
{
********************************************************************************
*
*   SINDX ind str
*
*   Returns the string containing only the character at index IND of STR.
}
procedure escr_ifun_sindx (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  ind: sys_int_max_t;                  {string index}
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var strings}

  if not escr_ifn_get_int (e, ind, stat) then begin {get IND}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_str (e, str, stat) then begin {get STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if (ind > 0) and (ind <= str.len)
    then begin                         {there is a character to return}
      escr_ifn_ret_char (e, str.str[ind]); {return the character}
      end
    else begin                         {indexed character is outside the string}
      escr_ifn_ret_empty (e);          {return the empty string}
      end
    ;
  end;
{
********************************************************************************
*
*   UCASE str
*
*   Returns the string STR with all alphabetic characters upper case.
}
procedure escr_ifun_ucase (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var strings}

  if not escr_ifn_get_str (e, str, stat) then begin {get STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  string_upcase (str);                 {convert to upper case}

  escr_ifn_ret_str (e, str);           {return the string}
  end;
{
********************************************************************************
*
*   LCASE
*
*   Returns the string STR with all alphabetic characters lower case.
}
procedure escr_ifun_lcase (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var8192_t;

begin
  str.max := size_char(str.str);       {init local var strings}

  if not escr_ifn_get_str (e, str, stat) then begin {get STR}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  string_downcase (str);               {convert to lower case}

  escr_ifn_ret_str (e, str);           {return the string}
  end;
{
********************************************************************************
*
*   INT int fmt ... fmt
*
*   Converts the integer INT to a string according to optional formatting rules.
*   The FMT parameters are optional, and are strings that contain formatting
*   commands.  Multiple formatting commands may be in one FMT parameter.
*   Multiple FMT parameters are interpreted as if there is a space between each
*   string.
*
*   See the header comments of subroutine ESCR_FORMAT_INT for a description of
*   the formatting commands.
}
procedure escr_ifun_int (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  ival: sys_int_max_t;                 {input integer value}
  fmt: string_var132_t;                {concatenated format string}
  tk: string_var80_t;                  {string representation of input value}

begin
  fmt.max := size_char(fmt.str);       {init local var strings}
  tk.max := size_char(tk.str);

  if not escr_ifn_get_int (e, ival, stat) then begin {get input value}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  fmt.len := 0;                        {init format string to empty}
  while true do begin                  {back here each new FMT parameter}
    if not escr_ifn_get_str (e, tk, stat) then begin {get this FMT parameter}
      if sys_error(stat) then return;
      exit;                            {end of FMT parameters}
      end;
    if (fmt.len > 0) and (fmt.str[fmt.len] <> ' ') then begin {add blank separator ?}
      string_append1 (fmt, ' ');
      end;
    string_append (fmt, tk);           {add this parm to end of format string}
    end;                               {back to get next FMT parameter}

  escr_format_int (                    {create string representation}
    e, ival, fmt, tk, stat);
  if sys_error(stat) then return;

  escr_ifn_ret_str (e, tk);            {return the resulting string}
  end;
{
********************************************************************************
*
*   FP fp fmt ... fmt
*
*   Converts the floating point value FP to a string according to optional
*   formatting rules.  The FMT parameters are optional, and are strings that
*   contain formatting commands.  Multiple formatting commands may be in one FMT
*   parameter.  Multiple FMT parameters are interpreted as if there is a space
*   between each string.
*
*   See the header comments of subroutine ESCR_FORMAT_FP for a description of
*   the formatting commands.
}
procedure escr_ifun_fp (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fpval: sys_fp_max_t;                 {the FP value}
  fmt: string_var132_t;                {concatenated format string}
  tk: string_var80_t;                  {string representation of input value}

begin
  fmt.max := size_char(fmt.str);       {init local var strings}
  tk.max := size_char(tk.str);

  if not escr_ifn_get_fp (e, fpval, stat) then begin {get input value}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  fmt.len := 0;                        {init format string to empty}
  while true do begin                  {back here each new FMT parameter}
    if not escr_ifn_get_str (e, tk, stat) then begin {get this FMT parameter}
      if sys_error(stat) then return;
      exit;                            {end of FMT parameters}
      end;
    if (fmt.len > 0) and (fmt.str[fmt.len] <> ' ') then begin {add blank separator ?}
      string_append1 (fmt, ' ');
      end;
    string_append (fmt, tk);           {add this parm to end of format string}
    end;                               {back to get next FMT parameter}

  escr_format_fp (                     {create string representation}
    e, fpval, fmt, tk, stat);
  if sys_error(stat) then return;

  escr_ifn_ret_str (e, tk);            {return the resulting string}
  end;
{
********************************************************************************
*
*   ENG val [sig [str]]
*
*   Returns the string representation of the numeric value VAL.  Engineering
*   notation with power of 1000 suffix letters will be used.  See ESCR
*   documatation for details.
}
procedure escr_ifun_eng (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fpval: sys_fp_max_t;                 {the input value}
  sig: sys_int_max_t;                  {SIG parameter}
  str: string_var80_t;                 {optional STR parameter}
  tk: string_var80_t;                  {result string}
  tk2: string_var4_t;                  {units scale name}

begin
  str.max := size_char(str.str);       {init local var strings}
  tk.max := size_char(tk.str);
  tk2.max := size_char(tk2.str);

  if not escr_ifn_get_fp (e, fpval, stat) then begin {get input value}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  sig := 3;                            {init to default number of digits}
  str.len := 1;                        {init to default separator string}
  str.str[1] := ' ';

  if escr_ifn_get_int (e, sig, stat) then begin
    discard( escr_ifn_get_str (e, str, stat) );
    end;
  if sys_error(stat) then return;

  string_f_fp_eng (tk, fpval, sig, tk2); {get number string and units}
  string_append (tk, str);             {append separator string}
  string_append (tk, tk2);             {append units scale name string}

  escr_ifn_ret_str (e, tk);            {return the resulting string}
  end;
{
********************************************************************************
*
*   SHIFTR val n
*
*   VAL shifted right N bits.
}
procedure escr_ifun_shiftr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: sys_int_max_t;
  n: sys_int_max_t;

begin
  if not escr_ifn_get_int (e, val, stat) then begin {get bits to shift}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_int (e, n, stat) then begin {get shift amount}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if n >= 0
    then val := rshft(val, n)
    else val := lshft(val, -n);

  escr_ifn_ret_int (e, val);           {return the result}
  end;
{
********************************************************************************
*
*   SHIFTL val n
*
*   VAL shifted left N bits.
}
procedure escr_ifun_shiftl (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: sys_int_max_t;
  n: sys_int_max_t;

begin
  if not escr_ifn_get_int (e, val, stat) then begin {get bits to shift}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_int (e, n, stat) then begin {get shift amount}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if n >= 0
    then val := lshft(val, n)
    else val := rshft(val, -n);

  escr_ifn_ret_int (e, val);           {return the result}
  end;
{
********************************************************************************
*
*   NOT b
*
*   Opposite of boolean B.
}
procedure escr_ifun_not (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  b: boolean;

begin
  if not escr_ifn_get_bool (e, b, stat) then begin {get the input value}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  b := not b;                          {apply the function logic}
  escr_ifn_ret_bool (e, b);            {return the result}
  end;
{
********************************************************************************
*
*   ~ int
*
*   Bitwise inversion (one's complement)
}
procedure escr_ifun_inv (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: sys_int_max_t;

begin
  if not escr_ifn_get_int (e, val, stat) then begin {get input value}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ifn_ret_int (e, ~val);          {return the result}
  end;
{
********************************************************************************
*
*   AND arg ... arg
*
*   Logical AND of all parameters when they are boolean.  Bitwise AND of all
*   parameters when they are integer.
}
procedure escr_ifun_and (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {value of first parameter}
  b: boolean;                          {boolean value}
  ii: sys_int_max_t;                   {integer value}

begin
  if not escr_ifn_get_val (e, val, stat) then begin
    escr_ifn_stat_required (e, stat);  {the first parameter is required}
    return;
    end;
  case val.dtype of                    {what is data type of first parameter ?}

escr_dtype_bool_k: begin               {BOOL}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_bool (e, b, stat) then exit;
    val.bool := val.bool and b;
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_bool (e, val.bool);     {return the final value}
  end;

escr_dtype_int_k: begin                {INTEGER}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_int (e, ii, stat) then exit;
    val.int := val.int & ii;
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_int (e, val.int);       {return the final value}
  end;

otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   OR arg ... arg
*
*   Logical OR of all parameters when they are boolean.  Bitwise OR of all
*   parameters when they are integer.
}
procedure escr_ifun_or (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {value of first parameter}
  b: boolean;                          {boolean value}
  ii: sys_int_max_t;                   {integer value}

begin
  if not escr_ifn_get_val (e, val, stat) then begin
    escr_ifn_stat_required (e, stat);  {the first parameter is required}
    return;
    end;
  case val.dtype of                    {what is data type of first parameter ?}

escr_dtype_bool_k: begin               {BOOL}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_bool (e, b, stat) then exit;
    val.bool := val.bool or b;
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_bool (e, val.bool);     {return the final value}
  end;

escr_dtype_int_k: begin                {INTEGER}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_int (e, ii, stat) then exit;
    val.int := val.int ! ii;
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_int (e, val.int);       {return the final value}
  end;

otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   XOR arg ... arg
*
*   Logical XOR of all parameters when they are boolean.  Bitwise XOR of all
*   parameters when they are integer.
}
procedure escr_ifun_xor (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {value of first parameter}
  b: boolean;                          {boolean value}
  ii: sys_int_max_t;                   {integer value}

begin
  if not escr_ifn_get_val (e, val, stat) then begin
    escr_ifn_stat_required (e, stat);  {the first parameter is required}
    return;
    end;
  case val.dtype of                    {what is data type of first parameter ?}

escr_dtype_bool_k: begin               {BOOL}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_bool (e, b, stat) then exit;
    val.bool := (val.bool and (not b)) or ((not val.bool) and b);
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_bool (e, val.bool);     {return the final value}
  end;

escr_dtype_int_k: begin                {INTEGER}
  while true do begin                  {loop over the remaining terms}
    if not escr_ifn_get_int (e, ii, stat) then exit;
    val.int := xor(val.int, ii);
    end;
  if sys_error(stat) then return;
  escr_ifn_ret_int (e, val.int);       {return the final value}
  end;

otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   + arg ... arg
*
*   Sum of all arguments.  All must be numeric, except one can be TIME.  Result
*   is integer, boolean, or time, depending on the arguments.  With a single
*   argument, the result is that argument.  When no arguments, the result is
*   integer 0.
}
procedure escr_ifun_plus (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  sumi: sys_int_max_t;                 {integer sum}
  sumf: sys_fp_max_t;                  {floating point sum}
  sumt: sys_clock_t;                   {time sum}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  dtype := escr_dtype_int_k;           {init to sum data type is integer}
  sumi := 0;                           {init the sum}

  while true do begin                  {loop over all the parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;
    case dtype of                      {what is the current data type ?}
{
*   Current sum data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT + INT}
      sumi := sumi + val.int;
      end;
escr_dtype_fp_k: begin                 {INT + FP}
      dtype := escr_dtype_fp_k;
      sumf := sumi + val.fp;
      end;
escr_dtype_time_k: begin               {INT + TIME}
      dtype := escr_dtype_time_k;
      sumt := sys_clock_add (val.time, sys_clock_from_fp_rel(sumi));
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current sum data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP + INT}
      sumf := sumf + val.int;
      end;
escr_dtype_fp_k: begin                 {FP + FP}
      sumf := sumf + val.fp;
      end;
escr_dtype_time_k: begin               {FP + TIME}
      dtype := escr_dtype_time_k;
      sumt := sys_clock_add (val.time, sys_clock_from_fp_rel(sumf));
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current data type is TIME.
}
escr_dtype_time_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {TIME + INT}
      sumt := sys_clock_add (sumt, sys_clock_from_fp_rel(val.int));
      end;
escr_dtype_fp_k: begin                 {TIME + FP}
      sumt := sys_clock_add (sumt, sys_clock_from_fp_rel(val.fp));
      end;
otherwise
    goto dtype_bad;
    end;
  end;

      end;                             {end of current data type cases}
    end;                               {back to get the next term}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, sumi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, sumf);
      end;
escr_dtype_time_k: begin               {TIME}
      escr_ifn_ret_time (e, sumt);
      end;
    end;
  return;
{
*   The data type of the current term is invalid.
}
dtype_bad:
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  end;
{
********************************************************************************
*
*   - arg1 [arg ... arg]
*
*   Subtraction of the first argument minus all the remaining arguments.  The
*   first argument is required.
}
procedure escr_ifun_minus (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  sumi: sys_int_max_t;                 {integer result}
  sumf: sys_fp_max_t;                  {floating point result}
  sumt: sys_clock_t;                   {time result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  case val.dtype of                    {what data type is this first argument ?}
escr_dtype_int_k: begin
      sumi := val.int;
      dtype := escr_dtype_int_k;
      end;
escr_dtype_fp_k: begin
      sumf := val.fp;
      dtype := escr_dtype_fp_k;
      end;
escr_dtype_time_k: begin
      sumt := val.time;
      dtype := escr_dtype_time_k;
      end;
otherwise
    goto dtype_bad;
    end;

  while true do begin                  {loop over all the remaining terms}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;
    case dtype of                      {what is the current data type ?}
{
*   Current sum data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT - INT}
      sumi := sumi - val.int;
      end;
escr_dtype_fp_k: begin                 {INT - FP}
      dtype := escr_dtype_fp_k;
      sumf := sumi - val.fp;
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current sum data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP - INT}
      sumf := sumf - val.int;
      end;
escr_dtype_fp_k: begin                 {FP - FP}
      sumf := sumf - val.fp;
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current data type is TIME.
}
escr_dtype_time_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {TIME - INT}
      sumt := sys_clock_add (sumt, sys_clock_from_fp_rel(-val.int));
      end;
escr_dtype_fp_k: begin                 {TIME - FP}
      sumt := sys_clock_add (sumt, sys_clock_from_fp_rel(-val.fp));
      end;
escr_dtype_time_k: begin               {TIME - TIME}
      dtype := escr_dtype_fp_k;
      sumf := sys_clock_to_fp2 (
        sys_clock_sub (sumt, val.time) );
      end;
otherwise
    goto dtype_bad;
    end;
  end;

      end;                             {end of current data type cases}
    end;                               {back to get the next term}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, sumi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, sumf);
      end;
escr_dtype_time_k: begin               {TIME}
      escr_ifn_ret_time (e, sumt);
      end;
    end;
  return;
{
*   The data type of the current term is invalid.
}
dtype_bad:
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  end;
{
********************************************************************************
*
*   1+ var
*
*   Return the value of the integer variable VAR, then increment the variable by
*   1.
}
procedure escr_ifun_postinc(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the variable}

begin
  if not escr_ifn_get_var_int (e, sym_p, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ifn_ret_int (e, sym_p^.var_val.int); {pass back variable value}
  sym_p^.var_val.int := sym_p^.var_val.int + 1; {increment the variable}
  end;
{
********************************************************************************
*
*   +1 var
*
*   Increment the integer variable VAR by 1, then return its value.
}
procedure escr_ifun_preinc(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the variable}

begin
  if not escr_ifn_get_var_int (e, sym_p, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  sym_p^.var_val.int := sym_p^.var_val.int + 1; {increment the variable}
  escr_ifn_ret_int (e, sym_p^.var_val.int); {pass back variable value}
  end;
{
********************************************************************************
*
*   1- var
*
*   Return the value of the integer variable VAR, then decrement the variable by
*   1.
}
procedure escr_ifun_postdec(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the variable}

begin
  if not escr_ifn_get_var_int (e, sym_p, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ifn_ret_int (e, sym_p^.var_val.int); {pass back variable value}
  sym_p^.var_val.int := sym_p^.var_val.int - 1; {decrement the variable}
  end;
{
********************************************************************************
*
*   -1 var
*
*   Decrement the integer variable VAR by 1, then return its value.
}
procedure escr_ifun_predec(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to the variable}

begin
  if not escr_ifn_get_var_int (e, sym_p, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  sym_p^.var_val.int := sym_p^.var_val.int - 1; {decrement the variable}
  escr_ifn_ret_int (e, sym_p^.var_val.int); {pass back variable value}
  end;
{
********************************************************************************
*
*   * arg ... arg
*
*   Product of all arguments.  All arguments must be numeric.  With a single
*   argument, the result is that argument.  With no arguments, the result is
*   integer 1.
}
procedure escr_ifun_times (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  resi: sys_int_max_t;                 {integer result}
  resf: sys_fp_max_t;                  {floating point result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  dtype := escr_dtype_int_k;           {init to result data type is integer}
  resi := 1;                           {init the result}

  while true do begin                  {loop over all the parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;
    case dtype of                      {what is the current data type ?}
{
*   Current result data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT * INT}
      resi := resi * val.int;
      end;
escr_dtype_fp_k: begin                 {INT * FP}
      dtype := escr_dtype_fp_k;
      resf := resi * val.fp;
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current result data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP * INT}
      resf := resf * val.int;
      end;
escr_dtype_fp_k: begin                 {FP * FP}
      resf := resf * val.fp;
      end;
otherwise
    goto dtype_bad;
    end;
  end;

      end;                             {end of current data type cases}
    end;                               {back to get the next term}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, resi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, resf);
      end;
    end;
  return;
{
*   The data type of the current term is invalid.
}
dtype_bad:
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  end;
{
********************************************************************************
*
*   / arg1 [arg ... arg]
*
*   The first argument divided by the product of the remaining arguments.  All
*   argumnets must be numeric.  The first argument is required.  The result is
*   always floating point.
}
procedure escr_ifun_divide (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad, div0;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of                    {what data type is this first arument ?}
escr_dtype_int_k: begin
      resf := val.int;
      end;
escr_dtype_fp_k: begin
      resf := val.fp;
      end;
otherwise
    goto dtype_bad;
    end;

  while true do begin                  {loop over all the parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;

    case val.dtype of                  {what type is this term ?}
escr_dtype_int_k: begin                {FP / INT}
        if val.int = 0 then goto div0;
        resf := resf / val.int;
        end;
escr_dtype_fp_k: begin                 {FP / FP}
        if val.fp = 0.0 then goto div0;
        resf := resf / val.fp;
        end;
otherwise
      goto dtype_bad;
      end;

    end;                               {back to get the next term}

  escr_ifn_ret_fp (e, resf);           {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {data type of term is invalid}
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  return;

div0:                                  {attempt to divide by 0}
  sys_stat_set (escr_subsys_k, escr_err_div0_k, stat);
  end;
{
********************************************************************************
*
*   DIV arg1 [arg ... arg]
*
*   Integer division (any fraction truncated) of the first argument successivly
*   by each of the following arguments.  All arguments must be integer, and the
*   result is integer.
}
procedure escr_ifun_div (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resi: sys_int_max_t;                 {integer result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad, div0;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of                    {what data type is this first arument ?}
escr_dtype_int_k: begin
      resi := val.int;
      end;
otherwise
    goto dtype_bad;
    end;

  while true do begin                  {loop over all the parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;

    case val.dtype of                  {what type is this term ?}
escr_dtype_int_k: begin                {INT div INT}
        if val.int = 0 then goto div0;
        resi := resi div val.int;
        end;
otherwise
      goto dtype_bad;
      end;

    end;                               {back to get the next term}

  escr_ifn_ret_int (e, resi);          {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {data type of term is invalid}
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  return;

div0:                                  {attempt to divide by 0}
  sys_stat_set (escr_subsys_k, escr_err_div0_k, stat);
  end;
{
********************************************************************************
*
*   TRUNC val
*
*   Return the integer truncation (rounding towards 0) of VAL.
}
procedure escr_ifun_trunc (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {current function parameter}

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, val.int);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_int (e, trunc(val.fp));
      end;
otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   RND val
*
*   VAL rounded to the nearest integer.
}
procedure escr_ifun_rnd (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {current function parameter}

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, val.int);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_int (e, round(val.fp));
      end;
otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   MAX arg1 [arg ... arg]
*
*   Maximum of all arguments.
}
procedure escr_ifun_max (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  resi: sys_int_max_t;                 {integer result}
  resf: sys_fp_max_t;                  {floating point result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of                    {what data type is this first arument ?}
escr_dtype_int_k: begin
      dtype := escr_dtype_int_k;
      resi := val.int;
      end;
escr_dtype_fp_k: begin
      dtype := escr_dtype_fp_k;
      resf := val.fp;
      end;
otherwise
    goto dtype_bad;
    end;

  while true do begin                  {loop over the remaining parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;
    case dtype of                      {what is the current data type ?}
{
*   Current result data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT, INT}
      resi := max(resi, val.int);
      end;
escr_dtype_fp_k: begin                 {INT, FP}
      dtype := escr_dtype_fp_k;
      resf := max(resi, val.fp);
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current result data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP, INT}
      resf := max(resf, val.int);
      end;
escr_dtype_fp_k: begin                 {FP, FP}
      resf := max(resf, val.fp);
      end;
otherwise
    goto dtype_bad;
    end;
  end;

      end;                             {end of current data type cases}
    end;                               {back to get the next term}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, resi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, resf);
      end;
    end;
  return;
{
*   The data type of the current term is invalid.
}
dtype_bad:
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  end;
{
********************************************************************************
*
*   MIN arg1 [arg ... arg]
*
*   Minimum of all arguments.
}
procedure escr_ifun_min (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  resi: sys_int_max_t;                 {integer result}
  resf: sys_fp_max_t;                  {floating point result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of                    {what data type is this first arument ?}
escr_dtype_int_k: begin
      dtype := escr_dtype_int_k;
      resi := val.int;
      end;
escr_dtype_fp_k: begin
      dtype := escr_dtype_fp_k;
      resf := val.fp;
      end;
otherwise
    goto dtype_bad;
    end;

  while true do begin                  {loop over the remaining parameters}
    if not escr_ifn_get_val (e, val, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;
    case dtype of                      {what is the current data type ?}
{
*   Current result data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT, INT}
      resi := min(resi, val.int);
      end;
escr_dtype_fp_k: begin                 {INT, FP}
      dtype := escr_dtype_fp_k;
      resf := min(resi, val.fp);
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current result data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP, INT}
      resf := min(resf, val.int);
      end;
escr_dtype_fp_k: begin                 {FP, FP}
      resf := min(resf, val.fp);
      end;
otherwise
    goto dtype_bad;
    end;
  end;

      end;                             {end of current data type cases}
    end;                               {back to get the next term}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, resi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, resf);
      end;
    end;
  return;
{
*   The data type of the current term is invalid.
}
dtype_bad:
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  end;
{
********************************************************************************
*
*   ABS arg
*
*   Absolute value.
}
procedure escr_ifun_abs (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {current function parameter}

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, abs(val.int));
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, abs(val.fp));
      end;
otherwise                              {bad data type}
    escr_ifn_bad_dtype (e, val, stat);
    end;
  end;
{
********************************************************************************
*
*   SQRT arg
*
*   Square root.
}
procedure escr_ifun_sqrt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fp: sys_fp_max_t;

begin
  if not escr_ifn_get_fp (e, fp, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if fp < 0.0 then begin               {negative ?}
    sys_stat_set (escr_subsys_k, escr_err_sqrtneg_k, stat);
    return;
    end;

  escr_ifn_ret_fp (e, sqrt(fp));       {return the result}
  end;
{
********************************************************************************
*
*   SIN arg
*
*   Sine.  ARG is in radians.
}
procedure escr_ifun_sin (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  resf := sin(resf);                   {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   COS
*
*   Cosine.  ARG is in radians.
}
procedure escr_ifun_cos (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  resf := cos(resf);                   {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   TAN arg
*
*   Tangent.  ARG is in radians.
}
procedure escr_ifun_tan (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  resf := sin(resf) / cos(resf);       {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   PI [val]
*
*   Returns Pi or Pi*val.
}
procedure escr_ifun_pi (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fp: sys_fp_max_t;                    {function return value}

begin
  if escr_ifn_get_fp (e, fp, stat)
    then begin                         {got VAL in FP}
      fp := fp * pi;                   {make the result}
      end
    else begin                         {didn't get VAL}
      if sys_error(stat) then return;
      fp := pi;                        {return just Pi}
      end
    ;

  escr_ifn_ret_fp (e, fp);             {return the result}
  end;
{
********************************************************************************
*
*   E [arg]
*
*   Returns e^ARG.  Just returns e when no arg.
}
procedure escr_ifun_e (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    if sys_error(stat) then return;    {hard error ?}
    escr_ifn_ret_fp (e, ek);           {return just e}
    return;
    end;

  resf := exp(resf);                   {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   EXP arg1 arg2
*
*   ARG1 raised to the power of ARG2.  If ARG1 is negative, then ARG2 must be
*   integer.
}
procedure escr_ifun_exp (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  dtype: escr_dtype_k_t;               {return value data type}
  resi: sys_int_max_t;                 {integer result}
  resf: sys_fp_max_t;                  {floating point result}
  val: escr_val_t;                     {current function parameter}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get first term}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val.dtype of                    {what data type is this first arument ?}
escr_dtype_int_k: begin                {INT}
      resi := val.int;
      end;
escr_dtype_fp_k: begin
      resf := val.fp;                  {FP}
      end;
otherwise
    goto dtype_bad;
    end;
  dtype := val.dtype;

  if not escr_ifn_get_val (e, val, stat) then begin {get second term}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case dtype of                        {what is the current data type ?}
{
*   Current result data type is INTEGER.
}
escr_dtype_int_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {INT ** INT}
      resi := resi ** val.int;
      end;
escr_dtype_fp_k: begin                 {INT ** FP}
      if resi < 0 then begin
        sys_stat_set (escr_subsys_k, escr_err_negexpnint_k, stat);
        return;
        end;
      resf := resi ** val.fp;
      dtype := escr_dtype_fp_k;
      end;
otherwise
    goto dtype_bad;
    end;
  end;
{
*   Current result data type is FLOATING POINT.
}
escr_dtype_fp_k: begin
  case val.dtype of
escr_dtype_int_k: begin                {FP ** INT}
      resf := resf ** val.int;
      end;
escr_dtype_fp_k: begin                 {FP ** FP}
      if resf < 0.0 then begin
        sys_stat_set (escr_subsys_k, escr_err_negexpnint_k, stat);
        return;
        end;
      resf := resf ** val.fp;
      end;
otherwise
    goto dtype_bad;
    end;
  end;

    end;                               {end of current data type cases}
{
*   Return the result.
}
  case dtype of                        {what is the result data type ?}
escr_dtype_int_k: begin                {INT}
      escr_ifn_ret_int (e, resi);
      end;
escr_dtype_fp_k: begin                 {FP}
      escr_ifn_ret_fp (e, resf);
      end;
    end;
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL has bad data type}
  escr_ifn_bad_dtype (e, val, stat);   {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   LOG arg
*
*   Natural logarithm of ARG.
}
procedure escr_ifun_log (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if resf <= 0.0 then begin
    sys_stat_set (escr_subsys_k, escr_err_lognpos_k, stat);
    return;
    end;

  resf := ln(resf);                    {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   LOG2 arg
*
*   Base 2 logarithm of ARG.
}
procedure escr_ifun_log2 (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if resf <= 0.0 then begin
    sys_stat_set (escr_subsys_k, escr_err_lognpos_k, stat);
    return;
    end;

  resf := ln(resf) / ln2;              {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   RDEG arg
*
*   Radians to degrees.
}
procedure escr_ifun_rdeg (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  conv = 180.0 / pi;                   {conversion factor}

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  resf := resf * conv;                 {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   DEGR arg
*
*   Degrees to radians.
}
procedure escr_ifun_degr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  conv = pi / 180.0;                   {conversion factor}

var
  resf: sys_fp_max_t;                  {floating point result}

begin
  if not escr_ifn_get_fp (e, resf, stat) then begin {get the argument}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  resf := resf * conv;                 {perform the operation}
  escr_ifn_ret_fp (e, resf);           {return the result}
  end;
{
********************************************************************************
*
*   < arg1 arg2
}
procedure escr_ifun_lt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int < val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int < val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp < val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp < val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) < 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := comp = sys_compare_lt_k;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   <= arg1 arg2
}
procedure escr_ifun_le (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int <= val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int <= val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp <= val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp <= val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) <= 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := (comp = sys_compare_lt_k) or (comp = sys_compare_eq_k);
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   = arg1 arg2
}
procedure escr_ifun_eq (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_bool_k: begin               {boolean}
      case val2.dtype of
escr_dtype_bool_k: begin               {BOOL BOOL}
          resb := val1.bool = val2.bool;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int = val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int = val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp = val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp = val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) = 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := comp = sys_compare_eq_k;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   <> arg1 arg2
}
procedure escr_ifun_ne (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_bool_k: begin               {boolean}
      case val2.dtype of
escr_dtype_bool_k: begin               {BOOL BOOL}
          resb := val1.bool <> val2.bool;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int <> val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int <> val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp <> val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp <> val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) <> 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := comp <> sys_compare_eq_k;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   >= arg1 arg2
}
procedure escr_ifun_ge (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int >= val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int >= val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp >= val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp >= val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) >= 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := (comp = sys_compare_eq_k) or (comp = sys_compare_gt_k);
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   > arg1 arg2
}
procedure escr_ifun_gt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val1, val2: escr_val_t;              {value of each argument}
  resb: boolean;                       {boolean result}
  comp: sys_compare_k_t;               {intermediate compare result}

label
  dtype_bad;

begin
  if not escr_ifn_get_val (e, val1, stat) then begin {get first term into VAL1}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not escr_ifn_get_val (e, val2, stat) then begin {get first term into VAL2}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  case val1.dtype of                   {data type of first term ?}

escr_dtype_int_k: begin                {integer}
      case val2.dtype of
escr_dtype_int_k: begin                {INT INT}
          resb := val1.int > val2.int;
          end;
escr_dtype_fp_k: begin                 {INT FP}
          resb := val1.int > val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_fp_k: begin                 {floating point}
      case val2.dtype of
escr_dtype_int_k: begin                {FP INT}
          resb := val1.fp > val2.int;
          end;
escr_dtype_fp_k: begin                 {FP FP}
          resb := val1.fp > val2.fp;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_str_k: begin                {string}
      case val2.dtype of
escr_dtype_str_k: begin                {STR STR}
          resb := string_compare (val1.str, val2.str) > 0;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

escr_dtype_time_k: begin               {time}
      case val2.dtype of
escr_dtype_time_k: begin               {TIME TIME}
          comp := sys_clock_compare (val1.time, val2.time);
          resb := comp = sys_compare_gt_k;
          end;
otherwise
        goto dtype_bad;
        end;
      end;

otherwise
    escr_ifn_bad_dtype (e, val1, stat); {set STAT to bad data type error}
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  return;
{
*   Error exits.
}
dtype_bad:                             {VAL2 has bad data type}
  escr_ifn_bad_dtype (e, val2, stat);  {set STAT to bad data type error}
  return;
  end;
{
********************************************************************************
*
*   NOW
*
*   Returns the current time.
}
procedure escr_ifun_now (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  escr_ifn_ret_time (e, sys_clock);
  end;
{
********************************************************************************
*
*   DATE time [local] [field arg ... arg] ... [field arg ... arg]
*
*   Creates a formatted string from the time TIME.  See the documentation for
*   details.
}
procedure escr_ifun_date (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  time: sys_clock_t;                   {time to make string from}
  tzone: sys_tzone_k_t;                {time zone}
  hours_west: real;                    {hours west of CUT}
  daysave: sys_daysave_k_t;            {daylight savings strategy}
  date: sys_date_t;                    {expanded date descriptor}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  ii: sys_int_max_t;                   {integer parameter}
  tk: string_var8192_t;                {temporary token}
  str: string_var8192_t;               {returned string}
  havedate: boolean;                   {DATE has been filled in}
  wstr: boolean;                       {something has been written to STR}

label
  chdate;
{
******************************
*
*   Local subroutine MAKE_DATE
*
*   Ensure that DATE is filled in.  Nothing is done if DATE is already filled
*   in.
}
procedure make_date;
  val_param; internal;

begin
  if not havedate then begin
    sys_clock_to_date (                {make expanded date descriptor from the time}
      time,                            {input time}
      tzone, hours_west, daysave,      {information about timezone to convert into}
      date);                           {returned expanded date/time descriptor}
    havedate := true;                  {indicate that DATE has been filled in}
    end;
  end;
{
******************************
*
*   Start of main routine.
}
begin
  tk.max := size_char(tk.str);         {init local var strings}
  str.max := size_char(str.str);

  if not escr_ifn_get_time (e, time, stat) then begin {get time to make string from}
    escr_ifn_stat_required (e, stat);  {the first parameter is required}
    return;
    end;

  tzone := sys_tzone_cut_k;            {init for coordinated universal time}
  hours_west := 0.0;
  daysave := sys_daysave_no_k;
  str.len := 0;                        {init returned string to empty}
  havedate := false;                   {init to DATE not filled in yet}
  wstr := false;                       {init to nothing written to STR yet}

  while true do begin                  {loop over each keyword}
    if not escr_ifn_get_keyw (e, tk, stat) then begin
      if sys_error(stat) then return;
      exit;                            {done processing all arguments}
      end;
    string_tkpick80 (tk,               {pick keyword from list}
      'LOCAL STR YEAR MNUM MONTH MON DAY DAYWK DWK HOUR MIN SEC SECF D',
      pick);
    case pick of

1: begin                               {LOCAL}
  if havedate then goto chdate;
  sys_timezone_here (                  {set date converion for local time zone}
    tzone, hours_west, daysave);
  end;

2: begin                               {STR string}
  if not escr_ifn_get_str (e, tk, stat) then begin
    escr_ifn_stat_required (e, stat);  {parameter is required}
    return;
    end;
  string_append (str, tk);
  wstr := true;
  end;

3: begin                               {YEAR}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_year_k, 4, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

4: begin                               {MNUM}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_mon_k, 2, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

5: begin                               {MONTH}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_mon_name_k, string_fw_freeform_k, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

6: begin                               {MON}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_mon_abbr_k, string_fw_freeform_k, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

7: begin                               {DAY}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_day_k, 2, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

8: begin                               {DAYWK}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_daywk_name_k, string_fw_freeform_k, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

9: begin                               {DWK}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_daywk_abbr_k, string_fw_freeform_k, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

10: begin                              {HOUR}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_hour_k, 2, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

11: begin                              {MIN}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_min_k, 2, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

12: begin                              {SEC}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_sec_k, 2, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

13: begin                              {SECF n}
  make_date;                           {make sure DATE is filled in}
  if not escr_ifn_get_int (e, ii, stat) then begin
    escr_ifn_stat_required (e, stat);  {parameter is required}
    return;
    end;
  sys_date_string (date, sys_dstr_sec_frac_k, ii+3, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

14: begin                              {D}
  make_date;                           {make sure DATE is filled in}
  sys_date_string (date, sys_dstr_day_k, string_fw_freeform_k, tk, stat);
  string_append (str, tk);
  wstr := true;
  end;

otherwise                              {unrecognized keyword}
      sys_stat_set (escr_subsys_k, escr_err_badparmfun_k, stat);
      sys_stat_parm_vstr (tk, stat);
      sys_stat_parm_vstr (e.parse_p^.funame, stat);
      return;
      end;                             {end of keyword cases}
    end;                               {back to get next keyword}

  if not wstr then begin               {nothing was written, write default ?}
    make_date;                         {make expanded date description}
    sys_date_string (date, sys_dstr_year_k, 4, tk, stat);
    string_append (str, tk);
    string_append1 (str, '/');
    sys_date_string (date, sys_dstr_mon_k, 2, tk, stat);
    string_append (str, tk);
    string_append1 (str, '/');
    sys_date_string (date, sys_dstr_day_k, 2, tk, stat);
    string_append (str, tk);
    string_append1 (str, '.');
    sys_date_string (date, sys_dstr_hour_k, 2, tk, stat);
    string_append (str, tk);
    string_append1 (str, ':');
    sys_date_string (date, sys_dstr_min_k, 2, tk, stat);
    string_append (str, tk);
    string_append1 (str, ':');
    sys_date_string (date, sys_dstr_sec_k, 2, tk, stat);
    string_append (str, tk);
    end;

  escr_ifn_ret_str (e, str);           {return the accumulated string}
  return;

chdate:                                {date-changing arg after date set}
  sys_stat_set (escr_subsys_k, escr_err_afdate_k, stat);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   IF cond arg1 arg2
*
*   Returns ARG1 when COND is true, ARG2 when false.
}
procedure escr_ifun_if (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  b: boolean;                          {COND value}
  val: escr_val_t;                     {current function parameter}

begin
  if not escr_ifn_get_bool (e, b, stat) then begin {get COND value into B}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if not escr_ifn_get_val (e, val, stat) then begin {get first value}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if b then begin                      {return first value ?}
    escr_ifn_ret_val (e, val);
    end;

  if not escr_ifn_get_val (e, val, stat) then begin {get second value}
    escr_ifn_stat_required (e, stat);
    return;
    end;
  if not b then begin                  {return second value ?}
    escr_ifn_ret_val (e, val);
    end;
  end;
{
********************************************************************************
*
*   ARG n
*
*   Return the raw characters of argument N to the innermost block that takes
*   arguments.
}
procedure escr_ifun_arg (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  argn: sys_int_max_t;                 {argument number}
  str_p: string_var_p_t;               {pointer to argument string}

begin
  if not escr_ifn_get_int (e, argn, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_exblock_arg_get (e, argn, str_p); {get pointer to the indexed argument}
  if str_p <> nil then begin           {argument string exists}
    escr_ifn_ret_chars (e, str_p^);    {return just the raw argument characters}
    end;
  end;
{
********************************************************************************
*
*   SYM name [NL] [qual]
*
*   Returns infomation about the symbol NAME.  QUAL can be:
*
*     TYPE (default)  -  Returns symbol type, like VAR, CONST, etc.
*
*     DTYPE  -  Returns symbol data type name.
*
*     VER  -  Returns the absolute version number of the symbol.
*
*     QUAL  -  Returns fully qualified symbol name.
*
*   The empty string is returned if the symbol does not exist.
*
*   The optional NL keyword causes local versions of symbols to be ignored.
*   Symbols are resolved as they would be in the parent context of the current
*   execution block.
}
procedure escr_ifun_sym (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var132_t;               {symbol name}
  sym_p: escr_sym_p_t;                 {pointer to symbol}
  dtype: escr_dtype_k_t;               {data type of symbol}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  tk: string_var132_t;
  locoff: boolean;                     {local symbols have been disabled}

label
  next_token, leave, ret_empty;

begin
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);

  if not escr_ifn_get_str (e, name, stat) then begin {get the symbol name}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  locoff := false;                     {init to local symbols not off}
next_token:
  if escr_ifn_get_keyw (e, tk, stat)   {get next keyword}
    then begin                         {got it}
      string_tkpick80 (tk,             {pick keyword from the list}
        'TYPE DTYPE VER QUAL NAME NL',
        pick);
      if pick <= 0 then begin
        escr_ifn_bad_keyw (e, tk, stat);
        goto leave;
        end;
      end
    else begin                         {no keyword or error}
      if sys_error(stat) then goto leave; {hard error}
      pick := 1;                       {set choice to default keyword}
      end
    ;

  escr_sym_find (e, name, sym_p);      {find the symbol if it exists}
  if sym_p = nil then goto ret_empty;  {no such symbol, return empty string}

  case pick of                         {what about symbol to return ?}
{
********************
*
*   SYM name TYPE
}
1:  begin
  case sym_p^.stype of                 {what type of symbol is this ?}
escr_sym_var_k: escr_ifn_ret_strp (e, 'VAR'(0));
escr_sym_const_k: escr_ifn_ret_strp (e, 'CONST'(0));
escr_sym_subr_k, escr_sym_isubr_k: escr_ifn_ret_strp (e, 'SUBR'(0));
escr_sym_macro_k, escr_sym_imacro_k: escr_ifn_ret_strp (e, 'MACRO'(0));
escr_sym_func_k, escr_sym_ifunc_k: escr_ifn_ret_strp (e, 'FUNC'(0));
escr_sym_cmd_k, escr_sym_icmd_k: escr_ifn_ret_strp (e, 'CMD'(0));
escr_sym_label_k: escr_ifn_ret_strp (e, 'LABEL'(0));
    end;
  end;
{
********************
*
*   SYM name DTYPE
}
2:  begin
  case sym_p^.stype of                 {what type of symbol is this ?}
escr_sym_var_k: dtype := sym_p^.var_val.dtype;
escr_sym_const_k: dtype := sym_p^.const_val.dtype;
otherwise                              {symbol doesn't have data type}
    goto ret_empty;
    end;
  case dtype of                        {which data type ?}
escr_dtype_bool_k: escr_ifn_ret_strp (e, 'BOOL'(0));
escr_dtype_int_k: escr_ifn_ret_strp (e, 'INTEGER'(0));
escr_dtype_fp_k: escr_ifn_ret_strp (e, 'REAL'(0));
escr_dtype_str_k: escr_ifn_ret_strp (e, 'STRING'(0));
escr_dtype_time_k: escr_ifn_ret_strp (e, 'TIME'(0));
otherwise                              {unsupported data type}
    escr_err_dtype_unimp (e, dtype, 'ESCR_IFUN_SYM');
    end;
  end;
{
********************
*
*   SYM name VER
}
3:  begin
  escr_ifn_ret_int (e, sym_p^.vern);
  end;
{
********************
*
*   SYM name QUAL
}
4:  begin
  escr_sym_name (sym_p^, tk);          {make the fully qualified name}
  escr_ifn_ret_str (e, tk);            {return it}
  end;
{
********************
*
*   SYM name NAME
}
5:  begin
  escr_ifn_ret_str (e, sym_p^.name_p^);
  end;
{
********************
*
*   NL
}
6:  begin
      if not locoff then begin         {locals not already off ?}
        escr_exblock_locals_off (e);   {turn off local symbols}
        locoff := true;                {remember that local symbols are off}
        end;
      goto next_token;                 {back to get next function option keyword}
      end;
{
********************
*
*   Unrecognized qualifier.
}
otherwise                              {unrecognized QUAL keyword}
    writeln ('INTERNAL ERROR: Unexpected PICK value of ', pick, ' in IFUN_SYM.');
    escr_err_atline (e, '', '', nil, 0);
    end;                               {end of QUAL cases}

leave:                                 {common exit point if locals may be off}
  if locoff then begin                 {local symbols temporarily off ?}
    escr_exblock_locals_on (e);        {re-enable the local symbols}
    end;
  return;

ret_empty:                             {return the empty string}
  escr_ifn_ret_empty (e);              {no such symbol, return the empty string}
  goto leave;
  end;
{
********************************************************************************
*
*   EXIST name [nametype]
*
*   Returns bool indicating whether the symbol NAME exists.  NAMETYPE can be:
*
*     PSYM  -  Symbol (default).
*
*     EVAR  -  System environment variable.
*
*     FNAM  -  File system name, links followed.
*
*     FNAMNL  -  File system name, links not followed.
*
*     ARG  -  Execution block argument.  NAME is integer number of the argument.
}
procedure escr_ifun_exist (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var8192_t;              {NAME}
  sym_p: escr_sym_p_t;                 {pointer to symbol}
  hpos: string_hash_pos_t;             {position of name in symbol table}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  tk: string_var8192_t;
  str_p: string_var_p_t;               {scratch pointer to string}
  tstat: string_tnstat_k_t;            {treename translation status}
  resb: boolean;                       {boolean return value}

begin
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);

  if not escr_ifn_get_str (e, name, stat) then begin {get the symbol name}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  if escr_ifn_get_keyw (e, tk, stat)   {get NAMETYPE keyword}
    then begin                         {got it}
      string_tkpick80 (tk,             {pick keyword from the list}
        'PSYM EVAR FNAM FNAMNL ARG',
        pick);
      end
    else begin                         {no keyword or error}
      if sys_error(stat) then return;  {hard error}
      pick := 1;                       {set choice to default keyword}
      end
    ;
  resb := false;                       {init to item does not exist}
  case pick of                         {which NAMETYPE ?}

1:  begin                              {PSYM}
      escr_sym_lookup_qual (           {look up the symbol}
        e, name, hpos, sym_p, stat);
      if sys_error(stat) then return;
      resb := sym_p <> nil;
      end;

2:  begin                              {EVAR}
      sys_envvar_get (name, tk, stat);
      resb := not sys_stat_match (sys_subsys_k, sys_stat_envvar_noexist_k, stat);
      sys_error_none (stat);
      end;

3:  begin                              {FNAM}
      string_treename_opts (           {translate file system pathname}
        name,                          {input pathname}
        [ string_tnamopt_flink_k,      {follow symbolic links}
          string_tnamopt_remote_k,     {continue on remote systems as needed}
          string_tnamopt_proc_k,       {translate from point of view of this process}
          string_tnamopt_native_k],    {return native system name, not Embed portable}
        tk,                            {resulting pathname}
        tstat);
      if tstat = string_tnstat_native_k then begin {got native pathname ?}
        resb := file_exists (tk);      {check the pathname}
        end;
      end;

4:  begin                              {FNAMNL}
      string_treename_opts (           {translate file system pathname}
        name,                          {input pathname}
        [ string_tnamopt_proc_k,       {translate from point of view of this process}
          string_tnamopt_native_k],    {return native system name, not Embed portable}
        tk,                            {resulting pathname}
        tstat);
      if tstat = string_tnstat_native_k then begin {got native pathname ?}
        resb := file_exists (tk);      {check the pathname}
        end;
      end;

5:  begin                              {ARG}
      string_t_int (name, pick, stat); {make argument number}
      if sys_error(stat) then begin
        escr_ifn_bad_keyw (e, name, stat);
        return;
        end;
      escr_exblock_arg_get (e, pick, str_p); {get pointer to argument value}
      resb := str_p <> nil;            {TRUE if argument exists}
      end;

otherwise                              {unrecognized NAMETYPE keyword}
    escr_ifn_bad_keyw (e, tk, stat);
    return;
    end;

  escr_ifn_ret_bool (e, resb);         {return the result}
  end;
{
********************************************************************************
*
*   TNAM fnam
*
*   Full treename of fnam.
}
procedure escr_ifun_tnam (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;
  ress: string_treename_t;

begin
  fnam.max := size_char(fnam.str);     {init local var strings}
  ress.max := size_char(ress.str);

  escr_ifn_get_strs (e, fnam, stat);   {get concatenation of all arguments}
  if sys_error(stat) then return;

  string_treename (fnam, ress);
  escr_ifn_ret_str (e, ress);
  end;
{
********************************************************************************
*
*   LNAM fnam suff ... suff
}
procedure escr_ifun_lnam (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {input pathname}
  suff: string_var80_t;                {list of suffixes}
  ress: string_treename_t;             {result string}

begin
  fnam.max := size_char(fnam.str);     {init local var strings}
  suff.max := size_char(suff.str);
  ress.max := size_char(ress.str);

  if not escr_ifn_get_str (e, fnam, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  suff.len := 0;                       {init list of suffixes to empty}
  while true do begin                  {loop over the SUFF arguments}
    if not escr_ifn_get_str (e, ress, stat) then begin
      if sys_error(stat) then return;
      exit;
      end;
    string_append_token (suff, ress);  {add this suffix to list}
    end;                               {back to get next suffix}

  string_terminate_null (suff);
  string_generic_fnam (fnam, suff.str, ress); {do the conversion}
  escr_ifn_ret_str (e, ress);          {return the result}
  end;
{
********************************************************************************
*
*   DIR fnam
*
*   Full directory pathname of FNAM.
}
procedure escr_ifun_dir (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {input pathname}
  ress: string_treename_t;             {resulting string}
  tnam: string_treename_t;             {scratch treename}

begin
  fnam.max := size_char(fnam.str);     {init local var strings}
  ress.max := size_char(ress.str);
  tnam.max := size_char(tnam.str);

  escr_ifn_get_strs (e, fnam, stat);   {get concatenation of all arguments}
  if sys_error(stat) then return;

  if fnam.len = 0
    then begin                         {empty string arg, return current directory}
      string_treename (fnam, ress);
      end
    else begin                         {pathname supplies, return lowest directory}
      string_treename (fnam, tnam);    {make full pathname of FNAM}
      string_pathname_split (tnam, ress, fnam); {split into directory and leafname}
      end
    ;

  escr_ifn_ret_str (e, ress);          {return the directory name}
  end;
{
********************************************************************************
*
*   EVAR name
*
*   System environment variable value.  Empty string if no such variable.
}
procedure escr_ifun_evar (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_treename_t;             {NAME parameter}
  ress: string_treename_t;             {result string}

begin
  name.max := size_char(name.str);     {init local var strings}
  ress.max := size_char(ress.str);

  if not escr_ifn_get_str (e, name, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  sys_envvar_get (name, ress, stat);
  if sys_error(stat) then begin
    ress.len := 0;
    sys_error_none(stat);
    end;

  escr_ifn_ret_str (e, ress);          {return the result}
  end;
{
********************************************************************************
*
*   LAB name
*
*   Write label made unique to this invocation of a macro, subroutine, and the
*   like.
}
procedure escr_ifun_lab (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_treename_t;             {NAME parameter}
  ress: string_treename_t;             {result string}

begin
  name.max := size_char(name.str);     {init local var strings}
  ress.max := size_char(ress.str);

  if not escr_ifn_get_keyw (e, name, stat) then begin
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ulab_get (e, name, ress);       {get the label}
  escr_ifn_ret_chars (e, ress);        {return the result}
  end;
{
********************************************************************************
*
*   V arg
*
*   Returns the value of the argument.  This can be used to dereference a
*   variable, for example.
}
procedure escr_ifun_v (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {argument value}

begin
  if not escr_ifn_get_val (e, val, stat) then begin {get the argument value}
    escr_ifn_stat_required (e, stat);  {this argument is required}
    return;
    end;

  escr_ifn_ret_val (e, val);           {return the value of the argument}
  end;
{
********************************************************************************
*
*   SEQ fnam [AFT, incr,  first]
*
*   Return sequence number.
}
procedure escr_ifun_seq (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  fnam: string_treename_t;             {FNAM parameter}
  tk: string_var32_t;                  {scratch token}
  flags: string_seq_t;                 {modifier flags}
  iparm: sys_int_machine_t;            {1-N number of next integer parameter}
  ii: sys_int_machine_t;               {scratch integer}
  incr: sys_int_max_t;                 {sequence increment}
  first: sys_int_max_t;                {sequence when file not previously exist}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  resi: sys_int_max_t;                 {integer result}

begin
  fnam.max := size_char(fnam.str);     {init local var strings}
  tk.max := size_char(tk.str);
  flags := [];                         {init all modifier flags to off}
  iparm := 1;                          {next integer parameter is the first}
  incr := 1;                           {init sequence increment value}
  first := 1;                          {init seq when file not exist}

  if not escr_ifn_get_str (e, fnam, stat) then begin {get sequence file name}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  while true do begin                  {loop over all the parameters}
    if not escr_ifn_get_keyw (e, tk, stat) then begin {get this term into VAL}
      if sys_error(stat) then return;
      exit;
      end;

    string_t_int (tk, ii, stat);       {try to interpret as integer}
    if not sys_error(stat) then begin
      case iparm of                    {which integer parameter is this ?}
1:    incr := ii;                      {increment amount}
2:    first := ii;                     {first value for new sequence}
otherwise
        sys_stat_set (escr_subsys_k, escr_err_extra_k, stat); {extra parameter}
        sys_stat_parm_vstr (tk, stat);
        return;
        end;
      iparm := iparm + 1;              {make number of next integer parameter}
      next;                            {back to get next parameter}
      end;

    string_upcase (tk);                {make upper case for keyword matching}
    string_tkpick80 (tk,               {pick the keyword from the list}
      'AFT',
      pick);
    case pick of                       {which keyword is it ?}
1:    begin                            {AFT}
        flags := flags + [string_seq_after_k]; {get value after increment}
        end;
otherwise
      escr_ifn_bad_keyw (e, tk, stat);
      return;
      end;
    end;                               {back to get next parameter}

  resi := string_seq_get (             {get the unique sequence number}
    fnam,                              {sequence number file name}
    incr,                              {amount to increment seq number by}
    first,                             {initial value if file not exist}
    flags,                             {modifier flags}
    stat);
  if sys_error(stat) then return;

  escr_ifn_ret_int (e, resi);          {return the integer value}
  end;
{
********************************************************************************
*
*   UNQUOTE str
*
*   Return the string STR without surrounding quotes, if any.  If STR does not
*   have starting and ending quotes, then nothing is done.  If it does, then the
*   string inside these quotes is returned.  This function can will remove
*   multiple layers of quotes, if present.
*
*   Two successive end-quote characters in the interior of the string will be
*   translated to a single end-quote character.
}
procedure escr_ifun_unquote (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  s: string_var8192_t;                 {the string to remove quotes from}
  lev: sys_int_machine_t;              {number of layers of quotes found}
  qsyn_p: escr_quotesyn_p_t;           {pointer to current quoted string syntax def}
  qen: char;                           {quote end char for inner-most quotes}
  p: sys_int_machine_t;                {parse index after quotes removed}
  en: sys_int_machine_t;               {index of last char with quotes removed}
  c: char;                             {current source character being parsed}
  pq: boolean;                         {previously parsed char was end quote}

label
  retry, donelev;

begin
  s.max := size_char(s.str);           {init local var string}

  if not escr_ifn_get_str (e, s, stat) then begin {get input string into S}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  lev := 0;                            {init to no levels of quotes found}

retry:                                 {back here to try again after each layer removed}
  if s.len < ((lev * 2) + 2) then begin {too short for another layer of quotes ?}
    goto donelev;                      {done finding all quote levels}
    end;

  qsyn_p := e.quotesyn_p;              {init to first quoted string syntax in list}
  while qsyn_p <> nil do begin         {loop over list of quoted string syntaxes}
    if                                 {enclosed in these quotes ?}
        (s.str[1 + lev] = qsyn_p^.st) and {starts with starting quote ?}
        (s.str[s.len - lev] = qsyn_p^.en) {ends with ending quote ?}
        then begin
      lev := lev + 1;                  {record one more level of quotes found}
      qen := qsyn_p^.en;               {save the quote end character}
      goto retry;                      {back to check for another level of quotes}
      end;
    qsyn_p := qsyn_p^.next_p;          {advance to next quote syntax in list}
    end;                               {back to check this new quote syntax}

donelev:                               {done finding all levels of quotes}
  if lev > 0 then begin                {at least one level of quotes was found ?}
    p := 1 + lev;                      {init parse index to after leading quotes}
    en := s.len - lev;                 {index of last character to parse}
    s.len := 0;                        {init final returne string to empty}
    pq := false;                       {init to previous parsed char no end quote}
    while p <= en do begin             {scan the string inside the quotes}
      c := s.str[p];                   {fetch this source character}
      if c = qen
        then begin                     {this is a quote end character}
          if pq
            then begin                 {previous char was also quote end}
              string_append1 (s, c);   {add single end-quote char to output string}
              pq := false;             {reset to previous char was not end quote}
              end
            else begin                 {previous char was not end quote}
              pq := true;              {flag previous char was end-quote for next time}
              end
            ;
          end
        else begin                     {this is not a quote end char}
          string_append1 (s, c);       {add this character to the output string}
          pq := false;                 {previous char was not end-quote}
          end
        ;
      p := p + 1;                      {advance to next source character}
      end;                             {back to check this new source character}
    end;                               {end of quotes were removed case}

  escr_ifn_ret_str (e, s);             {return the final unquoted string}
  end;
{
********************************************************************************
*
*   ISINT arg
*
*   Returns TRUE iff the argument can be interpreted as a integer value.
}
procedure escr_ifun_isint (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  b: boolean;                          {function return value}
  ival: sys_int_max_t;                 {argument integer value}

begin
  b := escr_ifn_get_int (e, ival, stat); {TRUE if argument is integer}
  sys_error_none (stat);               {don't bomb on bad arg}
  if e.parse_p^.funarg.p <= e.parse_p^.funarg.s.len then begin {unused args ?}
    e.parse_p^.funarg.p := e.parse_p^.funarg.s.len + 1; {indicate all args uses}
    b := false;                        {wasn't just integer}
    end;

  escr_ifn_ret_bool (e, b);            {return the boolean function value}
  end;
{
********************************************************************************
*
*   ISNUM arg
*
*   Returns TRUE iff the argument can be interpreted as a numeric value.
}
procedure escr_ifun_isnum (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  b: boolean;                          {function return value}
  fpval: sys_fp_max_t;                 {the argument value in floating point}

begin
  b := escr_ifn_get_fp (e, fpval, stat); {TRUE if argument is numeric}
  sys_error_none (stat);               {don't bomb on bad arg}
  if e.parse_p^.funarg.p <= e.parse_p^.funarg.s.len then begin {unused args ?}
    e.parse_p^.funarg.p := e.parse_p^.funarg.s.len + 1; {indicate all args uses}
    b := false;                        {wasn't just integer}
    end;

  escr_ifn_ret_bool (e, b);            {return the boolean function value}
  end;
{
********************************************************************************
*
*   VNL arg
*
*   Evaluates the argument with local symbols ignored.  Specifically, for any
*   symbol that has a local version, the current version of that symbol is the
*   one before the local version.  The local versions are restored to the
*   current versions before the function returns.
*
*   This function is like V, except for the handling of local symbols.  The
*   purpose of this function is to be able to evaluate expressions in the
*   context of the parent block.  This is necessary for the proper evaluation of
*   call arguments in most cases.
}
procedure escr_ifun_vnl (              {evaluate without local variables}
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  val: escr_val_t;                     {argument value}
  gotit: boolean;                      {got argument value}

begin
  escr_exblock_locals_off (e);         {make parent versions of local symbols current}
  gotit := escr_ifn_get_val (e, val, stat); {try to get the argument value}
  escr_exblock_locals_on (e);          {make local versions of local symbols current}

  if not gotit then begin              {no function argument ?}
    escr_ifn_stat_required (e, stat);  {this argument is required}
    return;
    end;
  if sys_error(stat) then return;      {error getting function argument ?}

  escr_ifn_ret_val (e, val);           {return the value of the argument}
  end;
{
********************************************************************************
*
*   RUNEX agr ... arg
*
*   Run the command line that is the concatenation of the string representation
*   of all the arguments.  The function returns the exit status code resulting
*   from running the command.  The variable EXITSTATUS and the script exit
*   status code are NOT altered.  Any output from the program is discarded.
}
procedure escr_ifun_runex (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var8192_t;
  tf: boolean;                         {TRUE/FALSE result from the program}
  exstat: sys_sys_exstat_t;            {exit status code from program}

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_ifn_get_strs (e, tk, stat);     {get the concatenated string}
  if sys_error(stat) then return;

  sys_run_wait_stdnone (               {run program, no I/O}
    tk,                                {command line to run}
    tf,                                {TRUE/FALSE result returned by program}
    exstat,                            {exit status returned by program}
    stat);
  if sys_error(stat) then return;

  escr_ifn_ret_int (e, exstat);
  end;
{
********************************************************************************
*
*   RUNTF agr ... arg
*
*   Run the command line that is the concatenation of the string representation
*   of all the arguments.  The function returns TRUE or FALSE depending on the
*   exit status code.  The exact interpretation of TRUE/FALSE from the exit
*   status code is operating-system dependent.  However, generally a exit status
*   code of 0 is interpreted as TRUE, with anything else being FALSE.  The
*   variable EXITSTATUS and the script exit status code are NOT altered.
}
procedure escr_ifun_runtf (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var8192_t;
  tf: boolean;                         {TRUE/FALSE result from the program}
  exstat: sys_sys_exstat_t;            {exit status code from program}

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_ifn_get_strs (e, tk, stat);     {get the concatenated string}
  if sys_error(stat) then return;

  sys_run_wait_stdnone (               {run program, no I/O}
    tk,                                {command line to run}
    tf,                                {TRUE/FALSE result returned by program}
    exstat,                            {exit status returned by program}
    stat);
  if sys_error(stat) then return;

  escr_ifn_ret_bool (e, tf);
  end;
{
********************************************************************************
*
*   Local subroutine THREAD_READ_DISCARD (ARG)
*
*   This routine is run in a separate thread.  ARG is the address of the
*   TH_READ_T structure providing the library state and information about what
*   to read from.
*
*   The thread establishes a connection to a I/O unit.  It then reads and
*   discards all information received.  The thread ends when any error is
*   encountered on reading from the I/O connection, such as end of file or end
*   of pipe.
}
procedure thread_read_discard (        {thread to read and discard data from I/O unit}
  in      arg: sys_int_adr_t);         {address of TH_READ_T structure}
  val_param;

var
  th_p: th_read_p_t;                   {pointer to structure passed from caller}
  conn: file_conn_t;                   {connection to the I/O unit}
  str: string_var80_t;                 {one line read from I/O connection}
  stat: sys_err_t;

begin
  str.max := size_char(str.str);       {init local var string}
  th_p := th_read_p_t(arg);            {get pointer to data for this thread}

  file_open_sysconn_text (             {open connection to the I/O stream}
    th_p^.sysconn,                     {system I/O unit to connect to}
    [file_rw_read_k],                  {we will read from the I/O connection}
    conn,                              {returned I/O connection}
    stat);
  if sys_error(stat) then begin        {error opening I/O connection ?}
    sys_thread_exit;                   {end this thread}
    end;

  while true do begin                  {back here each new line to read}
    file_read_text (conn, str, stat);  {read a line}
    if sys_error(stat) then exit;      {didn't get the line}
    end;                               {back to read the next line}

  file_close (conn);                   {close the connection to the I/O unit}
  end;
{
********************************************************************************
*
*   Local subroutine THREAD_READ_LINE1 (ARG)
*
*   This routine is run in a separate thread.  ARG is the address of the
*   TH_READ_T structure providing the library state and information about what
*   to read from.
*
*   The thread establishes a connection to a I/O unit.  It then reads all text
*   lines from the I/O connection.  The current function return value is set to
*   the first line.  All other lines are discarded.
*
*   The thread ends when any error is encountered on reading from the I/O
*   connection, such as end of file or end of pipe.
}
procedure thread_read_line1 (          {thread to return first line from I/O unit}
  in      arg: sys_int_adr_t);         {address of TH_READ_T structure}
  val_param;

var
  th_p: th_read_p_t;                   {pointer to structure passed from caller}
  conn: file_conn_t;                   {connection to the I/O unit}
  str: string_var8192_t;               {one line read from I/O connection}
  first: boolean;                      {next line is the first}
  stat: sys_err_t;

begin
  str.max := size_char(str.str);       {init local var string}
  th_p := th_read_p_t(arg);            {get pointer to data for this thread}

  file_open_sysconn_text (             {open connection to the I/O stream}
    th_p^.sysconn,                     {system stream to connect to}
    [file_rw_read_k],                  {we will read from the I/O connection}
    conn,                              {returned I/O connection}
    stat);
  if sys_error(stat) then begin        {error opening I/O connection ?}
    sys_thread_exit;                   {end this thread}
    end;

  first := true;                       {init to next line is the first}
  while true do begin                  {back here each new line to read}
    file_read_text (conn, str, stat);  {read a line}
    if sys_error(stat) then exit;      {didn't get the line}
    if first then begin                {this was the first line ?}
      first := false;                  {subsequent lines won't be the first}
      escr_ifn_ret_str (th_p^.e_p^, str); {set function return value}
      end;
    end;                               {back to read the next line}

  file_close (conn);                   {close the connection to the I/O unit}
  end;
{
********************************************************************************
*
*   RUNSO agr ... arg
*
*   Run the command line that is the concatenation of the string representation
*   of all the arguments.
*
*   The function returns the first standard output line emitted by the program.
*   The standard output of the program is otherwise lost, and not copied to the
*   standard output of the script running this function.
*
*   The integer variable EXITSTATUS is set to the exit status code of the
*   program.  This variable is created if it does not already exist.  The script
*   exit status is raised to the program exit status code if it was previously
*   lower.
}
procedure escr_ifun_runso (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  tk: string_var8192_t;
  exstat: sys_sys_exstat_t;            {exit status code from program}
  procid: sys_sys_proc_id_t;           {ID of process running the program}
  th_out: th_read_t;                   {data for thread reading STDOUT}
  th_err: th_read_t;                   {data for thread reading STDERR}
  io_in: sys_sys_file_conn_t;          {system handle to command's STDIN}
  ev: sys_sys_event_id_t;              {scratch system event}

begin
  tk.max := size_char(tk.str);         {init local var string}

  escr_ifn_get_strs (e, tk, stat);     {get the concatenated string}
  if sys_error(stat) then return;
{
*   Run the command in a separate process.
}
  sys_run_stdtalk (                    {run the command}
    tk,                                {command line to run}
    procid,                            {returned ID of new process running the prog}
    io_in,                             {handle to program's STDIN stream}
    th_out.sysconn,                    {handle to program's STDOUT stream}
    th_err.sysconn,                    {handle to program's STDERR stream}
    stat);
  if sys_error(stat) then return;

  file_close_sysconn (io_in);          {program's STDIN is not used}
{
*   Start the threads to read from the STDOUT and STDERR output streams from the
*   new process.
}
  th_out.e_p := addr(e);               {pass pointer to ESCR library use state}
  sys_thread_create (                  {start thread to handle STDOUT from the command}
    addr(thread_read_line1),           {thread routine}
    sys_int_adr_t(addr(th_out)),       {pass address of data for this thread}
    th_out.thid,                       {returned ID of this thread}
    stat);
  th_out.run := not sys_error(stat);   {save whether thread was launched}

  th_err.e_p := addr(e);               {pass pointer to ESCR library use state}
  sys_thread_create (                  {start thread to handle STDOUT from the command}
    addr(thread_read_discard),         {thread routine}
    sys_int_adr_t(addr(th_err)),       {pass address of data for this thread}
    th_err.thid,                       {returned ID of this thread}
    stat);
  th_err.run := not sys_error(stat);   {save whether thread was launched}
{
*   Wait for the command and the I/O read threads to complete.
}
  discard( sys_proc_status (           {wait for command to complete}
    procid,                            {ID of the process}
    true,                              {wait for the process to terminate}
    exstat,                            {exit status from the process}
    stat) );

  while true do begin                  {code block for STDOUT thread exit}
    if not th_out.run then exit;       {thread was never run ?}
    sys_thread_event_get (             {get system event to wait on for thread exit}
      th_out.thid,                     {thread ID}
      ev,                              {returned event}
      stat);
    if sys_error(stat) then exit;
    sys_event_wait (ev, stat);         {wait for the thread to exit}
    sys_event_del_bool (ev);           {done with the event}
    exit;
    end;
  file_close_sysconn (th_out.sysconn); {close system I/O connection}

  while true do begin                  {code block for STDERR thread exit}
    if not th_err.run then exit;       {thread was never run ?}
    sys_thread_event_get (             {get system event to wait on for thread exit}
      th_err.thid,                     {thread ID}
      ev,                              {returned event}
      stat);
    if sys_error(stat) then exit;
    sys_event_wait (ev, stat);         {wait for the thread to exit}
    sys_event_del_bool (ev);           {done with the event}
    exit;
    end;
  file_close_sysconn (th_err.sysconn); {close system I/O connection}
{
*   Update the system state to the exit status code of the command.
}
  escr_exitstatus (e, exstat);         {update EXITSTATUS variable and thread ex stat}
  sys_error_none (stat);               {end the function without error}
  end;
{
********************************************************************************
*
*   FILE action fnam
*
*   Returns information about a file.  The possible action keywords and the
*   resulting function values are:
*
*     TYPE  -  String containing a file type keyword:
*
*       FILE  -  Ordinary file.
*       DIR  -  Directory.
*       LINK  -  Symbolic link.
*       <blank>  -  Object does not exist.
*
*     LEN  -  Integer file length in bytes.
*
*     DTM  -  Time of last modification.
}
procedure escr_ifun_file(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  pick: sys_int_machine_t;             {number of keyword picked from list}
  iflag: file_iflag_k_t;               {ID for which information requested}
  finfo: file_info_t;                  {information about a file system object}
  tk: string_var32_t;                  {scratch token}
  fnam: string_treename_t;             {file name}

label
  missing;

begin
  tk.max := size_char(tk.str);         {init local var strings}
  fnam.max := size_char(fnam.str);

  if not escr_ifn_get_keyw (e, tk, stat) {get action keyword}
    then goto missing;
  string_tkpick80 (tk,
    'TYPE LEN DTM',
    pick);
  case pick of                         {which action is requested ?}
1:  iflag := file_iflag_type_k;
2:  iflag := file_iflag_len_k;
3:  iflag := file_iflag_dtm_k;
otherwise
    escr_ifn_bad_keyw (e, tk, stat);   {set bad keyword error}
    return;
    end;

  if not escr_ifn_get_str (e, fnam, stat) {get the file name}
    then goto missing;

  file_info (                          {get the information about the file}
    fnam,                              {name of object inquiring about}
    [iflag],                           {ID for the information being requested}
    finfo,                             {the returned information}
    stat);
  if                                   {asking for type but not found ?}
     (iflag = file_iflag_type_k) and then {requesting object type ?}
      file_not_found(stat)             {the object does not exist ?}
      then begin
   escr_ifn_ret_empty (e);             {return the empty string}
   return;
   end;
  if sys_error(stat) then return;

  case iflag of                        {which information to return ?}

file_iflag_type_k: begin               {type of file system object}
      case finfo.ftype of              {which type is it ?}
file_type_dir_k: begin                 {directory}
          string_vstring (tk, 'DIR'(0), -1);
          end;
file_type_link_k: begin                {symbolic link}
          string_vstring (tk, 'LINK'(0), -1);
          end;
otherwise                              {anything else}
      string_vstring (tk, 'FILE'(0), -1);
      end;
    escr_ifn_ret_str (e, tk);          {return the keyword string}
    end;                               {end of TYPE case}

file_iflag_len_k: begin                {file length in bytes}
    escr_ifn_ret_int (e, finfo.len);
    end;

file_iflag_dtm_k: begin                {time of last modification}
    escr_ifn_ret_time (e, finfo.modified);
    end;

otherwise
    writeln ('INTERNAL ERROR: Unexpected IFLAG value of ', ord(iflag),
      ' in ESCR_IFUN_FILE.');
    sys_bomb;
    end;
  return;                              {normal return point}

missing:                               {a required argument is missing}
  if sys_error(stat) then return;      {STAT already set to previous error ?}
  escr_ifn_stat_required (e, stat);
  end;
{
********************************************************************************
*
*   QTK arg ... arg
*
*   The concatention of the string representation of all the arguments returned
*   as a single quoted token.
}
procedure escr_ifun_qtk (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  str: string_var8192_t;               {raw string}
  tk: string_var8192_t;                {quoted string as single token}
  ii: sys_int_machine_t;               {loop counter}

begin
  str.max := size_char(str.str);       {init local var strings}
  tk.max := size_char(tk.str);

  escr_ifn_get_strs (e, str, stat);    {get the concatenated string}
  if sys_error(stat) then return;
{
*   Copy the string from STR into TK, converting it to a quoted token at the
*   same time.
}
  tk.str[1] := '"';                    {init TK with the leading quote}
  tk.len := 1;

  for ii := 1 to str.len do begin      {scan the input string}
    string_append1 (tk, str.str[ii]);  {add this char to token}
    if str.str[ii] = '"' then begin    {this is a quote character ?}
      string_append1 (tk, '"');        {double the quote}
      end;
    end;

  string_append1 (tk, '"');            {add trailing quote}

  escr_ifn_ret_str (e, tk);            {return the token}
  end;
