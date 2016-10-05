{   Module that contains intrinsic function routines.  These are the functions
*   that are pre-defined and implemented with compiled code.  Some large
*   intrinsic function routines are in their own modules, called
*   ESCR_IFUN_<name>.  The small intrinsic function routines are collected here.
*
*   All the intrinsic function routines have this interface:
*
*      escr_ifun_<name> (E, STAT)
*
*   The function invocation is in E.FUNARG.  This is the string starting with
*   the function name and including the function arguments.  It does not contain
*   whatever syntax was used to indicate the start and end of the function
*   invocation.  The parse index in FUNARG is set to next character after the
*   delimiter after the function name.  In other words, it is ready to parse the
*   first parameter.  There is no guarantee there are any parameters.  If there
*   are no parameters, then the parse index will be past the end of the string.
*
*   The function routine must write the expansion of the function to E.FUNRET.
*   This string has been initialized to empty.
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
define escr_ifun_evar;
define escr_ifun_lab;
define escr_ifun_v;
define escr_ifun_seq;
%include 'escr2.ins.pas';
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
  string_append (e.funret, tk);        {return the raw characters}
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
    e.funarg.p := e.funarg.p - 1;
    until e.funarg.s.str[e.funarg.p] <> ' ';
  e.funarg.p := e.funarg.p + 2;        {go to first raw string character}

  str.len := 0;                        {init function parameter characters}
  while e.funarg.p <= e.funarg.s.len do begin {copy all characters to STR}
    string_append1 (str, e.funarg.s.str[e.funarg.p]); {copy this char}
    e.funarg.p := e.funarg.p + 1;      {advance to next character}
    end;
  escr_ifn_ret_str (e, str);           {return the characters as a string}
  end;
{
********************************************************************************
*
*   CHAR ccode
*
*   Returns the single-character string containing the character of character
*   code CCODE.
}
procedure escr_ifun_char (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  ccode: sys_int_max_t;                {character code}

begin
  if not escr_ifn_get_int (e, ccode, stat) then begin {get character code into CCODE}
    escr_ifn_stat_required (e, stat);
    return;
    end;

  escr_ifn_ret_char (e, chr(ccode));   {return string containing only this character}
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
*   INT
}
procedure escr_ifun_int (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   FP
}
procedure escr_ifun_fp (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   ENG
}
procedure escr_ifun_eng (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   SHIFTR
}
procedure escr_ifun_shiftr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   SHIFTL
}
procedure escr_ifun_shiftl (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   NOT
}
procedure escr_ifun_not (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   ~
}
procedure escr_ifun_inv (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   AND
}
procedure escr_ifun_and (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   OR
}
procedure escr_ifun_or (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   XOR
}
procedure escr_ifun_xor (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   +
}
procedure escr_ifun_plus (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   -
}
procedure escr_ifun_minus (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   *
}
procedure escr_ifun_times (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   /
}
procedure escr_ifun_divide (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   DIV
}
procedure escr_ifun_div (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   TRUNC
}
procedure escr_ifun_trunc (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   RND
}
procedure escr_ifun_rnd (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   MAX
}
procedure escr_ifun_max (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   MIN
}
procedure escr_ifun_min (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   ABS
}
procedure escr_ifun_abs (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   SQRT
}
procedure escr_ifun_sqrt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   SIN
}
procedure escr_ifun_sin (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   COS
}
procedure escr_ifun_cos (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   TAN
}
procedure escr_ifun_tan (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   PI
}
procedure escr_ifun_pi (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   E
}
procedure escr_ifun_e (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   EXP
}
procedure escr_ifun_exp (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   LOG
}
procedure escr_ifun_log (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   LOG2
}
procedure escr_ifun_log2 (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   RDEG
}
procedure escr_ifun_rdeg (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   DEGR
}
procedure escr_ifun_degr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   <
}
procedure escr_ifun_lt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   <=
}
procedure escr_ifun_le (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   =
}
procedure escr_ifun_eq (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   <>
}
procedure escr_ifun_ne (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   >=
}
procedure escr_ifun_ge (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   >
}
procedure escr_ifun_gt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   NOW
}
procedure escr_ifun_now (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   DATE
}
procedure escr_ifun_date (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   IF
}
procedure escr_ifun_if (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
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
    string_append (e.funret, str_p^);  {return just the raw argument chars}
    end;
  end;
{
********************************************************************************
*
*   SYM
}
procedure escr_ifun_sym (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   EXIST
}
procedure escr_ifun_exist (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   TNAM
}
procedure escr_ifun_tnam (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   LNAM
}
procedure escr_ifun_lnam (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   EVAR
}
procedure escr_ifun_evar (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
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

begin
  end;
{
********************************************************************************
*
*   V
}
procedure escr_ifun_v (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
{
********************************************************************************
*
*   SEQ
}
procedure escr_ifun_seq (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  end;
