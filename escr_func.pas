{   The code in this module performs the processing of the inline function.
*   The mechanics of detecting the inline functions and handling their
*   nested expansions is done in the INLINE module.
}
module escr_func;
define escr_inline_func_init;
define escr_inline_func;
%include '/cognivision_links/dsee_libs/pic/escr.ins.pas';

const
{
*   Physical constants.  Don't mess with these.
}
  pi = 3.14159265358979323846;         {what it sounds like, don't touch}
  e = 2.718281828;                     {ditto}
  pi2 = 2.0 * pi;                      {2 Pi}
  pi2nv = 1.0 / pi2;                   {1 / 2Pi}
  env = 1.0 / e;                       {1 / e}
  ln2 = ln(2.0);                       {natural log of 2}

var
  fnames: string_var1024_t;            {all the inline function names, upper case}
{
****************************************************************************
*
*   Initialize for processing inline functions.  This routine must be called
*   once before the other routines in this module.
}
procedure escr_inline_func_init;       {one-time init for processing inline funcs}
  val_param;

begin
  fnames.max := size_char(fnames.str); {init function names list to empty}
  fnames.len := 0;
{
*   Build the list of function names.
}
  string_append_token (fnames, string_v('FP24I')); {1}
  string_append_token (fnames, string_v('STR')); {2}
  string_append_token (fnames, string_v('QSTR')); {3}
  string_append_token (fnames, string_v('FFTC2')); {4}
  string_append_token (fnames, string_v('FFFREQ')); {5}
  string_append_token (fnames, string_v('SIN')); {6}
  string_append_token (fnames, string_v('COS')); {7}
  string_append_token (fnames, string_v('TAN')); {8}
  string_append_token (fnames, string_v('PI')); {9}
  string_append_token (fnames, string_v('RDEG')); {10}
  string_append_token (fnames, string_v('DEGR')); {11}
  string_append_token (fnames, string_v('+')); {12}
  string_append_token (fnames, string_v('-')); {13}
  string_append_token (fnames, string_v('*')); {14}
  string_append_token (fnames, string_v('/')); {15}
  string_append_token (fnames, string_v('DIV')); {16}
  string_append_token (fnames, string_v('RND')); {17}
  string_append_token (fnames, string_v('TRUNC')); {18}
  string_append_token (fnames, string_v('V')); {19}
  string_append_token (fnames, string_v('ABS')); {20}
  string_append_token (fnames, string_v('EXP')); {21}
  string_append_token (fnames, string_v('LOG2')); {22}
  string_append_token (fnames, string_v('SQRT')); {23}
  string_append_token (fnames, string_v('SHIFTR')); {24}
  string_append_token (fnames, string_v('SHIFTL')); {25}
  string_append_token (fnames, string_v('<')); {26}
  string_append_token (fnames, string_v('<=')); {27}
  string_append_token (fnames, string_v('=')); {28}
  string_append_token (fnames, string_v('>=')); {29}
  string_append_token (fnames, string_v('>')); {30}
  string_append_token (fnames, string_v('<>')); {31}
  string_append_token (fnames, string_v('NOT')); {32}
  string_append_token (fnames, string_v('AND')); {33}
  string_append_token (fnames, string_v('OR')); {34}
  string_append_token (fnames, string_v('XOR')); {35}
  string_append_token (fnames, string_v('UCASE')); {36}
  string_append_token (fnames, string_v('LCASE')); {37}
  string_append_token (fnames, string_v('~')); {38}
  string_append_token (fnames, string_v('SLEN')); {39}
  string_append_token (fnames, string_v('CHAR')); {40}
  string_append_token (fnames, string_v('SUBSTR')); {41}
  string_append_token (fnames, string_v('TNAM')); {42}
  string_append_token (fnames, string_v('LNAM')); {43}
  string_append_token (fnames, string_v('IF')); {44}
  string_append_token (fnames, string_v('CHARS')); {45}
  string_append_token (fnames, string_v('SINDX')); {46}
  string_append_token (fnames, string_v('SEQ')); {47}
  string_append_token (fnames, string_v('NOW')); {48}
  string_append_token (fnames, string_v('DATE')); {49}
  string_append_token (fnames, string_v('EVAR')); {50}
  string_append_token (fnames, string_v('EXIST')); {51}
  string_append_token (fnames, string_v('ARG')); {52}
  string_append_token (fnames, string_v('INT')); {53}
  string_append_token (fnames, string_v('FP')); {54}
  string_append_token (fnames, string_v('FP32F')); {55}
  string_append_token (fnames, string_v('FP24_INT')); {56}
  string_append_token (fnames, string_v('FP32F_INT')); {57}
  string_append_token (fnames, string_v('MAX')); {58}
  string_append_token (fnames, string_v('MIN')); {59}
  string_append_token (fnames, string_v('ENG')); {60}
  string_append_token (fnames, string_v('LOG')); {61}
  string_append_token (fnames, string_v('E')); {62}
  string_append_token (fnames, string_v('CCODE')); {63}
  string_append_token (fnames, string_v('SYM')); {64}
  string_append_token (fnames, string_v('LAB')); {65}
  end;
{
****************************************************************************
*
*   Subroutine INLINE_FUNC (FSTR, LOT)
*
*   Perform the operation indicated by the inline function in FSTR.  The
*   resulting expansion, if any, is appended to LOT.  FSTR contains exactly
*   the function body.  This is the part just inside the "[" and "]".
}
procedure escr_inline_func (           {perform inline function operation}
  in      fstr: univ string_var_arg_t; {function source string, brackets removed}
  in out  lot: string_var8192_t);      {string to append function expansion to}
  val_param;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  pick: sys_int_machine_t;             {number of keyword picked from list}
  p: string_index_t;                   {input string parse index}
  pf: string_index_t;                  {function name parse index and temp string index}
  funn: string_var32_t;                {function name keyword}
  tk, tk2, tk3: string_var8192_t;      {scratch strings}
  a1, a2: sys_fp_max_t;                {scratch floating point function arguments}
  i1, i2: sys_int_max_t;               {scratch integer arguments}
  b1: boolean;                         {scratch boolean arguments}
  b: boolean;                          {scratch boolean}
  comp: sys_compare_k_t;               {result of comparison}
  r: sys_fp_max_t;                     {scratch floating point}
  i, n: sys_int_max_t;                 {scratch integers}
  ii: sys_int_machine_t;               {scratch integer}
  sym_p: sym_p_t;                      {scratch pointer to a ESCR symbol}
  fp24: pic_fp24_t;                    {PIC 24 bit floating point number}
  fp32f: pic_fp32f_t;                  {32 bit fast floating point for dsPICs}
  val, val2: val_t;                    {arguments or other intermediate values}
  time: sys_clock_t;                   {scratch absolute time value}
  tstat: string_tnstat_k_t;            {treename translation result status}
  tzone: sys_tzone_k_t;                {ID for timezone to convert strings for}
  hours_west: real;                    {timezone offset}
  daysave: sys_daysave_k_t;            {daylight savings time strategy}
  date: sys_date_t;                    {expanded date/time descriptor}
  isint: boolean;                      {TRUE for integer arguments}
  dtype: dtype_k_t;                    {scratch data type ID}
  ccond: sys_compare_t;                {set of comparison conditions for TRUE result}
  seqflags: string_seq_t;              {modifier flags for getting sequence number}
  str_p: string_var_p_t;               {scratch string pointer}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  notind, ret_str, ret_r, ret_time_r, ret_time, ret_ir, ret_i, ret_b, ret_bi,
  ret_val, have_seqparm, done_func, error, arg_not_num_time, arg_not_num,
  arg_dtype_bad, arg_bad_tk, arg_error, arg_missing;
{
****************************************
*
*   Function GVAL (VAL)
*
*   Get the next argument value into VAL.  Returns TRUE if argument was available.
}
function gval (                        {get next function argument}
  out     val: val_t)                  {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

begin
  gval := term_get (fstr, p, val);
  end;
{
****************************************
*
*   Function GBOOL (B)
*
*   Get the next arugment as a boolean value into B.  Returns TRUE if argument
*   was available.
}
function gbool (                       {get the next argument as a boolean}
  out     b: boolean)                  {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  gbool := false;                      {init to no argument available}
  if not gval (val) then return;       {no argument ?}
  gbool := true;
  b := val_bool (val);                 {pass back argument value}
  end;
{
****************************************
*
*   Function GINT (I)
*
*   Get the next arugment as a integer value into I.  Returns TRUE if argument
*   was available.
}
function gint (                        {get the next argument as a integer}
  out     i: sys_int_max_t)            {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  gint := false;                       {init to no argument available}
  if not gval (val) then return;       {no argument ?}
  gint := true;
  i := val_int (val);                  {pass back argument value}
  end;
{
****************************************
*
*   Function GTIME (TIME)
*
*   Get the next argument as a time value into TIME.  Returns TRUE if argument
*   was available.
}
function gtime (                       {get next argument as time value}
  out     time: sys_clock_t)           {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  gtime := false;                      {init to no argument available}
  if not gval (val) then return;       {no argument ?}
  gtime := true;
  time := val_time (val);              {pass back argument value}
  end;
{
****************************************
*
*   Function GFP (FP)
*
*   Get the next arugment as a floating point value into FP.  Returns TRUE if
*   argument was available.
}
function gfp (                         {get the next argument as a floating point}
  out     fp: sys_fp_max_t)            {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  gfp := false;                        {init to no argument available}
  if not gval (val) then return;       {no argument ?}
  gfp := true;
  fp := val_fp (val);                  {pass back argument value}
  end;
{
****************************************
*
*   Function GSTR (STR)
*
*   Get the string value of the next argument into STR.  Returns TRUE if argument
*   was available.
}
function gstr (                        {get string value of next argument}
  in out  str: univ string_var_arg_t)  {returned value}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  val: val_t;                          {term value}

begin
  gstr := false;                       {init to no argument available}
  if not gval (val) then return;       {no argument ?}
  gstr := true;
  escr_val_str (val, str);             {pass back argument value}
  end;
{
****************************************
*
*   Function GKEYW (STR)
*
*   Get the next argument as a keyword in STR.  Returns TRUE if argument was
*   available.
}
function gkeyw (                       {get next argument as a keyword}
  in out  str: univ string_var_arg_t)  {returned keyword, always upper case}
  :boolean;                            {TRUE if argument was available}
  val_param;

var
  stat: sys_err_t;

begin
  gkeyw := true;                       {init to got keyword}
  string_token (fstr, p, str, stat);   {get next argument token}
  if string_eos(stat) then begin       {no argument available ?}
    gkeyw := false;
    end;
  string_upcase (str);                 {return keyword in upper case}
  escr_err_atline_abort (stat, '', '', nil, 0);
  end;
{
****************************************
*
*   Subroutine GSTRS (STR)
*
*   Get the concatenation of the string represenation of all remaining arguments.
*   The arguments are guaranteed to be exhausted after this call.
}
procedure gstrs (                      {get string representation of remaining arguments}
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
  if not gstr(tk) then return;         {get string representation of next argument}
  string_append (str, tk);             {append it to end of return string}
  goto loop;                           {back to get next argument}
  end;
{
****************************************
*
*   Subroutine RETURN_STRING (S)
*
*   Return the string in S as a single string argument.
}
procedure return_string (              {write string argument to output}
  in      s: univ string_var_arg_t);   {characters to write as a string argument}
  val_param;

var
  i: sys_int_machine_t;                {string index}
  c: char;                             {scratch character}
  q: char;                             {" or ' quote character to use}

begin
  q := '"';                            {init to default quote char}
  for i := 1 to s.len do begin         {scan the input string}
    c := s.str[i];                     {get this input string character}
    if c = '"' then q := '''';         {use apostrophies if string contains quotes}
    if c = '''' then begin             {found apostrophy in string ?}
      q := '"';                        {use quotes}
      exit;                            {no point scanning further}
      end;
    end;
{
*   Q is the string quoting character to use.
}
  string_append1 (lot, q);             {write leading quote}
  for i := 1 to s.len do begin         {once for each string character}
    c := s.str[i];                     {get this string character}
    if c = q then begin                {this is quote character ?}
      string_append1 (lot, q);         {write quote character twice}
      end;
    string_append1 (lot, c);           {write string character}
    end;
  string_append1 (lot, q);             {write closing quote}
  end;
{
****************************************
*
*   Function COMPARE_STRINGS (V1, V2, COND)
*
*   Perform the comparison of the string representation of V1 compared to the string
*   representation of V2.  The function result is TRUE iff the result of this
*   comparison is included in COND.
}
function compare_strings (             {compare string representation of two values}
  in      v1, v2: val_t;               {input values, V1 will be compared to V2}
  in      cond: sys_compare_t)         {comparison conditions to result in TRUE}
  :boolean;
  val_param;

var
  s1, s2: string_var8192_t;            {string expansion of input values in case needed}
  s1_p, s2_p: string_var8192_p_t;      {pointers to string representations of args}

begin
  s1.max := size_char(s1.str);         {init local var strings}
  s2.max := size_char(s2.str);
  compare_strings := false;            {keep compiler from complaining}

  if v1.dtype = dtype_str_k
    then begin                         {V1 is already string}
      s1_p := addr(v1.str);
      end
    else begin                         {V1 is not string}
      escr_val_str (v1, s1);
      s1_p := addr(s1);
      end
    ;

  if v2.dtype = dtype_str_k
    then begin                         {V2 is already string}
      s2_p := addr(v2.str);
      end
    else begin                         {V2 is not string}
      escr_val_str (v2, s2);
      s2_p := addr(s2);
      end
    ;

  case string_compare (s1_p^, s2_p^) of {what is result of string comparison ?}
-1: compare_strings := sys_compare_lt_k in cond; {V1 < V2}
0:  compare_strings := sys_compare_eq_k in cond; {V1 = V2}
1:  compare_strings := sys_compare_gt_k in cond; {V1 > V2}
    end;
  end;
{
****************************************
*
*   Start of code for subroutine INLINE_FUNC.
}
begin
  funn.max := size_char(funn.str);     {init local var strings}
  tk.max := size_char(tk.str);
  tk2.max := size_char(tk2.str);
  tk3.max := size_char(tk3.str);
  val.str.max := size_char(val.str.str);

  p := 1;                              {init parse index}
  string_token (fstr, p, funn, stat);  {get function name}
  string_unpad (funn);                 {delete trailing spaces from function name}
  if string_eos(stat) or else (funn.len <= 0) then begin {function name is missing ?}
    escr_err_atline ('pic', 'func_name_missing', nil, 0);
    end;
  escr_err_atline_abort (stat, '', '', nil, 0);
  string_upcase (funn);                {make upper case for keyword matching}
{
*   The upper case function name is in FUNN and it is guaranteed to be at
*   least one character long.
*
*   Now check for this is the ASM30 register indirect addressing syntax
*   instead of a preprocessor inline function.  This syntax always starts
*   a W0 - W15 register name, possibly preceded by "++" or "--".
}
  if lang <> lang_dspic_k then goto notind; {wrong language ?}

  pf := 1;                             {init FUNN parse index}
  {
  *   Skip accross any leading "+" or "-" characters.
  }
  while pf <= funn.len do begin        {scan forward}
    if not ((funn.str[pf] = '+') or (funn.str[pf] = '-')) then exit; {not + or - ?}
    pf := pf + 1;                      {advance to the next name character}
    end;                               {back to check next function name character}
  {
  *   Check for remaining name starts with Wn, there N is a 0-9 digit.
  }
  if pf > funn.len then goto notind;   {check for "W"}
  if funn.str[pf] <> 'W' then goto notind;
  pf := pf + 1;

  if pf > funn.len then goto notind;   {check for 0-9 digit}
  if
      (ord(funn.str[pf]) < ord('0')) or
      (ord(funn.str[pf]) > ord('9'))
    then goto notind;
  {
  *   This inline function is really a register indirect syntax.  Restore
  *   the original text to the output string.
  }
  string_append1 (lot, '[');
  string_append (lot, fstr);
  string_append1 (lot, ']');
  return;
notind:                                {not ASM30 register indirect syntax}

  string_tkpick (funn, fnames, pick);  {pick the function name from the list}
  case pick of                         {which function is it ?}
{
********************
*
*   FP24I fp
}
1: begin
  if not gfp(r) then goto arg_missing; {get the floating point value into R}
  fp24 := pic_fp24_f_real (r);         {convert to PIC 24 bit representation}
  case lang of                         {what is the input source language}

lang_aspic_k: begin                    {language is MPASM}
  string_appends (tk, 'h'''(0));       {force HEX format}
  string_f_int8h (tk2, fp24.b2);
  string_append (tk, tk2);
  string_f_int8h (tk2, fp24.b1);
  string_append (tk, tk2);
  string_f_int8h (tk2, fp24.b0);
  string_append (tk, tk2);
  string_append1 (tk, '''');
  end;                                 {end of MPASM language case}

lang_dspic_k: begin                    {language is ASM30}
  string_appends (tk, '0x'(0));        {force HEX format}
  string_f_int8h (tk2, fp24.b2);
  string_append (tk, tk2);
  string_f_int8h (tk2, fp24.b1);
  string_append (tk, tk2);
  string_f_int8h (tk2, fp24.b0);
  string_append (tk, tk2);
  end;                                 {end of ASM30 language case}

otherwise
    err_lang (lang, 'ESCR_FUNC', 1);
    end;
  string_append (lot, tk);
  end;
{
********************
*
*   STR arg ... arg
}
2: begin
  gstrs(tk);                           {get string representation of all arguments}
ret_str:                               {common code to return string in TK}
  return_string (tk);
  end;
{
********************
*
*   QSTR <string>
}
3: begin
  p := p - 1;                          {make sure in delimeter after func name}
  while fstr.str[p] = ' ' do p := p - 1; {find index of last func name char}
  string_substr (fstr, p + 2, fstr.len, tk); {get raw char string into TK}
  string_token_make (tk, val.str);     {convert to a single token in VAL.STR}
  if (val.str.str[1] <> '''') and (val.str.str[1] <> '"') then begin {not quoted ?}
    string_append1 (lot, '"');         {write leading quote}
    string_append1 (val.str, '"');     {add trailing quote to the token}
    end;
  string_append (lot, val.str);        {append the quoted string to the output}
  p := fstr.len + 1;                   {indicate input string fully used}
  end;
{
********************
*
*   FFTC2 tcfilt sfreq
}
4: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);                  {get power of 2 time constant}
  if not term_get (fstr, p, val) then goto arg_missing;
  a2 := val_fp (val);                  {get sampling frequency}

  r := 1.0 - 0.5 ** (1.0 / (a1 * a2)); {make the filter fraction}

ret_r:                                 {common code to return FP value in R}
  escr_str_from_fp (r, tk);            {make floating point string in TK}
  string_append (lot, tk);             {append floating point string to the output}
  end;
{
********************
*
*   FFFREQ ffreq sfreq
}
5: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);                  {get filter rolloff frequency}
  if not term_get (fstr, p, val) then goto arg_missing;
  a2 := val_fp (val);                  {get sampling frequency}

  r := pi2nv / a1;                     {make standard power of E time constant}
  r := 1.0 - env ** (1.0 / (r * a2));  {make the filter fraction}
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   SIN ang
}
6: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);                  {get angle in radians}
  r := sin(a1);                        {compute the function value}
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   COS ang
}
7: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);                  {get angle in radians}
  r := cos(a1);                        {compute the function value}
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   TAN ang
}
8: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);                  {get angle in radians}
  r := sin(a1) / cos(a1);              {compute the function value}
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   PI
}
9: begin                               {PI}
  r := pi;
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   RDEG ang
}
10: begin                              {RDEG}
  if not term_get (fstr, p, val) then goto arg_missing;
  r := val_fp(val) * 180.0 / pi;
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   DEGR ang
}
11: begin                              {DEGR}
  if not term_get (fstr, p, val) then goto arg_missing;
  r := val_fp(val) * pi / 180.0;
  goto ret_r;                          {return the value in R}
  end;
{
********************
*
*   + arg1 arg2 ... argN
}
12: begin                              {+}
  i := 0;                              {init the result}
  dtype := dtype_int_k;                {init result data type}
  while term_get (fstr, p, val) do begin {loop once for each argument}
    case val.dtype of                  {what is data type of this argument}

dtype_int_k: begin                     {argument is integer}
        case dtype of                  {what is current output data type ?}
dtype_int_k: begin                     {INTEGER + INTEGER}
            i := i + val.int;
            end;
dtype_fp_k,                            {REAL + INTEGER}
dtype_time_k: begin                    {TIME + INTEGER}
            r := r + val.int;
            end;
otherwise
          goto arg_not_num_time;
          end;
        end;

dtype_fp_k: begin                      {argument is REAL}
        case dtype of                  {what is current output data type ?}
dtype_int_k: begin                     {INTEGER + REAL}
            r := i + val.fp;
            dtype := dtype_fp_k;
            end;
dtype_fp_k: begin                      {REAL + REAL}
            r := r + val.fp;
            end;
dtype_time_k: begin                    {TIME + REAL}
            r := r + val.fp;
            end;
otherwise
          goto arg_not_num_time;
          end;
        end;

dtype_time_k: begin                    {argument is TIME}
        case dtype of                  {what is current output data type ?}
dtype_int_k: begin                     {INTEGER + TIME}
            r := i;
            end;
dtype_fp_k: ;                          {REAL + TIME}
dtype_time_k: begin                    {TIME + TIME}
            escr_err_atline ('pic', 'time_time', nil, 0);
            end;
otherwise
          goto arg_not_num_time;
          end;
        time := val.time;
        dtype := dtype_time_k;
        end;

otherwise                              {unexpected argument data type}
      goto arg_not_num_time;
      end;                             {end of argument data type cases}
    end;                               {back to get next argument}

  case dtype of                        {what is result data type ?}
dtype_int_k: goto ret_i;               {INTEGER}
dtype_fp_k: goto ret_r;                {REAL}
dtype_time_k: goto ret_time_r;         {TIME}
    end;
  sys_msg_parm_int (msg_parm[1], ord(dtype));
  sys_msg_parm_str (msg_parm[2], 'ESCR_FUNC, function "+"');
  escr_err_atline ('pic', 'err_dtype_unimp', msg_parm, 2);
{
*   Common return points for various data types.  These are jumped to at the
*   end of other functions to return specific data types.
}
ret_time_r:                            {return time value in TIME plus seconds in R}
  time := sys_clock_add (time, sys_clock_from_fp_rel(r)); {make final time}
ret_time:                              {return time value in TIME}
  escr_str_from_time (time, tk);       {make time string in TK}
  string_append (lot, tk);             {return it}
  goto done_func;                      {done processing this function}

ret_ir:                                {common code to return I or R dep on ISINT}
  if not isint then goto ret_r;        {return floating point value in R ?}

ret_i:                                 {common code to return integer value in I}
  string_f_int_max (tk, i);            {make integer string in TK}
  string_append (lot, tk);             {append the integer string to the output}
  end;
{
********************
*
*   - arg1 arg2
}
13: begin                              {-}
  if not term_get (fstr, p, val) then goto arg_missing;
  case val.dtype of                    {what is data type of first argument ?}

dtype_int_k: begin                     {INTEGER first arg}
      i := val.int;
      if not term_get (fstr, p, val) then goto arg_missing;
      case val.dtype of
dtype_int_k: begin                     {INTEGER - INTEGER}
          i := i - val.int;
          goto ret_i;
          end;
dtype_fp_k: begin                      {INTEGER - REAL}
          r := i - val.fp;
          goto ret_r;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      r := val.fp;
      if not term_get (fstr, p, val) then goto arg_missing;
      case val.dtype of
dtype_int_k: begin                     {REAL - INTEGER}
          r := r - val.int;
          goto ret_r;
          end;
dtype_fp_k: begin                      {REAL - REAL}
          r := r - val.fp;
          goto ret_r;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      time := val.time;
      if not term_get (fstr, p, val) then goto arg_missing;
      case val.dtype of
dtype_int_k: begin                     {TIME - INTEGER}
          r := -val.int;
          goto ret_time_r;
          end;
dtype_fp_k: begin                      {TIME - REAL}
          r := -val.fp;
          goto ret_time_r;
          end;
dtype_time_k: begin                    {TIME - TIME}
          time := sys_clock_sub (time, val.time); {make relative time difference}
          r := sys_clock_to_fp2 (time); {convert to seconds}
          goto ret_r;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
    goto arg_not_num_time;
    end;                               {end of first argument data type cases}
  end;
{
********************
*
*   * arg1 arg2 ... argN
}
14: begin                              {*}
  i := 1;                              {init the result}
  isint := true;                       {init to data type is integer}
  while term_get (fstr, p, val) do begin {loop once for each argument}
    if not val_isint (val, i1, a1) then begin {this argument is floating point ?}
      if isint then begin              {all was integer so far ?}
        r := i;                        {convert to floating point}
        isint := false;
        end;
      end;
    if isint
      then begin                       {do integer computation}
        i := i * i1;
        end
      else begin                       {do floating point computation}
        r := r * a1;
        end
      ;
    end;                               {back to get next argument}
  goto ret_ir;                         {ret integer I or float R depending on ISINT}
  end;
{
********************
*
*   / arg1 arg2
}
15: begin                              {/}
  if not term_get (fstr, p, val) then goto arg_missing;
  r := val_fp (val);                   {get numerator}
  if not term_get (fstr, p, val) then goto arg_missing;
  r := r / val_fp(val);                {get denominator and compute result}
  goto ret_r;                          {return the floating point value in R}
  end;
{
********************
*
*   DIV arg1 arg2
}
16: begin                              {DIV}
  if not term_get (fstr, p, val) then goto arg_missing;
  i := val_int (val);                  {get numerator}
  if not term_get (fstr, p, val) then goto arg_missing;
  i := i div val_int(val);             {get denominator and compute the result}
  goto ret_i;                          {return the integer value in I}
  end;
{
********************
*
*   RND arg
}
17: begin                              {RND}
  if not term_get (fstr, p, val) then goto arg_missing;
  if not val_isint (val, i, r) then begin {argument is floating point ?}
    i := round(val_fp(val));
    end;
  goto ret_i;                          {return the integer value in I}
  end;
{
********************
*
*   TRUNC arg
}
18: begin                              {TRUNC}
  if not term_get (fstr, p, val) then goto arg_missing;
  if not val_isint (val, i, r) then begin {argument is floating point ?}
    i := trunc(val_fp(val));
    end;
  goto ret_i;                          {return the integer value in I}
  end;
{
********************
*
*   V value
}
19: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  escr_val_text (val, tk);             {convert to native assembler format string}
  string_append (lot, tk);             {write to the output string}
  end;
{
********************
*
*   ABS arg
}
20: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  if val_isint (val, i1, a1)
    then begin                         {argument is integer}
      i := abs(i1);
      goto ret_i;
      end
    else begin                         {argument is floating point}
      r := abs(a1);
      goto ret_r;
      end
    ;
  end;
{
********************
*
*   EXP arg1 arg2
}
21: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  if val_isint (val, i1, a1)
    then begin                         {ARG1 is integer in I1}
      if not term_get (fstr, p, val) then goto arg_missing;
      if val_isint (val, i2, a1)
        then begin                     {ARG1 integer in I1, ARG2 integer in I2}
          i := i1 ** i2;
          goto ret_i;
          end
        else begin                     {ARG1 integer in I1, ARG2 float in A1}
          if i1 < 0                    {negative value to floating point exponent ?}
            then escr_err_atline ('pic', 'exp_neg_fp', nil, 0);
          r := i1 ** a1;
          goto ret_r;
          end
        ;
      end
    else begin                         {ARG1 is floating point in A1}
      if not term_get (fstr, p, val) then goto arg_missing;
      if val_isint (val, i1, a2)
        then begin                     {ARG1 fp in A1, ARG2 integer in I1}
          r := a1 ** i1;
          end
        else begin                     {ARG1 fp in A1, ARG2 fp in A2}
          if a1 < 0.0                  {negative value to floating point exponent ?}
            then escr_err_atline ('pic', 'exp_neg_fp', nil, 0);
          r := a1 ** a2;
          end
        ;
      goto ret_r;
      end
    ;
  end;
{
********************
*
*   LOG2 arg
}
22: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);
  if a1 <= 0.0
    then escr_err_atline ('pic', 'log_neg', nil, 0);
  r := ln(a1) / ln2;
  goto ret_r;
  end;
{
********************
*
*   SQRT arg
}
23: begin
  if not gfp (a1) then goto arg_missing;
  if a1 < 0.0
    then escr_err_atline ('pic', 'sqrt_neg', nil, 0);
  r := sqrt(a1);
  goto ret_r;
  end;
{
********************
*
*   SHIFTR value bits
}
24: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  i1 := val_int (val);                 {get VALUE}
  if not term_get (fstr, p, val) then goto arg_missing;
  i2 := val_int (val);                 {get BITS}
  i := rshft(i1, i2);
  goto ret_i;
  end;
{
********************
*
*   SHIFTL value bits
}
25: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  i1 := val_int (val);                 {get VALUE}
  if not term_get (fstr, p, val) then goto arg_missing;
  i2 := val_int (val);                 {get BITS}
  i := lshft(i1, i2);
  goto ret_i;
  end;
{
********************
*
*   < val1 val2
}
26: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_lt_k];         {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int < val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int < val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp < val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp < val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

ret_b:                                 {return the boolean value in B}
  if b
    then string_appends (lot, 'TRUE'(0))
    else string_appends (lot, 'FALSE'(0));
  end;
{
********************
*
*   <= val1 val2
}
27: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_lt_k, sys_compare_eq_k]; {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int <= val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int <= val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp <= val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp <= val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   = val1 val2
}
28: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_eq_k];         {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_bool_k: begin                    {BOOL first argument}
      case val2.dtype of
dtype_bool_k: begin                    {BOOL, BOOL}
          b := val.bool = val2.bool;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int = val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int = val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp = val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp = val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   >= val1 val2
}
29: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_gt_k, sys_compare_eq_k]; {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int >= val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int >= val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp >= val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp >= val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   > val1 val2
}
30: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_gt_k];         {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int > val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int > val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp > val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp > val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   <> val1 val2
}
31: begin
  if not gval (val) then goto arg_missing; {get first function parameter}
  if not gval (val2) then goto arg_missing; {get second function parameter}
  ccond := [sys_compare_lt_k, sys_compare_gt_k]; {comparison conditions for TRUE}

  if                                   {at least one argument is a string ?}
      (val.dtype = dtype_str_k) or
      (val2.dtype = dtype_str_k)
      then begin
    b := compare_strings (val, val2, ccond); {do string compare}
    goto ret_b;                        {return the boolean value in B}
    end;

  case val.dtype of                    {what is data type of first argument ?}

dtype_bool_k: begin                    {BOOL first argument}
      case val2.dtype of
dtype_bool_k: begin                    {BOOL, BOOL}
          b := val.bool <> val2.bool;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_int_k: begin                     {INTEGER first argument}
      case val2.dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
          b := val.int <> val2.int;
          end;
dtype_fp_k: begin                      {INTEGER, REAL}
          b := val.int <> val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_fp_k: begin                      {REAL first arg}
      case val2.dtype of
dtype_int_k: begin                     {REAL, INTEGER}
          b := val.fp <> val2.int;
          end;
dtype_fp_k: begin                      {REAL, REAL}
          b := val.fp <> val2.fp;
          end;
otherwise
        goto arg_not_num;
        end;
      end;

dtype_time_k: begin                    {TIME first arg}
      case val2.dtype of
dtype_time_k: begin                    {TIME, TIME}
          comp := sys_clock_compare (val.time, val2.time);
          b := comp in ccond;
          end;
otherwise
        goto arg_dtype_bad;
        end;
      end;

otherwise
      goto arg_dtype_bad;
      end;                             {end of first argument data type cases}

  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   NOT
}
32: begin
  if not gbool (b) then goto arg_missing;
  b := not b;
  goto ret_b;                          {return boolean value in B}
  end;
{
********************
*
*   AND
}
33: begin
  if not gval (val) then goto arg_missing; {get first argument}
  if val_isbool (val, b)
    then begin                         {first term is boolean}
      isint := false;
      end
    else begin
      i := val_int (val);
      isint := true;
      end
    ;
  n := 1;                              {init number of arguments read so far}

  while gval (val) do begin            {once for each remaining argument}
    if isint
      then begin                       {integer}
        i := i & val_int (val);
        end
      else begin                       {boolean}
        b := b and val_bool (val);
        end
      ;
    n := n + 1;                        {count one more argument read}
    end;                               {back for next argument}
  if n < 2 then goto arg_missing;      {at least two arguments are required}

ret_bi:                                {return boolean or integer depending on ISINT}
  if isint
    then goto ret_i
    else goto ret_b;
  end;
{
********************
*
*   OR
}
34: begin
  if not gval (val) then goto arg_missing; {get first argument}
  if val_isbool (val, b)
    then begin                         {first term is boolean}
      isint := false;
      end
    else begin
      i := val_int (val);
      isint := true;
      end
    ;
  n := 1;                              {init number of arguments read so far}

  while gval (val) do begin            {once for each remaining argument}
    if isint
      then begin                       {integer}
        i := i ! val_int (val);
        end
      else begin                       {boolean}
        b := b or val_bool (val);
        end
      ;
    n := n + 1;                        {count one more argument read}
    end;                               {back for next argument}
  if n < 2 then goto arg_missing;      {at least two arguments are required}
  goto ret_bi;                         {return INT or BOOL depending on ISINT}
  end;
{
********************
*
*   XOR
}
35: begin
  if not gval (val) then goto arg_missing; {get first argument}
  if val_isbool (val, b)
    then begin                         {first term is boolean}
      isint := false;
      end
    else begin
      i := val_int (val);
      isint := true;
      end
    ;
  n := 1;                              {init number of arguments read so far}

  while gval (val) do begin            {once for each remaining argument}
    if isint
      then begin                       {integer}
        i := xor(i, val_int (val));
        end
      else begin                       {boolean}
        b1 := val_bool (val);
        b := (b and (not b1)) or ((not b) and b1);
        end
      ;
    n := n + 1;                        {count one more argument read}
    end;                               {back for next argument}
  if n < 2 then goto arg_missing;      {at least two arguments are required}
  goto ret_bi;                         {return INT or BOOL depending on ISINT}
  end;
{
********************
*
*   UCASE str
}
36: begin
  if not gstr (tk) then goto arg_missing;
  string_upcase (tk);
  goto ret_str;
  end;
{
********************
*
*   LCASE str
}
37: begin
  if not gstr (tk) then goto arg_missing;
  string_downcase (tk);
  goto ret_str;
  end;
{
********************
*
*   ~ ival
}
38: begin
  if not gint (i) then goto arg_missing;
  i := ~i;
  goto ret_i;
  end;
{
********************
*
*   SLEN str
}
39: begin
  if not gstr (tk) then goto arg_missing;
  i := tk.len;
  goto ret_i;
  end;
{
********************
*
*   CHAR ival
}
40: begin
  if not gint (i) then goto arg_missing;
  tk.str[1] := chr(i);
  tk.len := 1;
  goto ret_str;
  end;
{
********************
*
*   SUBSTR st ln str
}
41: begin
  if not gint (i1) then goto arg_missing;
  if not gint (i2) then goto arg_missing;
  if not gstr (tk2) then goto arg_missing;
  string_substr (tk2, i1, i1 + i2 - 1, tk);
  goto ret_str;
  end;
{
********************
*
*   TNAM str
}
42: begin
  if not gstr (tk2) then goto arg_missing;
  string_treename (tk2, tk);
  goto ret_str;
  end;
{
********************
*
*   LNAM str suff ... suff
}
43: begin
  if not gstr (tk) then goto arg_missing;

  tk2.len := 0;                        {init list of suffixes to empty}
  while gstr (tk3) do begin            {once for each suffix argument}
    string_append_token (tk2, tk3);    {add this suffix to the list}
    end;
  string_terminate_null (tk2);         {make sure STR is null-terminated}

  string_generic_fnam (tk, tk2.str, tk3); {make generic leafname}
  return_string (tk3);
  end;
{
********************
*
*   IF bool val1 val2
}
44: begin
  if not gbool (b) then goto arg_missing;
  if b
    then begin                         {conditional is TRUE}
      if not gval (val) then goto arg_missing; {get the parameter to pass on}
      if not gval (val2) then goto arg_missing; {get the parameter to ignore}
      end
    else begin                         {conditional is FALSE}
      if not gval (val2) then goto arg_missing; {get the parameter to ignore}
      if not gval (val) then goto arg_missing; {get the parameter to pass on}
      end
    ;

ret_val:                               {return value in VAL in its data type}
  case val.dtype of                    {what data type is it ?}
dtype_bool_k: begin
      b := val.bool;
      goto ret_b;
      end;
dtype_int_k: begin
      i := val.int;
      goto ret_i;
      end;
dtype_fp_k: begin
      r := val.fp;
      goto ret_r;
      end;
dtype_str_k: begin
      return_string (val.str);
      end;
dtype_time_k: begin
      time := val.time;
      goto ret_time;
      end;
otherwise                              {unexpected data type}
    sys_msg_parm_int (msg_parm[1], ord(val.dtype));
    sys_msg_parm_str (msg_parm[2], 'ESCR_FUNC, function IF');
    escr_err_atline ('pic', 'err_dtype_unimp', msg_parm, 2);
    end;
  end;
{
********************
*
*   CHARS arg ... arg
}
45: begin
  gstrs (tk);                          {get concatenated string rep of all args}
  string_append (lot, tk);
  end;
{
********************
*
*   SINDX ind str
}
46: begin
  if not gint (i1) then goto arg_missing;
  if not gstr (tk) then goto arg_missing;
  if (i1 > 0) and (i1 <= tk.len)
    then begin                         {requested character exists}
      tk2.str[1] := tk.str[i1];
      tk2.len := 1;
      end
    else begin                         {outside of input string, return empty string}
      tk2.len := 0;
      end
    ;
  return_string (tk2);
  end;
{
********************
*
*   SEQ fnam [aft] [incr [start]]
}
47: begin
  if not gstr (tk) then goto arg_missing;
  seqflags := [];                      {init modifier flags}
  i1 := 1;                             {init increment}
  i2 := 1;                             {init default starting value}

  pf := p;                             {save parse index before looking for "AFT" keyword}
  string_token (fstr, p, tk2, stat);   {get next token}
  if string_eos(stat) then goto have_seqparm;
  if sys_error(stat) then goto arg_error;
  string_upcase (tk2);                 {make upper case for keyword matching}
  if string_equal (tk2, string_v('AFT'(0)))
    then begin                         {is "AFT" parameter}
      seqflags := seqflags + [string_seq_after_k];
      end
    else begin                         {TK2 contains INCR parameter}
      p := pf;                         {restore parse index to before this token}
      end
    ;

  if not gint(i) then goto have_seqparm; {get increment value if available}
  i1 := i;

  if not gint(i) then goto have_seqparm; {get default start value if available}
  i2 := i;

have_seqparm:                          {parameters I1, I2, and SEQFLAGS all set}
  i := string_seq_get (tk, i1, i2, seqflags, stat);
  escr_err_atline_abort (stat, '', '', nil, 0);
  goto ret_i;
  end;
{
********************
*
*   NOW
}
48: begin
  escr_str_from_time (sys_clock, tk);
  string_append (lot, tk);
  end;
{
********************
*
*   DATE time [LOCAL] field arg ... arg
}
49: begin
  if not gtime (time) then goto arg_missing;
  tzone := sys_tzone_cut_k;            {init for coordinated universal time}
  hours_west := 0.0;
  daysave := sys_daysave_no_k;

  string_token (fstr, p, tk, stat);    {get first keyword}
  if string_eos(stat) then goto arg_missing;
  if sys_error(stat) then goto arg_error;
  string_upcase (tk);                  {make upper case for keyword matching}
  if string_equal (tk, string_v('LOCAL'(0))) then begin {LOCAL keyword ?}
    sys_timezone_here (                {set date converion for local time zone}
      tzone, hours_west, daysave);
    string_token (fstr, p, tk, stat);  {get field name keyword}
    if string_eos(stat) then goto arg_missing;
    if sys_error(stat) then goto arg_error;
    string_upcase (tk);
    end;

  sys_clock_to_date (                  {make expanded date descriptor from the time}
    time,                              {input time}
    tzone, hours_west, daysave,        {information about timezone to convert into}
    date);                             {returned expanded date/time descriptor}

  string_tkpick80 (tk,                 {pick field name keyword from list}
    'YEAR MNUM MONTH MON DAY DAYWK DWK HOUR MIN SEC SECF',
    pick);
  case pick of
1:  begin                              {YEAR}
      sys_date_string (date, sys_dstr_year_k, 4, tk2, stat);
      end;
2:  begin                              {MNUM}
      sys_date_string (date, sys_dstr_mon_k, 2, tk2, stat);
      end;
3:  begin                              {MONTH}
      sys_date_string (date, sys_dstr_mon_name_k, string_fw_freeform_k, tk2, stat);
      end;
4:  begin                              {MON}
      sys_date_string (date, sys_dstr_mon_abbr_k, string_fw_freeform_k, tk2, stat);
      end;
5:  begin                              {DAY}
      sys_date_string (date, sys_dstr_day_k, 2, tk2, stat);
      end;
6:  begin                              {DAYWK}
      sys_date_string (date, sys_dstr_daywk_name_k, string_fw_freeform_k, tk2, stat);
      end;
7:  begin                              {DWK}
      sys_date_string (date, sys_dstr_daywk_abbr_k, string_fw_freeform_k, tk2, stat);
      end;
8:  begin                              {HOUR}
      sys_date_string (date, sys_dstr_hour_k, 2, tk2, stat);
      end;
9:  begin                              {MIN}
      sys_date_string (date, sys_dstr_min_k, 2, tk2, stat);
      end;
10: begin                              {SEC}
      sys_date_string (date, sys_dstr_sec_k, 2, tk2, stat);
      end;
11: begin                              {SECF n}
      if not gint (i1) then goto arg_missing;
      sys_date_string (date, sys_dstr_sec_frac_k, i1+3, tk2, stat);
      end;
otherwise
    goto arg_bad_tk;
    end;

  return_string (tk2);
  end;
{
********************
*
*   EVAR name
}
50: begin
  if not gstr (tk) then goto arg_missing;
  sys_envvar_get (tk, tk2, stat);
  if sys_error(stat) then begin
    tk2.len := 0;                      {pass back empty string on any error}
    sys_error_none (stat);             {don't indicate hard error}
    end;
  return_string (tk2);
  end;
{
********************
*
*   EXIST name nametype
}
51: begin
  if not gstr(tk2) then goto arg_missing; {get the symbol name in TK2}

  pick := 0;                           {init to no NAMETYPE keyword specified}
  if gkeyw(tk)                         {get NAMETYPE keyword into TK if available}
    then begin
      string_tkpick80 (tk,             {pick keyword from list}
        'PSYM EVAR FNAM FNAMNL ARG',
        pick);
      if pick = 0 then goto arg_bad_tk; {keyword is invalid ?}
      end
    else begin                         {no name type keyword}
      pick := 1;                       {indicate default of ESCR symbol}
      end
    ;

  b := false;                          {init to symbol does not exist}
  if tk2.len = 0 then goto ret_b;
  case pick of                         {what type of symbol to look for ?}

1:  begin                              {PSYM, any escr symbol}
      escr_sym_find (tk2, sym_p);
      b := sym_p <> nil;
      end;

2:  begin                              {EVAR}
      sys_envvar_get (tk2, tk, stat);
      b := not sys_stat_match (sys_subsys_k, sys_stat_envvar_noexist_k, stat);
      end;

3:  begin                              {FNAM}
      string_treename_opts (           {translate file system pathname}
        tk2,                           {input pathname}
        [ string_tnamopt_flink_k,      {follow symbolic links}
          string_tnamopt_remote_k,     {continue on remote systems as needed}
          string_tnamopt_proc_k,       {translate from point of view of this process}
          string_tnamopt_native_k],    {return native system name, not Embed portable}
        tk3,                           {resulting pathname}
        tstat);
      if tstat <> string_tnstat_native_k then goto ret_b; {couldn't get native pathname ?}
      b := file_exists (tk3);
      end;

4:  begin                              {FNAMNL}
      string_treename_opts (           {translate file system pathname}
        tk2,                           {input pathname}
        [ string_tnamopt_proc_k,       {translate from point of view of this process}
          string_tnamopt_native_k],    {return native system name, not Embed portable}
        tk3,                           {resulting pathname}
        tstat);
      if tstat <> string_tnstat_native_k then goto ret_b; {couldn't get native pathname ?}
      b := file_exists (tk3);
      end;

5:  begin                              {ARG}
      string_t_int (tk2, ii, stat);    {make argument number}
      escr_err_atline_abort (stat, 'pic', 'term_not_int', nil, 0);
      escr_exblock_arg_get (ii, str_p); {get pointer to argument value}
      b := str_p <> nil;               {TRUE if argument exists}
      end;

    end;                               {end of NAMETYPE keyword cases}

  goto ret_b;                          {return TRUE/FALSE according to B}
  end;
{
********************
*
*   ARG n
}
52: begin
  if not gint(i) then goto arg_missing; {get the argument number into I}
  escr_exblock_arg_get (i, str_p);     {get pointer to the indexed argument}
  if str_p <> nil then begin           {argument string exists}
    string_append (lot, str_p^);       {expand to just the raw argument chars}
    end;
  end;
{
********************
*
*   INT i fmt
*
*   Write integer value of I according to the format string FMT.
}
53: begin
  if not gint(i) then goto arg_missing; {get the integer value into I}
  gstrs (tk2);                         {get string concatenation of remaining arguments}
  escr_format_int (i, tk2, tk, stat);
  if sys_error(stat) then goto error;
  goto ret_str;                        {return the string in TK}
  end;
{
********************
*
*   FP r fmt
*
*   Write floating point value of R according to the format string FMT.
}
54: begin
  if not gfp(r) then goto arg_missing; {get the floating point value into R}
  gstrs (tk2);                         {get string concatenation of remaining arguments}
  escr_format_fp (r, tk2, tk, stat);
  if sys_error(stat) then goto error;
  goto ret_str;                        {return the string in TK}
  end;
{
********************
*
*   FP32F fp
}
55: begin
  if not gfp(r) then goto arg_missing; {get the floating point value into R}
  fp32f := pic_fp32f_f_real (r);       {convert to 32 bit fast dsPIC floating point}
  tk.len := 0;                         {init returned string to empty}
  case lang of                         {what is the input source language}

lang_aspic_k: begin                    {language is MPASM}
  string_appends (tk, 'h'''(0));       {force HEX format}
  string_f_int16h (tk2, fp32f.w1);     {write high word in HEX}
  string_append (tk, tk2);
  string_f_int16h (tk2, fp32f.w0);     {write low word in HEX}
  string_append (tk, tk2);
  string_append1 (tk, '''');
  end;                                 {end of MPASM language case}

lang_dspic_k: begin                    {language is ASM30}
  string_appends (tk, '0x'(0));        {force HEX format}
  string_f_int16h (tk2, fp32f.w1);     {write high word in HEX}
  string_append (tk, tk2);
  string_f_int16h (tk2, fp32f.w0);     {write low word in HEX}
  string_append (tk, tk2);
  end;                                 {end of ASM30 language case}

otherwise
    err_lang (lang, 'ESCR_FUNC', 1);
    end;
  string_append (lot, tk);
  end;
{
********************
*
*   FP24_INT fp
}
56: begin
  if not gfp(r) then goto arg_missing; {get the floating point value into R}
  fp24 := pic_fp24_f_real (r);         {convert to PIC 24 bit representation}
  i := fp24.b0;
  i := i ! lshft(fp24.b1, 8);
  i := i ! lshft(fp24.b2, 16);
  goto ret_i;
  end;
{
********************
*
*   FP32F_INT fp
}
57: begin
  if not gfp(r) then goto arg_missing; {get the floating point value into R}
  fp32f := pic_fp32f_f_real (r);       {convert to 32 bit fast dsPIC floating point}
  i := fp32f.w0;
  i := i ! lshft(fp32f.w1, 16);
  goto ret_i;
  end;
{
********************
*
*   MAX arg ... arg
}
58: begin                              {MAX}
  if not term_get (fstr, p, val) then begin {get the first term}
    sys_msg_parm_str (msg_parm[1], 'MAX');
    escr_err_atline ('pic', 'func_arg_missing', msg_parm, 1);
    end;
  dtype := val.dtype;                  {init returned data type to first argument}
  case val.dtype of                    {what is data type of first argument ?}
dtype_int_k: begin                     {first arg is INTEGER}
      i := val.int;
      end;
dtype_fp_k: begin                      {first arg is REAL}
      r := val.fp;
      end;
dtype_time_k: begin                    {first arg is TIME}
      time := val.time;
      end;
otherwise
    goto arg_not_num_time;             {invalid first argument data type}
    end;

  while term_get (fstr, p, val) do begin {loop once for each subsequent argument}
    case val.dtype of                  {what is data type of this argument ?}

dtype_int_k: begin                     {... INTEGER}
        case dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
            i := max(i, val.int);
            end;
dtype_fp_k: begin                      {REAL, INTEGER}
            r := max(r, val.int);
            end;
otherwise
          goto arg_dtype_bad;
          end;
        end;

dtype_fp_k: begin                      {... REAL}
        case dtype of
dtype_int_k: begin                     {INTEGER, REAL}
            r := max(i, val.fp);
            dtype := dtype_fp_k;
            end;
dtype_fp_k: begin                      {REAL, REAL}
            r := max(r, val.fp);
            end;
otherwise
          goto arg_dtype_bad;
          end;
        end;

dtype_time_k: begin                    {... TIME}
        case dtype of
dtype_time_k: begin                    {TIME, TIME}
            if sys_clock_compare (val.time, time) = sys_compare_gt_k then begin
              time := val.time;
              end;
            end;
otherwise
          goto arg_not_num;
          end;
        end;

otherwise                              {unexpected argument data type}
      goto arg_dtype_bad;
      end;                             {end of argument data type cases}
    end;                               {back to get next argument}

  case dtype of                        {what is result data type ?}
dtype_int_k: goto ret_i;               {INTEGER}
dtype_fp_k: goto ret_r;                {REAL}
dtype_time_k: goto ret_time;           {TIME}
    end;
  sys_msg_parm_int (msg_parm[1], ord(dtype));
  sys_msg_parm_str (msg_parm[2], 'ESCR_FUNC, function "MAX"');
  escr_err_atline ('pic', 'err_dtype_unimp', msg_parm, 2);
  end;
{
********************
*
*   MIN arg ... arg
}
59: begin                              {MIN}
  if not term_get (fstr, p, val) then begin {get the first term}
    sys_msg_parm_str (msg_parm[1], 'MIN');
    escr_err_atline ('pic', 'func_arg_missing', msg_parm, 1);
    end;
  dtype := val.dtype;                  {init returned data type to first argument}
  case val.dtype of                    {what is data type of first argument ?}
dtype_int_k: begin                     {first arg is INTEGER}
      i := val.int;
      end;
dtype_fp_k: begin                      {first arg is REAL}
      r := val.fp;
      end;
dtype_time_k: begin                    {first arg is TIME}
      time := val.time;
      end;
otherwise
    goto arg_not_num_time;             {invalid first argument data type}
    end;

  while term_get (fstr, p, val) do begin {loop once for each subsequent argument}
    case val.dtype of                  {what is data type of this argument ?}

dtype_int_k: begin                     {... INTEGER}
        case dtype of
dtype_int_k: begin                     {INTEGER, INTEGER}
            i := min(i, val.int);
            end;
dtype_fp_k: begin                      {REAL, INTEGER}
            r := min(r, val.int);
            end;
otherwise
          goto arg_dtype_bad;
          end;
        end;

dtype_fp_k: begin                      {... REAL}
        case dtype of
dtype_int_k: begin                     {INTEGER, REAL}
            r := min(i, val.fp);
            dtype := dtype_fp_k;
            end;
dtype_fp_k: begin                      {REAL, REAL}
            r := min(r, val.fp);
            end;
otherwise
          goto arg_dtype_bad;
          end;
        end;

dtype_time_k: begin                    {... TIME}
        case dtype of
dtype_time_k: begin                    {TIME, TIME}
            if sys_clock_compare (val.time, time) = sys_compare_lt_k then begin
              time := val.time;
              end;
            end;
otherwise
          goto arg_not_num;
          end;
        end;

otherwise                              {unexpected argument data type}
      goto arg_dtype_bad;
      end;                             {end of argument data type cases}
    end;                               {back to get next argument}

  case dtype of                        {what is result data type ?}
dtype_int_k: goto ret_i;               {INTEGER}
dtype_fp_k: goto ret_r;                {REAL}
dtype_time_k: goto ret_time;           {TIME}
    end;
  sys_msg_parm_int (msg_parm[1], ord(dtype));
  sys_msg_parm_str (msg_parm[2], 'ESCR_FUNC, function "MIN"');
  escr_err_atline ('pic', 'err_dtype_unimp', msg_parm, 2);
  end;
{
********************
*
*   ENG val [sig [str]]
}
60: begin                              {ENG}
  if not gfp(r) then goto arg_missing; {get VAL argument into A1}
  if not gint(i1) then i1 := 3;        {get or default number of significant digits}
  if not gstr(tk2) then begin          {get or default separator string}
    tk2.str[1] := ' ';
    tk2.len := 1;
    end;

  string_f_fp_eng (tk, r, i1, tk3);    {make engineering representation}
  string_append (tk, tk2);             {append separator string}
  string_append (tk, tk3);             {append units name string}

  goto ret_str;                        {go return the string in TK}
  end;
{
********************
*
*   LOG arg
}
61: begin
  if not term_get (fstr, p, val) then goto arg_missing;
  a1 := val_fp (val);
  if a1 <= 0.0
    then escr_err_atline ('pic', 'log_neg', nil, 0);
  r := ln(a1);
  goto ret_r;
  end;
{
********************
*
*   E [arg]
}
62: begin
  if not term_get (fstr, p, val) then begin
    r := e;
    goto ret_r;
    end;

  a1 := val_fp (val);
  r := exp(a1);
  goto ret_r;
  end;
{
********************
*
*   CCODE chr
}
63: begin
  if not gstr (tk) then goto arg_missing;
  if tk.len <> 1 then begin
    escr_err_atline ('pic', 'ccode_strlen', nil, 0);
    end;
  i := ord(tk.str[1]);                 {return the internal character code}
  goto ret_i;
  end;
{
********************
*
*   SYM name [qual]
}
64: begin
  if not gstr (tk) then goto arg_missing; {get symbol name into TK}
  escr_sym_find (tk, sym_p);           {look up the symbol name}
  if sym_p = nil then escr_err_sym_not_found (tk);

  if gkeyw (tk)
    then begin                         {QUAL keyword is in TK}
      string_tkpick80 (tk,             {resolve the QUAL keyword}
        'TYPE DTYPE',
        pick);
      if pick = 0 then goto arg_bad_tk;
      end
    else begin                         {no QUAL keyword}
      pick := 1;                       {default to first keyword in the list}
      end
    ;

  tk.len := 0;                         {init returned string to empty}
  case pick of                         {which QUAL keyword is it ?}

1:  begin                              {TYPE, symbol type}
      case sym_p^.stype of
sym_var_k: string_vstring (tk, 'VAR'(0), -1);
sym_const_k: string_vstring (tk, 'CONST'(0), -1);
sym_subr_k: string_vstring (tk, 'SUBR'(0), -1);
sym_macro_k: string_vstring (tk, 'MACRO'(0), -1);
        end;
      end;

2:  begin                              {DTYPE, symbol data type}
      case sym_p^.stype of
sym_var_k: dtype := sym_p^.var_val.dtype;
sym_const_k: dtype := sym_p^.const_val.dtype;
otherwise                              {this symbol has no data type}
        goto ret_str;                  {return empty string}
        end;
      case dtype of                    {which data type is it ?}
dtype_bool_k: string_vstring (tk, 'BOOL'(0), -1);
dtype_int_k: string_vstring (tk, 'INTEGER'(0), -1);
dtype_fp_k: string_vstring (tk, 'REAL'(0), -1);
dtype_str_k: string_vstring (tk, 'STRING'(0), -1);
dtype_time_k: string_vstring (tk, 'TIME'(0), -1);
        end;
      end;                             {end of DTYPE keyword case}

    end;                               {end of QUAL keyword cases}
  goto ret_str;                        {return the string in TK}
  end;
{
********************
*
*   LAB name
}
65: begin
  if not gkeyw(tk) then goto arg_missing; {get local label name into TK}
  loclab_get (tk, tk2);                {get the full expanded name of this local label}
  string_append (lot, tk2);
  end;
{
********************
*
*   Unrecognized function name.
}
otherwise
    sys_msg_parm_vstr (msg_parm[1], funn);
    escr_err_atline ('pic', 'func_name_bad', msg_parm, 1);
    end;

done_func:                             {done with unique code for the particular function}
  string_token (fstr, p, tk, stat);    {try to get another function argument}
  if not string_eos(stat) then begin   {didn't hit end of string as expected ?}
    escr_err_atline_abort (stat, '', '', nil, 0);
    sys_msg_parm_vstr (msg_parm[1], funn);
    sys_msg_parm_vstr (msg_parm[2], tk);
    escr_err_atline ('pic', 'func_arg_extra', msg_parm, 2);
    end;
  return;

error:                                 {general error on processing function, STAT set}
  sys_error_print (stat, '', '', nil, 0);
  sys_msg_parm_vstr (msg_parm[1], funn);
  escr_err_atline ('pic', 'func_err', msg_parm, 1);

arg_not_num_time:                      {argument is not numeric or time value}
  escr_err_atline ('pic', 'term_not_time_num', nil, 0);

arg_not_num:                           {argument is not numeric}
  escr_err_atline ('pic', 'term_not_num', nil, 0);

arg_dtype_bad:
  sys_msg_parm_vstr (msg_parm[1], funn);
  escr_err_atline ('pic', 'func_arg_dtype', msg_parm, 1);

arg_bad_tk:                            {bad function argument, argument in TK}
  sys_msg_parm_vstr (msg_parm[1], tk);
  sys_msg_parm_vstr (msg_parm[2], funn);
  escr_err_atline ('pic', 'func_arg_bad', msg_parm, 2);

arg_error:                             {error on attempt to get function arg, STAT set}
  sys_error_print (stat, '', '', nil, 0);
  sys_msg_parm_vstr (msg_parm[1], funn);
  escr_err_atline ('pic', 'func_arg_err', msg_parm, 1);

arg_missing:                           {a required argument is missing}
  sys_msg_parm_vstr (msg_parm[1], funn);
  escr_err_atline ('pic', 'func_arg_missing', msg_parm, 1);
  end;
