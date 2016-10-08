{   Routines for handling terms in expressions or function parameters.
}
module escr_term;
define escr_val_copy;
define escr_val_bool;
define escr_val_int;
define escr_val_fp;
define escr_val_isint;
define escr_val_isbool;
define escr_val_str;
define escr_val_text;
define escr_val_size;
define escr_val_init;
define escr_val_time;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_VAL_COPY (E, IVAL, OVAL, STAT)
*
*   Copy the value in IVAL to OVAL.  OVAL must be set up and of a data type that
*   can be unambiguously converted from IVAL.  Only the actual data value of
*   OVAL is irrelevant on entry.
}
procedure escr_val_copy (              {copy a VAL_T value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      ival: escr_val_t;            {the input value}
  out     oval: escr_val_t;            {the output value}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  case oval.dtype of                   {what data type converting to ?}
escr_dtype_bool_k: begin               {to BOOLEAN}
    oval.bool := escr_val_bool (e, ival, stat);
    end;
escr_dtype_int_k: begin                {to INTEGER}
    oval.int := escr_val_int (e, ival, stat);
    end;
escr_dtype_fp_k: begin                 {to FLOATING POINT}
    oval.fp := escr_val_fp (e, ival, stat);
    end;
escr_dtype_str_k: begin                {to STRING}
    escr_val_str (e, ival, oval.str);
    sys_error_none (stat);
    end;
escr_dtype_time_k: begin               {to TIME}
    oval.time := escr_val_time (e, ival, stat);
    end;
otherwise                              {unimplemented output data type}
    escr_err_dtype_unimp (e, oval.dtype, 'VAL_COPY');
    end;                               {end of output data type cases}
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_BOOL (E, VAL, STAT)
*
*   Return the boolean value of VAL.  It is an error if VAL can't be
*   unambiguously converted to a boolean.  The function value is always FALSE
*   when returning with error status.
}
function escr_val_bool (               {return boolean value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {the boolean value of VAL}
  val_param;

var
  pick: sys_int_machine_t;             {number of keyword picked from list}
  tk: string_var80_t;                  {scratch token}

begin
  tk.max := size_char(tk.str);         {init local var string}
  escr_val_bool := false;              {stop compiler from complaining}
  sys_error_none (stat);               {init to no error encountered}

  case val.dtype of                    {what is the input value data type ?}

escr_dtype_bool_k: begin               {input data type is boolean}
      escr_val_bool := val.bool;
      return;
      end;

escr_dtype_str_k: begin                {input data type is string}
      string_copy (val.str, tk);       {make local copy of string}
      string_upcase (tk);              {make upper case for keyword matching}
      string_tkpick80 (tk, 'TRUE FALSE', pick);
      case pick of                     {which keyword is it ?}
1:      begin                          {TRUE}
          escr_val_bool := true;
          return;
          end;
2:      begin                          {FALSE}
          escr_val_bool := false;
          return;
          end;
        end;                           {end of which keyword picked cases}
      end;
    end;                               {end of VAL data type cases}

  sys_stat_set (escr_subsys_k, escr_err_notbool_k, stat);
  escr_val_str (e, val, tk);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_INT (E, VAL, STAT)
*
*   Return the integer value of VAL.  It is an error if VAL can't be
*   unambiguously converted to an integer.  The function value is always 0 when
*   returning with error status.
}
function escr_val_int (                {return integer value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_int_max_t;                      {integer value of VAL}
  val_param;

var
  ii: sys_int_max_t;
  tk: string_var80_t;

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error encountered}
  escr_val_int := 0;                   {stop compiler from complaining}

  case val.dtype of                    {what is the input data type ?}
escr_dtype_int_k: begin                {integer}
      escr_val_int := val.int;
      return;
      end;
escr_dtype_str_k: begin                {string}
      string_t_int_max (val.str, ii, stat); {try converting to integer}
      return;
      end;
    end;                               {end of VAL data type cases}

  sys_stat_set (escr_subsys_k, escr_err_notint_k, stat);
  escr_val_str (e, val, tk);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_TIME (E, VAL, STAT)
*
*   Return the time value of VAL.  It is an error if VAL can't be unambiguously
*   converted to a time value.
}
function escr_val_time (               {convert to time value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_clock_t;                        {time value of VAL}
  val_param;

var
  time: sys_clock_t;
  tk: string_var80_t;

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error encountered}

  escr_val_time := val.time;           {init for easy case}
  case val.dtype of                    {what is the input data type ?}
escr_dtype_time_k: begin               {time}
      return;
      end;
escr_dtype_str_k: begin                {string}
      if escr_str_to_time (e, val.str, time) then begin {successfully converted ?}
        escr_val_time := time;         {pass back the result}
        return;
        end;
      end;
    end;                               {end of VAL data type cases}

  sys_stat_set (escr_subsys_k, escr_err_nottime_k, stat);
  escr_val_str (e, val, tk);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_FP (E, VAL, STAT)
*
*   Return the floating point value of VAL.  It is an error if VAL can't be
*   unambiguously converted to floating point.  The function value is always
*   0.0 when returning with error.
}
function escr_val_fp (                 {return FP value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_fp_max_t;                       {floating point value of VAL}
  val_param;

var
  fp: sys_fp_max_t;
  tk: string_var80_t;

begin
  tk.max := size_char(tk.str);         {init local var string}
  sys_error_none (stat);               {init to no error encountered}
  escr_val_fp := 0.0;                  {init return value}

  case val.dtype of                    {what is the input data type ?}
escr_dtype_int_k: begin                {integer}
      escr_val_fp := val.int;
      return;
      end;
escr_dtype_fp_k: begin                 {floating point}
      escr_val_fp := val.fp;
      return;
      end;
escr_dtype_str_k: begin                {string}
      string_t_fpmax (val.str, fp, [], stat); {try converting to floating point}
      if not sys_error(stat) then begin {conversion was successful ?}
        escr_val_fp := fp;
        return;
        end;
      end;
    end;                               {end of VAL data type cases}

  sys_stat_set (escr_subsys_k, escr_err_notfp_k, stat);
  escr_val_str (e, val, tk);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_ISINT (E, VAL, VI, VF)
*
*   Decide whether a numeric value is integer or floating point.  If integer,
*   then VI is set to the integer value and the function returns TRUE.  If
*   floating point then the function returns FALSE.  VF is set to the value in
*   either case of integer or floating point.  If VAL is neither integer or
*   floating point, then STAT is returned indicating error.  The function value
*   is always FALSE when returning with error.
}
function escr_val_isint (              {determine INT or FP, error if neither}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  out     vi: sys_int_max_t;           {returned integer value, if integer}
  out     vf: sys_fp_max_t;            {returned floating point value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if integer, FALSE on FP or error}
  val_param;

var
  ii: sys_int_max_t;
  fp: sys_fp_max_t;
  tk: string_var80_t;

begin
  tk.max := size_char(tk.str);         {init local var string}
  escr_val_isint := false;             {init to FP or error}
  sys_error_none (stat);               {init to no error encountered}

  case val.dtype of                    {what is the input data type ?}
escr_dtype_int_k: begin                {integer}
      vi := val.int;
      vf := val.int;
      escr_val_isint := true;
      return;
      end;
escr_dtype_fp_k: begin                 {floating point}
      vf := val.fp;
      return;
      end;
escr_dtype_str_k: begin                {string}
      string_t_int_max (val.str, ii, stat); {try converting to integer}
      if not sys_error(stat) then begin {conversion was successful ?}
        vi := ii;
        vf := ii;
        escr_val_isint := true;
        return;
        end;
      string_t_fpmax (val.str, fp, [], stat); {try converting to floating point}
      if not sys_error(stat) then begin {conversion was successful ?}
        vf := fp;
        return;
        end;
      end;
    end;                               {end of VAL data type cases}

  sys_stat_set (escr_subsys_k, escr_err_notintfp_k, stat);
  escr_val_str (e, val, tk);
  sys_stat_parm_vstr (tk, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_ISBOOL (E, VAL, B)
*
*   Checks whether the value in VAL has a boolean representation.  If so, the
*   function returns TRUE and the boolean value is returned in B.  If not, the
*   function returns FALSE and B is not altered.
}
function escr_val_isbool (             {check for VAL can be converted to boolean}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {input value to check}
  out     b: boolean)                  {boolean value, unaltered if VAL not boolean}
  :boolean;                            {TRUE if returning boolean value in B}
  val_param;

var
  pick: sys_int_machine_t;
  tk: string_var32_t;

begin
  tk.max := size_char(tk.str);         {init local var string}
  escr_val_isbool := true;             {init to value is boolean}

  case val.dtype of                    {what is the input data type ?}

escr_dtype_bool_k: begin               {boolean}
      b := val.bool;
      return;
      end;

escr_dtype_str_k: begin                {string}
      string_copy (val.str, tk);       {make local copy of string}
      string_upcase (tk);              {make upper case for keyword matching}
      string_tkpick80 (tk, 'TRUE FALSE', pick);
      case pick of                     {which keyword is it ?}
1:      begin                          {TRUE}
          b := true;
          return;
          end;
2:      begin                          {FALSE}
          b := false;
          return;
          end;
        end;                           {end of which keyword picked cases}
      end;                             {end of string data type case}

    end;                               {end of data type cases}
  escr_val_isbool := false;            {indicate VAL is not boolean}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_VAL_STR (E, VAL, STR)
*
*   Convert the value in VAL to a string and return it in STR.  There can be no
*   error condition because all data types have a string representation.
}
procedure escr_val_str (               {make string representation of value in VAL}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param;

begin
  case val.dtype of                    {what is the input value data type ?}

escr_dtype_bool_k: begin               {BOOLEAN}
      if val.bool
        then string_vstring (str, 'TRUE'(0), -1)
        else string_vstring (str, 'FALSE'(0), -1);
      end;

escr_dtype_int_k: begin                {INTEGER}
      string_f_int (str, val.int);
      end;

escr_dtype_fp_k: begin                 {REAL}
      escr_str_from_fp (e, val.fp, str);
      end;

escr_dtype_str_k: begin                {STRING}
      string_copy (val.str, str);
      end;

escr_dtype_time_k: begin               {TIME}
      escr_str_from_time (e, val.time, str);
      end;

otherwise
    writeln ('Internal error in program ESCR: Data type ID of', ord(val.dtype));
    writeln ('encountered in subroutine VAL_STR.');
    escr_err_atline (e, '', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_VAL_TEXT (E, VAL, STR)
*
*   Convert the value in VAL to its text representation in the current input
*   language.
}
procedure escr_val_text (              {make output language text representation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param;

var
  i: sys_int_machine_t;                {scratch loop counter}
  tk: string_var32_t;                  {scratch token}
  c: char;                             {scratch character}
  s_p: string_var_p_t;                 {scratch pointer to a string}

label
  ret_string;

begin
  tk.max := size_char(tk.str);         {init local var string}

  case val.dtype of                    {what is the input value data type ?}

escr_dtype_bool_k: begin               {BOOLEAN}
      if val.bool
        then string_vstring (str, '1'(0), -1) {true}
        else string_vstring (str, '0'(0), -1); {false}
      end;

escr_dtype_int_k: begin                {INTEGER}
      string_f_int (str, val.int);
      end;

escr_dtype_fp_k: begin                 {REAL}
      string_f_fp_free (str, val.fp, 6);
      end;                             {end of FP data type case}

escr_dtype_str_k: begin                {STRING}
      s_p := univ_ptr(addr(val.str));  {point to source string}
ret_string:                            {common code to return string S_P^}
      str.len := 0;                    {init output string to empty}
      string_append1 (str, '"');       {leading quote}
      for i := 1 to s_p^.len do begin  {once for each input character}
        c := s_p^.str[i];              {get this input character}
        string_append1 (str, c);       {copy input character to the output}
        if c = '"' then begin          {character is a quote ?}
          string_append1 (str, '"');   {write double quote for a single quote}
          end;
        end;                           {back for next input string character}
      string_append1 (str, '"');       {trailing quote}
      end;

escr_dtype_time_k: begin               {TIME}
      escr_str_from_time (e, val.time, tk); {make time string in TK}
      s_p := univ_ptr(addr(tk));       {point to string to return}
      goto ret_string;                 {return as text string}
      end;

otherwise
    writeln ('Internal error in program ESCR: Data type ID of', ord(val.dtype));
    writeln ('encountered in subroutine VAL_TEXT.');
    escr_err_atline (e, '', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   Function ESCR_VAL_SIZE (E, DTYPE, LEN)
*
*   Return the memory size needed for a VAL_T structure of data type DTYPE.
*   LEN is an additional parameter used for some data types.  The data types
*   where LEN is used are:
*
*     STRING - Maximum number of characters the string can hold.
*
*   LEN is ignored for all other data types.
}
function escr_val_size (               {return minimum required size of VAL_T structure}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {data type of the value}
  in      len: sys_int_machine_t)      {max string chars to hold, ignored other dtypes}
  :sys_int_adr_t;
  val_param;

begin
  case dtype of                        {what data type is it ?}

escr_dtype_bool_k: begin
      escr_val_size := offset(escr_val_t.bool) + size_min(escr_val_t.bool);
      end;

escr_dtype_int_k: begin
      escr_val_size := offset(escr_val_t.int) + size_min(escr_val_t.int);
      end;

escr_dtype_fp_k: begin
      escr_val_size := offset(escr_val_t.fp) + size_min(escr_val_t.fp);
      end;

escr_dtype_str_k: begin
      escr_val_size := offset(escr_val_t.str) + string_size(len);
      end;

escr_dtype_time_k: begin
      escr_val_size := offset(escr_val_t.time) + size_min(escr_val_t.time);
      end;

otherwise
    writeln ('Internal error in program ESCR: Data type ID of', ord(dtype));
    writeln ('encountered in subroutine VAL_SIZE.');
    escr_err_atline (e, '', '', nil, 0);
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_VAL_INIT (E, DTYPE, VAL)
*
*   Initialize the full VAL_T descriptor VAL to the data type DTYPE.  VAL must
*   be the full VAL_T structure as defined in the include file.  It must not
*   be a short version as used in some cases, such as for symbol values.
}
procedure escr_val_init (              {initialize full VAL_T descriptor to data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {data type to set up VAL for}
  out     val: escr_val_t);            {full value descriptor to initialize}
  val_param;

begin
  val.dtype := dtype;                  {set the data type}
  case dtype of                        {what data type being initialized to ?}
escr_dtype_str_k: begin                {STRING}
      val.str.max := size_char(val.str.str);
      val.str.len := 0;
      end;
    end;
  end;
