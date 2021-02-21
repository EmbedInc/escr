{   Collection of small utility routines that don't nicely fit into any of the
*   other modules.
}
module escr_util;
define escr_show_obuf;
define escr_str_to_time;
define escr_str_from_time;
define escr_str_from_fp;
define escr_uptocomm;
define escr_set_preproc;
define escr_set_func_detect;
define escr_str_quote;
define escr_exitstatus;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_SHOW_OBUF (E)
*
*   Write the contents of the global string OBUF as the next line to standard
*   output.  OBUF is reset to empty.
}
procedure escr_show_obuf (             {write line to standard output from OBUF}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  writeln (e.obuf.str:e.obuf.len);     {write OBUF to standard output as one line}
  e.obuf.len := 0;                     {reset the output line buffer to empty}
  end;
{
********************************************************************************
*
*   Function ESCR_STR_TO_TIME (E, S, TIME)
*
*   Interpret the string S to the absolute time and return the result in the
*   time descriptor TIME.  The function returns TRUE on success and FALSE if the
*   input string is not a valid date/time string.
}
function escr_str_to_time (            {make absolute time descriptor from string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {input string}
  out     time: sys_clock_t)           {returned time descriptor}
  :boolean;                            {TRUE on success}
  val_param;

var
  pick: sys_int_machine_t;             {number of delimiter picked from list}
  tk: string_var80_t;                  {token parsed from input string}
  date: sys_date_t;                    {expanded date/time descriptor}
  p: string_index_t;                   {input string parse index}
  stat: sys_err_t;

label
  have_date;

begin
  tk.max := size_char(tk.str);         {init local var string}
  escr_str_to_time := false;           {init to input string not valid time}

  p := 1;                              {init S parse index}
  string_token_anyd (                  {extract year number field}
    s, p,                              {input string and parse index}
    '/', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special opions}
    tk,                                {parsed token}
    pick,                              {index to terminating delimiter found}
    stat);
  if sys_error(stat) then return;
  if tk.len < 1 then return;           {year can't be the empty string}
  string_t_int (tk, date.year, stat);  {convert year string to integer}
  if sys_error(stat) then return;

  date.month := 0;                     {init remaining fields to their defaults}
  date.day := 0;
  date.hour := 0;
  date.minute := 0;
  date.second := 0;
  date.sec_frac := 0.0;
  date.hours_west := 0.0;
  date.tzone_id := sys_tzone_cut_k;
  date.daysave := sys_daysave_no_k;
  date.daysave_on := false;

  string_token_anyd (                  {extract month number field}
    s, p,                              {input string and parse index}
    '/', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special opions}
    tk,                                {parsed token}
    pick,                              {index to terminating delimiter found}
    stat);
  if string_eos(stat) then goto have_date;
  if sys_error(stat) then return;
  string_t_int (tk, date.month, stat);
  if sys_error(stat) then return;
  date.month := date.month - 1;

  string_token_anyd (                  {extract day number field}
    s, p,                              {input string and parse index}
    '.', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special opions}
    tk,                                {parsed token}
    pick,                              {index to terminating delimiter found}
    stat);
  if string_eos(stat) then goto have_date;
  if sys_error(stat) then return;
  string_t_int (tk, date.day, stat);
  if sys_error(stat) then return;
  date.day := date.day - 1;

  string_token_anyd (                  {extract hour number field}
    s, p,                              {input string and parse index}
    ':', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special opions}
    tk,                                {parsed token}
    pick,                              {index to terminating delimiter found}
    stat);
  if string_eos(stat) then goto have_date;
  if sys_error(stat) then return;
  string_t_int (tk, date.hour, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract minute number field}
    s, p,                              {input string and parse index}
    ':', 1,                            {list of token delimiters}
    0,                                 {first N delimiters that may be repeated}
    [],                                {no special opions}
    tk,                                {parsed token}
    pick,                              {index to terminating delimiter found}
    stat);
  if string_eos(stat) then goto have_date;
  if sys_error(stat) then return;
  string_t_int (tk, date.minute, stat);
  if sys_error(stat) then return;

  string_substr (s, p, s.len, tk);     {get remainder of input string into TK}
  if tk.len < 1 then goto have_date;
  string_t_fpm (tk, date.sec_frac, stat); {convert to floating point seconds}
  if sys_error(stat) then return;
  date.second := trunc(date.sec_frac); {extract whole seconds}
  date.sec_frac := date.sec_frac - date.second; {remove whole seconds from fraction}

have_date:                             {DATE is all filled in}
  time := sys_clock_from_date (date);  {make absolute time descripor}
  escr_str_to_time := true;            {indicate string conversion was successful}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_STR_FROM_TIME (E, TIME, S)
*
*   Create the string representation into S of the absolute time in TIME.
}
procedure escr_str_from_time (         {make string from absolute time descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      time: sys_clock_t;           {input absolute time descriptor}
  in out  s: univ string_var_arg_t);   {returned string representation of the time}
  val_param;

var
  date: sys_date_t;                    {expanded date/time descriptor}
  tk: string_var32_t;                  {scratch token}
  stat: sys_err_t;                     {completion status}

begin
  tk.max := size_char(tk.str);         {init local var string}

  sys_clock_to_date (                  {make expanded date/time descriptor}
    time,                              {input time}
    sys_tzone_cut_k,                   {coordinate universal time timezone}
    0.0,                               {hours west of CUT}
    sys_daysave_no_k,                  {daylight savings is never applied}
    date);                             {returned expaneded date/time descriptor}

  sys_date_string (date, sys_dstr_year_k, 4, s, stat); {year}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append1 (s, '/');
  sys_date_string (date, sys_dstr_mon_k, 2, tk, stat); {month}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, '/');
  sys_date_string (date, sys_dstr_day_k, 2, tk, stat); {day}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, '.');
  sys_date_string (date, sys_dstr_hour_k, 2, tk, stat); {hour}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, ':');
  sys_date_string (date, sys_dstr_min_k, 2, tk, stat); {minute}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, ':');
  sys_date_string (date, sys_dstr_sec_frac_k, 9, tk, stat); {seconds}
  escr_err_atline_abort (e, stat, '', '', nil, 0);
  string_append (s, tk);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_STR_FROM_FP (E, TK, FP)
*
*   Convert the floating point value FP to it's official ESCR string
*   representation.  The result must always contain a decimal point or
*   exponential notation to distinguish it from a integer string.
}
procedure escr_str_from_fp (           {make string from floating point value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fp: double;                  {floating point input value}
  in out  s: univ string_var_arg_t);   {returned string representation}
  val_param;

var
  stat: sys_err_t;

begin
  string_f_fp (                        {convert FP value to string}
    s,                                 {output string}
    fp,                                {input floating point number}
    0,                                 {no fixed string width}
    0,                                 {no fixed width for exponent, if used}
    7,                                 {min required significant digits}
    6,                                 {max digits allowed left of point}
    1,                                 {min digits required right of point}
    7,                                 {max digits allowed right of point}
    [string_ffp_exp_eng_k],            {exponent multiple of 3 when used}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_UPTOCOMM (E, S, NCLEN)
*
*   Find the length of the string S with any data file end of line comment and
*   blanks preceeding it removed.  The resulting line length is returned in
*   NCLEN.
}
procedure escr_uptocomm (              {find line length without EOL comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {the input string}
  out     nclen: string_index_t);      {string length with comment removed}
  val_param;

var
  p: string_index_t;                   {input string parse index}
  syent_p: escr_syrlist_p_t;           {syntax list entry}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  stat: sys_err_t;

label
  next_comm, next_char;

begin
  nclen := 0;                          {init length with EOL comment stripped}
  p := 1;                              {init to checking first char in string}
  while p <= s.len do begin            {loop over the characters in the string}
    if s.str[p] = ' ' then goto next_char; {skip over blanks without advancing length}

    if escr_excl_check (               {syntax exclusion starts here ?}
        e,                             {ESCR library use state}
        s,                             {the string to check for exclusion in}
        p,                             {char index to check for exclusion starting}
        e.syexcl_p,                    {pointer to list of exclusions to check for}
        nil,                           {don't copy characters to any output string}
        stat)
        then begin
      escr_err_atline_abort (e, stat, '', '', nil, 0);
      nclen := p - 1;                  {valid at least up to exclusion end}
      next;                            {back to check at this new parse index}
      end;

    syent_p := e.commdeol_p;           {init to first end of line comment list entry}
    while syent_p <> nil do begin      {loop over the end of line comment syntaxes}
      if (s.len - p + 1) < syent_p^.range.st.len {not enough room for this comment start ?}
        then goto next_comm;
      for ii := 1 to syent_p^.range.st.len do begin {check the comm start characters}
        if s.str[p + ii - 1] <> syent_p^.range.st.str[ii] {char doesn't match comm start ?}
          then goto next_comm;
        end;                           {back to check next comment start sequence char}
      return;                          {found end of line comment start}
next_comm:                             {done with this comment type, on to next}
      syent_p := syent_p^.next_p;      {advance to next EOL comment type in the list}
      end;                             {back to check this new EOL comment type}

    nclen := p;                        {this is a valid non-comment non-blank char}

next_char:                             {advance to next input string character}
    p := p + 1;                        {to next character}
    end;                               {back to check this new character}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SET_PREPROC (E, ON)
*
*   Set the system to preprocessor mode when ON is TRUE, and script mode when
*   ON is FALSE.
*
*   In preprocessor mode, the input source contains arbitrary lines that are to
*   copied to a output file.  In script mode, all lines must be valid script
*   code, and there is no inherent output file.
}
procedure escr_set_preproc (           {set preprocessor mode on/off}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      on: boolean);                {enables preprocessor mode, default off}
  val_param;

begin
  if on
    then begin                         {preprocessor mode}
      e.flags := e.flags + [escr_flag_preproc_k];
      end
    else begin
      e.flags := e.flags - [escr_flag_preproc_k];
      end
    ;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SET_FUNC_DETECT (E, P)
*
*   Install the routine pointed to by P for detecting the start of functions.
*   P can be NIL to de-install any such routine.
*
*   When a routine to detect function starts is installed, then the function
*   start sequence in E.SYFUNC.ST is unused and become irrelevant.  When no
*   routine is installed, E.SYFUNC.ST will be used and must be set correctly.
}
procedure escr_set_func_detect (       {set routine for detecting function start}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      p: escr_syfunc_st_p_t);      {routine to detect function, NIL for none}
  val_param;

begin
  e.syfunc_st_p := p;                  {save the pointer}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_STR_QUOTE (STRI, STRO)
*
*   Append the contents of STRI as a quoted string to the end of STRO.  Any
*   end-quote characters in STRI are doubled so that they will be interpreted
*   as single characters.
}
procedure escr_str_quote (             {quote and append string, ESCR syntax}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stri: univ string_var_arg_t; {input string}
  in out  stro: univ string_var_arg_t); {string to append to}
  val_param;

var
  p: sys_int_machine_t;                {string index}
  qsyn_p: escr_quotesyn_p_t;           {pointer to quoted string syntax to use}
  c: char;                             {scratch character}

begin
  qsyn_p := e.quotesyn_p;              {get pointer to the quoted string syntax to use}

  if qsyn_p = nil then begin           {this system doesn't have quoted string ?}
    string_append (stro, stri);        {append the bare string}
    return;
    end;
{
*   QSYN_P points to the quoted string syntax to use.
}
  string_append1 (stro, qsyn_p^.st);   {write leading quote}
  for p := 1 to stri.len do begin      {once for each string character}
    c := stri.str[p];                  {get this string character}
    if c = qsyn_p^.en then begin       {this is a end-quote character ?}
      string_append1 (stro, qsyn_p^.en); {write end-quote character twice}
      end;
    string_append1 (stro, c);          {write string character}
    end;
  string_append1 (stro, qsyn_p^.en);   {write closing quote}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_EXITSTATUS (E, EXSTAT)
*
*   Set the ESCR variable EXITSTATUS to the value in EXSTAT.  If EXITSTATUS
*   does not exist, it is created.
}
procedure escr_exitstatus (            {set EXITSTATUS, create if needed}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      exstat: sys_int_machine_t);  {value to set EXITSTATUS to}
  val_param;

var
  sym_p: escr_sym_p_t;                 {pointer to variable to update}
  name: string_var32_t;                {symbol name}
  stat: sys_err_t;

begin
  name.max := size_char(name.str);     {init local var string}
  string_vstring (name, 'EXITSTATUS'(0), -1); {set variable name}

  escr_sym_find_curr (                 {find EXITSTATUS variable}
    e,                                 {ESCR library use state}
    name,                              {NAME of symbol to find}
    escr_sytype_var_k,                 {symbol must be a variable}
    sym_p);                            {returned pointer to the symbol}

  if                                   {need to create the variable ?}
      (sym_p = nil) or else            {variable doesn't exist at all ?}
      (sym_p^.var_val.dtype <> escr_dtype_int_k) {variable is not integer ?}
      then begin
    escr_sym_new_var (                 {create the variable}
      e,
      name,                            {variable name}
      escr_dtype_int_k,                {integer}
      true,                            {global}
      sym_p,                           {returned pointer to new variable}
      stat);
    sys_error_abort (stat, '', '', nil, 0);
    end;

  sym_p^.var_val.int := exstat;        {set the variable to the exit status code}
  e.exstat := max(e.exstat, exstat);   {update the whole script exit status}
  end;
