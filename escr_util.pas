{   Collection of small utility routines that don't nicely fit into
*   any of the other modules.
}
module prepic_util;
define write_vstr;
define write_obuf;
define show_obuf;
define str_to_time;
define str_from_time;
define str_from_fp;
define close_out;
define close_out_all;
define uptocomm;
%include 'prepic.ins.pas';
{
********************************************************************************
*
*   Subroutine WRITE_VSTR (S, STAT)
*
*   Write the string in S as the next line to the current output file.
}
procedure write_vstr (                 {write var string to current output file}
  in      s: univ string_var_arg_t;    {the string to write}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  file_write_text (s, out_p^.conn, stat); {write string as next output file line}
  end;
{
********************************************************************************
*
*   Subroutine WRITE_OBUF
*
*   Write the contents of the global string OBUF as the next line to the
*   output file.  OBUF is reset to empty.  The program is aborted on
*   any error.
}
procedure write_obuf;                  {write line to output file from OBUF}
  val_param;

var
  stat: sys_err_t;

begin
  write_vstr (obuf, stat);             {write OBUF as next output file line}
  err_atline_abort (stat, '', '', nil, 0); {abort showing input line number on error}
  obuf.len := 0;                       {reset the output line buffer to empty}
  end;
{
********************************************************************************
*
*   Subroutine SHOW_OBUF
*
*   Write the contents of the global string OBUF as the next line to standard
*   output.  OBUF is reset to empty.
}
procedure show_obuf;                   {write line to standard output from OBUF}
  val_param;

begin
  writeln (obuf.str:obuf.len);         {write OBUF to standard output as one line}
  obuf.len := 0;                       {reset the output line buffer to empty}
  end;
{
********************************************************************************
*
*   Function STR_TO_TIME (S, TIME)
*
*   Interpret the string S to the absolute time and return the result in
*   the time descriptor TIME.  The function returns TRUE on success and FALSE
*   if the input string is not a valid date/time string.
}
function str_to_time (                 {make absolute time descriptor from string}
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
  str_to_time := false;                {init to input string not valid time}

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
  str_to_time := true;                 {indicate string conversion was successful}
  end;
{
********************************************************************************
*
*   Subroutine STR_FROM_TIME (TIME, S)
*
*   Create the string representation into S of the absolute time in TIME.
}
procedure str_from_time (              {make string from absolute time descriptor}
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
  err_atline_abort (stat, '', '', nil, 0);
  string_append1 (s, '/');
  sys_date_string (date, sys_dstr_mon_k, 2, tk, stat); {month}
  err_atline_abort (stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, '/');
  sys_date_string (date, sys_dstr_day_k, 2, tk, stat); {day}
  err_atline_abort (stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, '.');
  sys_date_string (date, sys_dstr_hour_k, 2, tk, stat); {hour}
  err_atline_abort (stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, ':');
  sys_date_string (date, sys_dstr_min_k, 2, tk, stat); {minute}
  err_atline_abort (stat, '', '', nil, 0);
  string_append (s, tk);
  string_append1 (s, ':');
  sys_date_string (date, sys_dstr_sec_frac_k, 9, tk, stat); {seconds}
  err_atline_abort (stat, '', '', nil, 0);
  string_append (s, tk);
  end;
{
********************************************************************************
*
*   Subroutine STR_FROM_FP (TK, FP)
*
*   Convert the floating point value FP to it's official Prepic string
*   representation.  The result must always contain a decimal point or
*   exponential notation to distinguish it from a integer string.
}
procedure str_from_fp (                {make string from floating point value}
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
*   Subroutine CLOSE_OUT (DEL)
*
*   Close the current output file and pop back to the previous output file.
*   OUT_P will be NIL if the original output file is closed.  The closed file
*   will be deleted when DEL is TRUE.
}
procedure close_out (                  {close the current output file, pop previous}
  in      del: boolean);               {delete the file}
  val_param;

var
  o_p: outfile_p_t;                    {pointer to state of output file to close}
  stat: sys_err_t;

begin
  o_p := out_p;                        {save pointer to state of the file to delete}
  if o_p = nil then return;            {no current output file, nothing to do ?}
  out_p := o_p^.prev_p;                {pop back to previous output file}

  file_close (o_p^.conn);              {close the file}
  if del then begin                    {supposed to delete the file ?}
    file_delete_name (o_p^.conn.tnam, stat); {try to delete the file}
    end;

  sys_mem_dealloc (o_p);               {deallocate the descriptor for this output file}
  end;
{
********************************************************************************
*
*   Subroutine CLOSE_OUT_ALL (DEL)
*
*   Close all the output files.  The files are deleted when DEL if TRUE.  OUT_P
*   will be NIL when this routine returns.
}
procedure close_out_all (              {close all output files}
  in      del: boolean);               {delete the file files}
  val_param;

begin
  while out_p <> nil do begin          {loop until all output files closed}
    close_out (del);                   {delete this output file, pop back to previous}
    end;
  end;
{
********************************************************************************
*
*   Subroutine UPTOCOMM (S, NCLEN)
*
*   Finds the length of the string S with the assembler end of line comment and
*   any blanks preceeding it removed.  The resulting line length is returned in
*   NCLEN.
}
procedure uptocomm (                   {find line length without comment}
  in      s: univ string_var_arg_t;    {the input string}
  out     nclen: string_index_t);      {string length with comment removed}
  val_param;

type
  parse_k_t = (                        {line parsing state}
    parse_norm_k,                      {normal, not within special syntax}
    parse_quote_k);                    {within quoted string, EQ is ending quote}

var
  p: string_index_t;                   {input string parse index}
  parse: parse_k_t;                    {input string parsing state}
  c: char;                             {scratch character}
  eq: char;                            {ending quote character}

label
  found_comm;

begin
  parse := parse_norm_k;               {init parsing state}
  p := 1;                              {init parsing index}
  while p <= s.len do begin            {scan the input string}
    c := s.str[p];                     {get this input line character}
    case parse of                      {what is parsing state ?}
parse_norm_k: begin                    {not within any special syntax}
        case c of                      {check for special characters}
'''', '"': begin                       {start of quoted string}
            parse := parse_quote_k;    {indicate now within quoted string}
            eq := c;                   {set ending quote character}
            end;
';':      begin                        {start of end of line comment}
            goto found_comm;
            end;
          end;                         {end of special character cases}
        end;                           {end of normal parsing state case}
parse_quote_k: begin                   {within a quoted string}
        if c = eq then begin           {this is the end quote character ?}
          parse := parse_norm_k;       {back to normal parsing case}
          end;
        end;
      end;                             {end of parsing state cases}
    p := p + 1;                        {advance parse index to next character}
    end;                               {back to parse this next character}
{
*   No comment found.
}
  nclen := s.len;                      {pass back original string length}
  return;
{
*   Found comment starting at index P.
}
found_comm:
  p := p - 1;                          {make last index before comment}
  while p > 0 do begin                 {delete spaces right before comment}
    if p <= 0 then exit;
    if s.str[p] <> ' ' then exit;
    p := p - 1;
    end;
  nclen := p;                          {pass back length without the comment}
  end;
