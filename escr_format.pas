{   Routines for formatting values to strings in specific ways.
}
module escr_format;
define format_int;
define format_fp;
%include 'escr.ins.pas';
{
********************************************************************************
*
*   Subroutine FORMAT_INT (I, FMT, S, STAT)
*
*   Format the integer value I into the string S according for the format string
*   FMT.  The valid format string elements are:
*
*     FW n  -  Field width.  For N >= 1, the output string will be N characters
*       wide.  N = 0 indicates free format, meaning the minimum number or
*       necessary output characters will be used.
*
*     PL  -  Write leading plus if number is positive.
*
*     NPL  -  Do not write leading plus if number is positive (default).
*
*     LZ  -  Fill fixed size field with leading zeros on left.
*
*     NLZ  -  Do not add leading zeros.  Blanks will fill fixed size field.
*       (default).
*
*     BASE n  -  N is number conversion base (radix).  Must be 2 - 36.  Default
*       is 10 (decimal).
*
*     USIN  -  The input integer will be interpreted as unsigned.
*
*     SIN  -  The input integer will be interpreted as signed (default).
}
procedure format_int (                 {create specifically formatted integer string}
  in      i: sys_int_max_t;            {input integer value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned integer string}
  out     stat: sys_err_t);            {completion status}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  p: string_index_t;                   {format string parse index}
  tk: string_var32_t;                  {token parsed from format string}
  radix: sys_int_machine_t;            {number conversion radix}
  fw: string_index_t;                  {field width}
  flags: string_fi_t;                  {string conversion flags}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  ii: sys_int_machine_t;               {scratch integer}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  tk.max := size_char(tk.str);         {init local var string}

  radix := 10;                         {init to default number conversion radix}
  fw := 0;                             {init to free format}
  flags := [];                         {init to no special conversion options}
  p := 1;                              {init format string parse index}

  while true do begin                  {back here each new format command}
    string_token (fmt, p, tk, stat);   {get next command from format string}
    if string_eos(stat) then exit;     {hit end of format string ?}
    if sys_error(stat) then return;
    string_upcase (tk);                {make upper case for keyword matching}
    string_tkpick80 (tk,               {pick command keyword from list}
      'FW PL NPL LZ NLZ BASE USIN SIN',
      pick);
    case pick of                       {which format command is it ?}
{
*   FW n
}
1: begin
  string_token_int (fmt, p, ii, stat);
  if sys_error(stat) then return;
  fw := max(ii, 0);
  end;
{
*   PL
}
2: begin
  flags := flags + [string_fi_plus_k];
  end;
{
*   NPL
}
3: begin
  flags := flags - [string_fi_plus_k];
  end;
{
*   LZ
}
4: begin
  flags := flags + [string_fi_leadz_k];
  end;
{
*   NLZ
}
5: begin
  flags := flags - [string_fi_leadz_k];
  end;
{
*   BASE n
}
6: begin
  string_token_int (fmt, p, radix, stat);
  end;
{
*   USIN
}
7: begin
  flags := flags + [string_fi_unsig_k];
  end;
{
*   SIN
}
8: begin
  flags := flags - [string_fi_unsig_k];
  end;
{
*   Unexpected format command.
}
otherwise
      sys_msg_parm_vstr (msg_parm[1], tk);
      err_atline ('pic', 'err_fmt_int', msg_parm, 1);
      end;
    if sys_error(stat) then return;
    end;                               {back to get next format command}

  string_f_int_max_base (              {do the integer to string conversion}
    s,                                 {output string}
    i,                                 {input integer}
    radix,                             {number conversion radix}
    fw,                                {field width}
    flags,                             {optional conversion modifiers}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine FORMAT_FP (FP, FMT, S, STAT)
*
*   Format the floating point value FP into the string S according for the
*   format string FMT.  The valid format string elements are:
*
*     FW n  -  Field width.  For N >= 1, the output string will be N characters
*       wide.  N = 0 indicates free format, meaning the minimum number or
*       necessary output characters will be used.
*
*     FWE n  -  Field width of the exponent part of the number if exponential
*       notation is used.  0 = free form.
*
*     SIG n  -  Minimum significant digits required.  Default = 3.
*
*     MXL  -  Maximum digits allowed left of decimal point.  Default = SIG + 1.
*
*     RIT n  -  Fixed number of digits right of decimal point.
*
*     MNR n  -  Minimum required digits right of decimal point.
*
*     MXR n  -  Maximum allowed digits right of point.  May force use of
*       exponential notation.
*
*     EXP  -  Force use of exponential notation.
*
*     NEXP  -  Prohibit use of exponential notation.
*
*     ENG  -  Use engineering format when exponential notation is used.  Causes
*       the exponent to be a multiple of 3.  Default.
*
*     SCI  -  Use engineering format when exponential notation is used.
*       exponent not restricted to a particular multiple.
*
*     PNT  -  Force decimal point to always be written, even if no digits to
*       its right.
*
*     NPTN  -  Don't write the decimal point where there are no digits to its
*       right.  Default.
*
*     ZB  -  Write zero before decimal if there would otherwise not be a digit
*       there.  Default.
*
*     NZB  -  Don't write zero as only digit before decimal point.
*
*     TZ  -  Trailing zeros are allowed.  Default.
*
*     NTZ  -  Truncate trailing zeros.
*
*     PLE  -  Write plus sign before exponent if it is positive.
*
*     NPLE  -  Do not write plus sign before positive exponent.  Default.
*
*     GRP  -  Write digits in groups (commas every three digits for English).
*
*     NGRP  -  Do not write digits in groups.  Default.
*
*     PL  -  Write leading plus if number is positive.
*
*     NPL  -  Do not write leading plus if number is positive (default).
*
*     LZ  -  Fill fixed size field with leading zeros on left.
*
*     NLZ  -  Do not add leading zeros.  Blanks will fill fixed size field.
*       (default).
}
procedure format_fp (                  {create specifically formatted floating point string}
  in      fp: sys_fp_max_t;            {input integer value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned floating point string string}
  out     stat: sys_err_t);            {completion status}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  p: string_index_t;                   {format string parse index}
  tk: string_var32_t;                  {token parsed from format string}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  ii: sys_int_machine_t;               {scratch integer}
  cmds: string_var256_t;               {list of format commands}

  fw: string_index_t;                  {field width}
  fw_exp: string_index_t;              {field width of exponent}
  min_sig: sys_int_machine_t;          {min significant digits}
  max_left: sys_int_machine_t;         {max digits allowed left of point}
  min_right: sys_int_machine_t;        {min digits required right of point}
  max_right: sys_int_machine_t;        {max digits allowed right of point}
  flags_fp: string_ffp_t;              {additional flags used for FP input type}
  ntz: boolean;                        {truncate trailing zeros}
  set_mxl: boolean;                    {TRUE if MAX_LEFT set from format string}
  set_mxr: boolean;                    {TRUE if MAX_RIGHT set from format string}
  set_mnr: boolean;                    {TRUE if MIN_RIGHT set from format string}

  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
{
******************************
*
*   Local subroutine ADD_COMMAND (NAME)
*
*   Add the command NAME to the end of the space-separated list of commands in
*   CMDS.
}
procedure add_command (                {add command name to CMDS}
  in      name: string);               {upper case name of command to add}
  val_param; internal;

var
  c: char;
  ind: sys_int_machine_t;              {NAME character index}

begin
  if cmds.len > 0 then begin           {not first name in list ?}
    string_append1 (cmds, ' ');        {add space separator after previous command name}
    end;

  for ind := 1 to size_char(name) do begin
    c := name[ind];                    {get this name character}
    if (ord(c) = 0) or (c = ' ') then exit;
    string_append1 (cmds, name[ind]);  {append this name character}
    end;
  end;
{
******************************
*
*   Start of executable code for FORMAT_FP.
}
begin
  tk.max := size_char(tk.str);         {init local var string}
  cmds.max := size_char(cmds.str);

  cmds.len := 0;
  add_command ('FW');                  {1}
  add_command ('FWE');                 {2}
  add_command ('SIG');                 {3}
  add_command ('MXL');                 {4}
  add_command ('RIT');                 {5}
  add_command ('MNR');                 {6}
  add_command ('MXR');                 {7}
  add_command ('EXP');                 {8}
  add_command ('NEXP');                {9}
  add_command ('ENG');                 {10}
  add_command ('SCI');                 {11}
  add_command ('PNT');                 {12}
  add_command ('NPTN');                {13}
  add_command ('ZB');                  {14}
  add_command ('NZB');                 {15}
  add_command ('TZ');                  {16}
  add_command ('NTZ');                 {17}
  add_command ('PLE');                 {18}
  add_command ('NPLE');                {19}
  add_command ('GRP');                 {20}
  add_command ('NGRP');                {21}
  add_command ('PL');                  {22}
  add_command ('NPL');                 {23}
  add_command ('LZ');                  {24}
  add_command ('NLZ');                 {25}

  fw := string_fw_freeform_k;
  fw_exp := string_fw_freeform_k;
  min_sig := 3;
  flags_fp := [
    string_ffp_exp_eng_k,              {engineering notation if use exponential}
    string_ffp_point_k,                {always write decimal point}
    string_ffp_z_aft_k];               {write zero after point if otherwise no digit}
  ntz := false;
  set_mxl := false;
  set_mxr := false;
  set_mnr := false;

  p := 1;                              {init format string parse index}
  while true do begin                  {back here each new format command}
    string_token (fmt, p, tk, stat);   {get next command from format string}
    if string_eos(stat) then exit;     {hit end of format string ?}
    if sys_error(stat) then return;
    string_upcase (tk);                {make upper case for keyword matching}
    string_tkpick (tk, cmds, pick);    {pick command name from list of keywords}
    case pick of                       {which format command is it ?}
{
*   FW n
}
1: begin
  string_token_int (fmt, p, ii, stat);
  if sys_error(stat) then return;
  fw := max(ii, 0);
  end;
{
*   FWE n
}
2: begin
  string_token_int (fmt, p, ii, stat);
  if sys_error(stat) then return;
  fw_exp := max(ii, 0);
  end;
{
*   SIG n
}
3: begin
  string_token_int (fmt, p, min_sig, stat);
  end;
{
*   MXL n
}
4: begin
  string_token_int (fmt, p, max_left, stat);
  set_mxl := true;
  end;
{
*   RIT n
}
5: begin
  string_token_int (fmt, p, max_right, stat);
  min_right := max_right;
  set_mxr := true;
  set_mnr := true;
  end;
{
*   MNR n
}
6: begin
  string_token_int (fmt, p, min_right, stat);
  set_mnr := true;
  end;
{
*   MXR n
}
7: begin
  string_token_int (fmt, p, max_right, stat);
  set_mxr := true;
  end;
{
*   EXP
}
8: begin
  flags_fp := flags_fp + [string_ffp_exp_k];
  flags_fp := flags_fp - [string_ffp_exp_no_k];
  end;
{
*   NEXP
}
9: begin
  flags_fp := flags_fp + [string_ffp_exp_no_k];
  flags_fp := flags_fp - [string_ffp_exp_k];
  end;
{
*   ENG
}
10: begin
  flags_fp := flags_fp + [string_ffp_exp_eng_k];
  end;
{
*   SCI
}
11: begin
  flags_fp := flags_fp - [string_ffp_exp_eng_k];
  end;
{
*   PNT
}
12: begin
  flags_fp := flags_fp + [string_ffp_point_k];
  end;
{
*   NPTN
}
13: begin
  flags_fp := flags_fp - [string_ffp_point_k];
  end;
{
*   ZB
}
14: begin
  flags_fp := flags_fp - [string_ffp_nz_bef_k];
  end;
{
*   NZB
}
15: begin
  flags_fp := flags_fp + [string_ffp_nz_bef_k];
  end;
{
*   TZ
}
16: begin
  ntz := false;
  end;
{
*   NTZ
}
17: begin
  ntz := true;
  end;
{
*   PLE
}
18: begin
  flags_fp := flags_fp + [string_ffp_plus_exp_k];
  end;
{
*   NPLE
}
19: begin
  flags_fp := flags_fp - [string_ffp_plus_exp_k];
  end;
{
*   GRP
}
20: begin
  flags_fp := flags_fp + [string_ffp_group_k];
  end;
{
*   NGRP
}
21: begin
  flags_fp := flags_fp - [string_ffp_group_k];
  end;
{
*   PL
}
22: begin
  flags_fp := flags_fp + [string_ffp_plus_man_k];
  end;
{
*   NPL
}
23: begin
  flags_fp := flags_fp - [string_ffp_plus_man_k];
  end;
{
*   LZ
}
24: begin
  flags_fp := flags_fp + [string_ffp_leadz_k];
  end;
{
*   NLZ
}
25: begin
  flags_fp := flags_fp - [string_ffp_leadz_k];
  end;
{
*   Unexpected format command.
}
otherwise
      sys_msg_parm_vstr (msg_parm[1], tk);
      err_atline ('pic', 'err_fmt_fp', msg_parm, 1);
      end;
    if sys_error(stat) then return;
    end;                               {back to get next format command}

  fw := max(0, fw);                    {clip to legal values}
  if not set_mxl then begin            {set default max digits left of point ?}
    max_left := min_sig + 1;
    end;
  if                                   {fixed format with no digits right spec ?}
      (fw <> string_fw_freeform_k) and
      (not set_mnr) and (not set_mxr)
      then begin
    min_right := max(0, min((fw-1) div 2, 2));
    max_right := min_right;
    set_mxr := true;                   {prevent altering any further}
    set_mnr := true;
    end;
  if not set_mxr then begin            {set default max digits right of point ?}
    if fw = string_fw_freeform_k
      then begin                       {free format}
        max_right := min_sig + 2;
        end
      else begin                       {fixed format}
        max_right := min_right;
        end
      ;
    end;
  if not set_mnr then begin            {set default min digits right of point ?}
    if fw = string_fw_freeform_k
      then begin                       {free format}
        min_right := 0;
        end
      else begin                       {fixed format}
        min_right := max_right;
        end
      ;
    end;

  string_f_fp (                        {convert floating point to string}
    s,                                 {output string}
    fp,                                {input floating point number}
    fw,                                {field width}
    fw_exp,                            {exponent field width}
    min_sig,                           {min significant digits required}
    max_left,                          {max digits allowed left of point}
    min_right,                         {min digits required right of point}
    max_right,                         {max digits allowed right of point}
    flags_fp,                          {additional modifier flags}
    stat);
  end;
