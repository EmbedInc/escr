{   Routines that cause execution of code.
}
module escr_run;
define escr_run_atline;
define escr_run_atlabel;
define escr_run;
define escr_run_file;
define escr_run_stop;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_RUN_ATLINE (E, LINE, STAT)
*
*   Execute script code starting at the indicated input file line.
*
*   Previous execution state, if any, is completely aborted before the new
*   execution is started.  This means, among other things, that all existing
*   local symbols will be deleted before the execution starts.
}
procedure escr_run_atline (            {run starting at specific input files line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to first input files line to execute}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  str_p: string_var_p_t;               {pointer to current source line}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  cleft: sys_int_machine_t;            {number of characters left in string}
  commstart: sys_int_machine_t;        {comment start column}
  sym_p: escr_sym_p_t;                 {pointer to symbol in symbol table}
  syr_p: escr_syrlist_p_t;             {points to current syntax range list entry}
  buf: string_var8192_t;               {temp processed input line buffer}

label
  loop_line, loop_srcchar, nscomme, next_scomms, done_srcline, no_cmd;

begin
  buf.max := size_char(buf.str);       {init local var string}

  escr_run_stop (e, stat);             {delete any current execution state}
  if sys_error(stat) then return;

  if line_p = nil then return;         {nothing to do ?}
{
*   Create and initialize the execution block state.
}
  escr_exblock_new (e);                {create top level execution block}
  escr_exblock_ulab_init (e);          {top level block has unique labels context}

  escr_exblock_inline_set (e, line_p); {set next source line to execute}
{
*   Main loop.  Back here each new source code line.
}
loop_line:
  escr_infile_getline (e, str_p);      {get next input line this execution block}
  if str_p = nil then begin            {no more input this execution block ?}
    if e.exblock_p^.prev_p = nil then begin {end of top level exec block ?}
      return;
      end;
    sys_stat_set (escr_subsys_k, escr_err_inend_k, stat);
    return;
    end;
{
*   Handle blank line.
*
*   These are ignored in script mode.
*
*   In preprocessor mode, these are written to the output file if not in a
*   nested execution block.
}
  if str_p^.len = 0 then begin         {this is a blank line ?}
    if not (escr_flag_preproc_k in e.flags) {in script mode ?}
      then goto loop_line;             {ignore the blank line}
    if e.exblock_p^.prev_p = nil then begin {in top level execution block ?}
      e.ibuf.len := 0;                 {"copy" the input line to the output line}
      goto no_cmd;                     {pass to output file}
      end;
    goto loop_line;                    {ignore this line}
    end;
{
*   Copy the source line into the local buffer BUF with script comments stripped
*   off.
}
  buf.len := 0;                        {init the input line to empty}
  e.ip := 1;                           {init source line parse index}
loop_srcchar:                          {back here to check new source line character}
  if                                   {other than just normal character here ?}
      escr_excl_nextchar (             {skip over syntax exclusions at this source char}
        e,                             {state for this use of the ESCR system}
        str_p^,                        {input string}
        e.ip,                          {input string parse index}
        buf,                           {output string}
        stat)                          {completion status, always OK on function false}
      then begin
    if sys_error(stat) then return;
    if e.ip > str_p^.len then goto done_srcline; {finished scanning whole source string ?}
    end;
  cleft := str_p^.len - e.ip + 1;      {number of characters left in source string}
  {
  *   Look for comment start here.
  }
  syr_p := e.commscr_p;                {init to first syntax range in list}
  while syr_p <> nil do begin          {scan the list of comment syntaxes}
    if syr_p^.range.st.len > cleft     {comment start doesn't fit here ?}
      then goto next_scomms;
    for ii := 1 to syr_p^.range.st.len do begin {compare the comment start chars}
      if str_p^.str[e.ip + ii - 1] <> syr_p^.range.st.str[ii] {mismatch ?}
        then goto next_scomms;
      end;
    {
    *   Script comment start found here.  E.IP is the source line index of the
    *   comment start.  SYR_P is pointing to the syntax ranges list entry for
    *   this comment type.
    }
    if syr_p^.range.en.len = 0 then begin {comment ends at end of line ?}
      goto done_srcline;               {done scanning source line}
      end;
    string_append1 (buf, ' ');         {replace comment with single space}
    commstart := e.ip;                 {save comment start column number}
    e.ip := e.ip + syr_p^.range.st.len; {skip over comment start syntax}

    while e.ip <= str_p^.len-syr_p^.range.en.len+1 do begin {scan remainder of source line}
      for ii := 1 to syr_p^.range.en.len do begin {compare to comment end chars}
        if str_p^.str[e.ip + ii - 1] <> syr_p^.range.en.str[ii] {mismatch}
          then goto nscomme;
        end;
      e.ip := e.ip + syr_p^.range.en.len; {skip over comment end}
      goto loop_srcchar;               {back to process new source chars here}
nscomme:                               {comment doesn't end here}
      e.ip := e.ip + 1;                {advance to next source character}
      end;                             {back to look for comment end at new char}

    sys_stat_set (escr_subsys_k, escr_err_scommnend_k, stat); {comment not ended}
    sys_stat_parm_int (commstart, stat); {comment start column}
    return;                            {return with error}

next_scomms:                           {this comment doesn't start here, try next}
    syr_p := syr_p^.next_p;            {advance to next comment type in list}
    end;                               {back to check for this new comment type}

  string_append1 (buf, str_p^.str[e.ip]); {copy this source character}
  e.ip := e.ip + 1;                    {advance to next source character}
  if e.ip <= str_p^.len then goto loop_srcchar; {back to process this new char}

done_srcline:                          {done scanning source line}
  string_unpad (buf);                  {strip trailing spaces from input line}
  if buf.len <= 0 then begin           {result is now a blank line ?}
    goto loop_line;                    {ignore this line}
    end;
{
*   The source line has been copied to the temporary buffer BUF with script
*   comments removed, and the result is not a blank line.
*
*   Copy this into the input line buffer, E.IBUF.  If execution is not inhibited
*   here, then expand inline functions in the process.
}
  if e.inhibit_p^.inh
    then begin                         {execution is inhibited}
      string_copy (buf, e.ibuf);       {no further processing on input line}
      end
    else begin                         {execution is active}
      escr_inline_expand_line (e, buf, e.ibuf); {expand all inline functions on this line}
      end
    ;
{
*   Check for the line starts with a command.  If not, jump to NO_CMD.  If so,
*   the upper case command name will be left in E.CMD.
}
  e.ip := 1;                           {init IBUF parse index}
  string_token (e.ibuf, e.ip, e.cmd, stat); {get first input line token into CMD}
  if sys_error(stat) then goto no_cmd; {definitely no preproc command on this line ?}

  if e.cmdst.len > 0 then begin        {command start with special string ?}
    if e.cmd.len < (e.cmdst.len + 1)   {not long enough to be a command ?}
      then goto no_cmd;
    for ii := 1 to e.cmdst.len do begin {check for command start string}
      if e.ibuf.str[ii] <> e.cmdst.str[ii] {mismatch ?}
        then goto no_cmd;
      end;
    for ii := 1 to e.cmd.len - e.cmdst.len do begin {shift to delete command start string}
      e.cmd.str[ii] := e.cmd.str[ii + e.cmdst.len];
      end;
    e.cmd.len := e.cmd.len - e.cmdst.len; {update length to command start removed}
    end;

  string_upcase (e.cmd);               {make upper case for keyword matching}
{
*   Handle the specific preprocessor command in CMD.
}
  escr_sym_find (                      {look up command name}
    e,                                 {state for this use of the ESCR system}
    e.cmd,                             {name to look up}
    e.sym_cmd,                         {symbol table to look up name in}
    sym_p);                            {returned pointer to symbol data}
  if sym_p = nil then begin            {no such command ?}
    sys_stat_set (escr_subsys_k, escr_err_cmdbad_k, stat);
    sys_stat_parm_vstr (e.cmd, stat);
    return;
    end;

  case sym_p^.stype of                 {what kind of symbol is this ?}
escr_sym_icmd_k: begin                 {intrinsic command, implemented by compiled routine}
      sys_error_none (stat);           {init to no error encountered}
      sym_p^.icmd_p^ (addr(e), stat);  {run the command routine}
      if sys_error(stat) then return;
      end;
escr_sym_cmd_k: begin                  {command defined with script code}
      writeln ('Script-defined commands not implemented yet.');
      escr_err_atline (e, '', '', nil, 0);
      end;
otherwise
    sys_stat_set (escr_subsys_k, escr_err_notcmd_k, stat); {symbol is not a command}
    sys_stat_parm_vstr (e.cmd, stat);
    return;
    end;

  if not e.inhibit_p^.inh then begin   {don't check unused tokens if not executed}
    escr_get_end (e);                  {error if input line not exhausted}
    end;
  if e.obuf.len > 0 then escr_write_obuf (e, stat); {write any line fragment left in out buffer}
  if sys_error(stat) then return;
  goto loop_line;
{
*   This input line does not contain a command.
}
no_cmd:
  if not (escr_flag_preproc_k in e.flags) then begin {in script mode ?}
    sys_stat_set (escr_subsys_k, escr_err_badinline_k, stat); {bad input line}
    return;
    end;
  {
  *   In preprocessor mode.
  }
  if e.inhibit_p^.inh then goto loop_line; {execution inhibited for this line ?}
  if escr_macro_run (e, stat) then goto loop_line; {macro invocation on this line processed ?}
  if sys_error(stat) then return;
  escr_write_vstr (e, e.ibuf, stat);
  if sys_error(stat) then return;
  goto loop_line;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_ATLABEL (E, NAME, STAT)
*
*   Execute script code starting at the labeled input files line.  NAME must be
*   the name of a previously existing label.
}
procedure escr_run_atlabel (           {run starting at label}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of label to start running at}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  sym_p: escr_sym_p_t;                 {label symbol}

begin
  escr_sym_find (                      {look up name in symbol table}
    e,                                 {state for this use of ESCR system}
    name,                              {name to look up}
    e.sym_lab,                         {symbol table to look up in}
    sym_p);                            {returned pointer to the symbol}
  if sym_p = nil then begin            {named label doesn't exist ?}
    sys_stat_set (escr_subsys_k, escr_err_nflab_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;
  if sym_p^.stype <> escr_sym_label_k then begin
    sys_stat_set (escr_subsys_k, escr_err_notlab_k, stat);
    sys_stat_parm_vstr (name, stat);
    return;
    end;

  escr_run_atline (                    {run the user code}
    e,                                 {state for this use of the ESCR system}
    sym_p^.label_line_p,               {pointer to line to start running at}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN (E, STAT)
*
*   Execute script code starting at the first line of the first input file.
}
procedure escr_run (                   {run starting at first line of first input file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  if e.files_p = nil                   {nothing to run ?}
    then return;

  escr_run_atline (                    {run the user code}
    e,                                 {state for this use of the ESCR system}
    e.files_p^.lines_p,                {poiner to line to start running at}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_FILE (E, FNAM, SUFF, STAT)
*
*   Execute script code starting at the first line of the indicated file.  The
*   file will be read and saved in memory before any code is executed.  If the
*   file was previously read into memory, then it will not be read again.
}
procedure escr_run_file (              {run starting at first line of file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to run script code from}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  infile_p: escr_infile_p_t;           {pointer to the input file data in memory}

begin
  escr_infile_open (                   {find file data or read it into memory}
    e,                                 {state for this use of the ESCR system}
    fnam, suff,                        {file name and allowed suffixes}
    infile_p,                          {returned pointer to file data}
    stat);
  if sys_error(stat) then return;

  if infile_p = nil                    {nothing to run (shouldn' happen) ?}
    then return;

  escr_run_atline (                    {run the user code}
    e,                                 {state for this use of the ESCR system}
    infile_p^.lines_p,                 {pointer to line to start running at}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_STOP (E, STAT)
*
*   Stop any execution in progress and delete the current execution state.
}
procedure escr_run_stop (              {unconditionally stop execution}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin






  end;
