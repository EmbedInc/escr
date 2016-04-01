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
  nclen: string_index_t;               {string length with comment removed}
  sym_p: escr_sym_p_t;                 {pointer to symbol in symbol table}

label
  loop_line, no_cmd;

begin
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

  string_unpad (str_p^);               {delete trailing space from the input line}
{
*   Handle blank line.  These are passed to the output file if not in a nested
*   execution block.  Otherwise, blank lines are completely ignored, and are not
*   copied to the output file.
}
  if str_p^.len = 0 then begin         {this is a blank line ?}
    if e.exblock_p^.prev_p = nil then begin {in top level execution block ?}
      e.ibuf.len := 0;                 {"copy" the input line to the output line}
      goto no_cmd;                     {pass to output file}
      end;
    goto loop_line;                    {ignore this line}
    end;
{
*   Check for "//" comment.  These lines are ignored at the preprocessor level
*   and not written to the output file.  This is different from regular
*   assembler comments, which are copied to the output file.
}
  e.ip := 1;                           {init input line parse index}
  while e.ip < str_p^.len do begin     {scan forwards to first non-blank}
    if str_p^.str[e.ip] <> ' ' then exit; {found first non-blank ?}
    e.ip := e.ip + 1;
    end;
  if                                   {this is a preprocessor comment line ?}
      (e.ip < str_p^.len) and then     {at least two chars starting at first non-blank ?}
      (str_p^.str[e.ip] = '/') and (str_p^.str[e.ip + 1] = '/') {starts with "//" ?}
    then goto loop_line;               {totally ignore this line}

  if e.inhibit_p^.inh
    then begin                         {execution is inhibited}
      string_copy (str_p^, e.ibuf);    {copy line without expanding inline functions}
      end
    else begin                         {normal execution}
      escr_inline_expand_line (e, str_p^, e.ibuf); {expand all inline functions on this line}
      end
    ;
  e.ip := 1;                           {init IBUF parse index}
  string_token (e.ibuf, e.ip, e.cmd, stat); {get first input line token into CMD}
  if sys_error(stat) then goto no_cmd; {definitely no preproc command on this line ?}
  if e.cmd.len < 2 then goto no_cmd;   {not long enough to be preproc command ?}
  if e.cmd.str[1] <> '/' then goto no_cmd; {token doesn't have preproc cmd prefix ?}

  for ii := 1 to e.cmd.len - 1 do begin {shift command name to delete leading "/"}
    e.cmd.str[ii] := e.cmd.str[ii + 1];
    end;
  e.cmd.len := e.cmd.len - 1;          {update command length to "/" removed}
  string_upcase (e.cmd);               {make upper case for keyword matching}
{
*   The line in IBUF contains a command.  The upper case command name is in CMD.
}
  escr_uptocomm (e, e.ibuf, nclen);    {get line length with comment stripped}
  e.ibuf.len := nclen;
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
  if e.obuf.len > 0 then escr_write_obuf (e); {write any line fragment left in out buffer}
  goto loop_line;
{
*   This input line does not contain a preprocessor command.
}
no_cmd:
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
*   Subroutine ESCR_RUN_FILE (E, FNAM, STAT)
*
*   Excute script code starting at the first line of the indicated file.  The
*   file will be read and saved in memory before any code is executed.  If the
*   file was previously read into memory, then it will not be read again.
}
procedure escr_run_file (              {run starting at first line of first input file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to run script code from}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  infile_p: escr_infile_p_t;           {pointer to the input file data in memory}

begin
  escr_infile_open (                   {find file data or read it into memory}
    e,                                 {state for this use of the ESCR system}
    fnam,                              {file name}
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
