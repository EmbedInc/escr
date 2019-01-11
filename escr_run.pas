{   Routines that cause execution of code.
}
module escr_run;
define escr_run;
define escr_run_atline;
define escr_run_conn;
define escr_run_file;
define escr_run_clean;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_RUN (E, STAT)
*
*   Execute script code from the current state.  The routine returns when the
*   starting execution block is deleted or the input stream ends with execution
*   at the same level as on entry.  When not returning with error, the current
*   execution block at entry will be deleted, and the current execution state
*   will be that of the original parent, if any.
*
*   This routine may be called recursively.  One example is when a user-defined
*   function is called.  When that happens, the current parsing state is saved,
*   a new execution block is created for the function, then ESCR_RUN called
*   recursively to execute the function.
}
procedure escr_run (                   {run, state all set up, returns on script exit}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  parent_p: escr_exblock_p_t;          {points to parent execution block}
  str_p: string_var_p_t;               {pointer to current source line}
  p: string_index_t;                   {raw source line parse index}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  sym_p: escr_sym_p_t;                 {pointer to symbol in symbol table}
  buf: string_var8192_t;               {temp processed input line buffer}
  inh: boolean;                        {saved inhibit at start of command}

label
  loop_line, no_cmd, leave_end, leave;

begin
  buf.max := size_char(buf.str);       {init local var string}
  sys_error_none (stat);               {init to no error encountered}

  if e.exblock_p = nil then begin      {not in any execution block ?}
    sys_stat_set (escr_subsys_k, escr_err_noblock_k, stat);
    return;
    end;
{
*   Set up the state so that we exit when the current execution block is
*   deleted.
}
  parent_p := e.exblock_p^.prev_p;     {save pointer to parent execution block}
  e.exblock_p^.prev_p := nil;          {prevent popping past this block without action here}
{
*   Main loop.  Back here each new source code line.
}
loop_line:
  if e.exblock_p = nil then begin      {original execution block deleted ?}
    goto leave;
    end;

  escr_infile_getline (e, str_p);      {get next input line this execution block}
  if str_p = nil then goto leave_end;  {no more input this execution block ?}
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
    if e.exblock_p^.level = 0 then begin {in top level execution block ?}
      e.parse_p^.ibuf.len := 0;        {"copy" the input line to the output line}
      goto no_cmd;                     {pass to output file}
      end;
    goto loop_line;                    {ignore this line}
    end;
{
*   Copy the source line into the local buffer BUF with script comments stripped
*   off.
}
  buf.len := 0;                        {init the input line to empty}
  p := 1;                              {init source line parse index}
  while p <= str_p^.len do begin       {loop until source line exhausted}
    if                                 {syntax exclusion here ?}
        escr_excl_check (              {skip over syntax exclusion, if present}
          e,                           {state for this use of the ESCR system}
          str_p^,                      {input string}
          p,                           {input string parse index}
          e.syexcl_p,                  {points to list of syntax exclusions}
          addr(buf),                   {pointer to string to append exclusion to}
          stat)                        {completion status, always OK on function false}
        then begin
      if sys_error(stat) then return;
      next;                            {back to check after this exclusion}
      end;
    if                                 {data file comment here ?}
        escr_excl_check (              {copy data file comments}
          e,                           {state for this use of the ESCR system}
          str_p^,                      {input string}
          p,                           {input string parse index}
          e.commdat_p,                 {points to list of syntax exclusions}
          addr(buf),                   {pointer to string to append exclusion to}
          stat)                        {completion status, always OK on function false}
        then begin
      if sys_error(stat) then return;
      next;                            {back to check after this exclusion}
      end;
    if                                 {script comment here ?}
        escr_excl_check (              {skip over any script comment here}
          e,                           {state for this use of the ESCR system}
          str_p^,                      {input string}
          p,                           {input string parse index}
          e.commscr_p,                 {points to list of syntax exclusions}
          nil,                         {discard the comment characters}
          stat)                        {completion status, always OK on function false}
        then begin
      if sys_error(stat) then return;
      next;                            {back to check after this comment}
      end;

    string_append1 (buf, str_p^.str[p]); {copy this source character to output string}
    p := p + 1;                        {advance to next source character}
    end;                               {back to process next source string character}

  string_unpad (buf);                  {strip trailing spaces from input line}

  if buf.len <= 0 then begin           {result is now a blank line ?}
    goto loop_line;                    {ignore this line}
    end;
{
*   The source line has been copied to the temporary buffer BUF with script
*   comments removed, and the result is not a blank line.
*
*   Copy this into the input line buffer, PARSE.IBUF.  If execution is not
*   inhibited here, then expand inline functions in the process.
}
  if e.inhibit_p^.inh
    then begin                         {execution is inhibited}
      string_copy (buf, e.parse_p^.ibuf); {no further processing on input line}
      end
    else begin                         {execution is active}
      escr_inline_expand_line (        {expand all inline functions on this line}
        e, buf, e.parse_p^.ibuf, stat);
      if sys_error(stat) then return;
      end
    ;
  e.parse_p^.ip := 1;                  {init IBUF parse index}
{
*   Check for the line starts with a command.  If so, the upper case command
*   name will be left in PARSE.CMD.  If not, jump to NO_CMD.
}
  if escr_flag_preproc_k in e.flags
    then begin
      {
      *   In preprocessor mode.  To identify a possible command, data file
      *   comments must first be stripped from the input line.  A copy of the
      *   input line with data file comments stripped is made in BUF, from which
      *   the candidate command is extracted.  If a command is found, then BUF
      *   is copied to IBUF, and command processing proceeds without data file
      *   comments.  If no command is found, then IBUF is not altered and
      *   execution goes to NO_CMD.
      }
      buf.len := 0;                    {init result line to empty}
      while e.parse_p^.ip <= e.parse_p^.ibuf.len do begin {loop until line exhausted}
        if                             {syntax exclusion here ?}
            escr_excl_check (          {skip over syntax exclusion, if present}
              e,                       {state for this use of the ESCR system}
              e.parse_p^.ibuf,         {input string}
              e.parse_p^.ip,           {input string parse index}
              e.syexcl_p,              {points to list of syntax exclusions}
              addr(buf),               {pointer to string to append exclusion to}
              stat)                    {completion status, always OK on function false}
            then begin
          if sys_error(stat) then return;
          next;                        {back to check after this exclusion}
          end;
        if                             {data file comment here ?}
            escr_excl_check (          {skip over any script comment here}
              e,                       {state for this use of the ESCR system}
              e.parse_p^.ibuf,         {input string}
              e.parse_p^.ip,           {input string parse index}
              e.commdat_p,             {points to list of comments start/end}
              nil,                     {discard the comment characters}
              stat)                    {completion status, always OK on function false}
            then begin
          if sys_error(stat) then return;
          next;                        {back to check after this comment}
          end;
        string_append1 (               {copy this source character to output string}
          buf, e.parse_p^.ibuf.str[e.parse_p^.ip]);
        e.parse_p^.ip := e.parse_p^.ip + 1; {advance to next source character}
        end;                           {back to process next source string character}

      e.parse_p^.ip := 1;              {init parse index to start of string}
      string_token (buf, e.parse_p^.ip, e.parse_p^.cmd, stat); {get first input line token into CMD}
      if sys_error(stat) then goto no_cmd; {definitely no preproc command on this line ?}

      if e.cmdst.len > 0 then begin    {commands start with special string ?}
        if e.parse_p^.cmd.len < (e.cmdst.len + 1) {not long enough to be a command ?}
          then goto no_cmd;
        for ii := 1 to e.cmdst.len do begin {check for command start string}
          if e.parse_p^.cmd.str[ii] <> e.cmdst.str[ii] {mismatch ?}
            then goto no_cmd;
          end;
        for                            {shift to delete command start string}
            ii := 1 to e.parse_p^.cmd.len - e.cmdst.len
            do begin
          e.parse_p^.cmd.str[ii] := e.parse_p^.cmd.str[ii + e.cmdst.len];
          end;
        e.parse_p^.cmd.len :=          {update length to command start removed}
          e.parse_p^.cmd.len - e.cmdst.len;
        end;

      string_copy (buf, e.parse_p^.ibuf); {process line with data file comments stripped}
      end
    else begin
      {
      *   In script mode.  Data file comments don't exist in script mode.
      }
      string_token (                   {get first input line token into CMD}
        e.parse_p^.ibuf, e.parse_p^.ip, e.parse_p^.cmd, stat);
      if sys_error(stat) then goto no_cmd; {definitely no command on this line ?}

      if e.cmdst.len > 0 then begin    {commands start with special string ?}
        if e.parse_p^.cmd.len < (e.cmdst.len + 1) {not long enough to be a command ?}
          then goto no_cmd;
        for ii := 1 to e.cmdst.len do begin {check for command start string}
          if e.parse_p^.ibuf.str[ii] <> e.cmdst.str[ii] {mismatch ?}
            then goto no_cmd;
          end;
        for                            {shift to delete command start string}
            ii := 1 to e.parse_p^.cmd.len - e.cmdst.len
            do begin
          e.parse_p^.cmd.str[ii] := e.parse_p^.cmd.str[ii + e.cmdst.len];
          end;
        e.parse_p^.cmd.len :=          {update length to command start removed}
          e.parse_p^.cmd.len - e.cmdst.len;
        end;
      end
    ;
{
*   Handle the command in CMD.
}
  escr_sym_find_type (                 {look up command name}
    e,                                 {state for this use of the ESCR system}
    e.parse_p^.cmd,                    {name to look up}
    escr_sytype_cmd_k,                 {must be command}
    sym_p,                             {returned pointer to symbol data}
    stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {no such command ?}
    sys_stat_set (escr_subsys_k, escr_err_cmdbad_k, stat);
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    return;
    end;

  case sym_p^.stype of                 {what kind of symbol is this ?}

escr_sym_icmd_k: begin                 {intrinsic command, implemented by compiled routine}
      inh := e.inhibit_p^.inh;         {temp save inhibit state at start of command}

      sys_error_none (stat);           {init to no error encountered}
      sym_p^.icmd_p^ (addr(e), stat);  {run the command routine}
      if sys_error(stat) then return;

      if not inh then begin            {check for extra parms if command was run}
        if not escr_get_end (e, stat) then return; {abort on extra parameter}
        end;
      if e.obuf.len > 0 then begin     {write any line fragment left in out buffer}
        escr_write_obuf (e, stat);
        end;
      if sys_error(stat) then return;
      end;

escr_sym_cmd_k: begin                  {user-defined command, implemented with script code}
      if not e.inhibit_p^.inh then begin {only run command if execution not inhibited}
        escr_exblock_new (e, stat);    {create new  execution block}
        if sys_error(stat) then return;
        escr_exblock_refsym (e, sym_p^); {indicate referencing symbol for this command}
        e.exblock_p^.bltype := escr_exblock_cmd_k; {new block is a command}
        e.exblock_p^.args := true;     {this block can take arguments}
        escr_exblock_arg_addn (e, e.parse_p^.cmd, 0); {command name is special argument 0}
        while true do begin            {loop until argument list exhausted}
          if not escr_get_tkraw (e, buf) then exit; {try to get command parameter}
          escr_exblock_arg_add (e, buf); {add as next argument to new execution block}
          end;
        escr_exblock_inline_set (      {go to command definition line}
          e, sym_p^.cmd_line_p, stat);
        if sys_error(stat) then return;
        escr_infile_skipline (e);      {skip over command definition line}
        escr_exblock_ulab_init (e);    {create table for local labels}
        end;                           {end of execution not inhibited case}
      end;                             {end of user-defined command case}

otherwise                              {not any kind of command name symbol}
    sys_stat_set (escr_subsys_k, escr_err_notcmd_k, stat); {symbol is not a command}
    sys_stat_parm_vstr (e.parse_p^.cmd, stat);
    return;
    end;                               {end of command symbol type cases}
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
  escr_write_vstr (e, e.parse_p^.ibuf, stat);
  if sys_error(stat) then return;
  goto loop_line;
{
********************
*
*   Normal exit points.
*
*   The input stream has ended.
}
leave_end:
  escr_exblock_close (e, stat);        {pop to the parent execution block}
  if sys_error(stat) then return;

  if e.exblock_p <> nil then begin     {didn't pop off the original top block ?}
    sys_stat_set (escr_subsys_k, escr_err_inend_k, stat);
    return;
    end;
{
*   There is no current execution block.  This is assumed to be because the
*   original execution block was deleted.  This block had its parent pointer set
*   to NIL at the beginning of this routine.
}
leave:
  e.exblock_p := parent_p;             {pop to original parent block}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_ATLINE (E, LINE, STAT)
*
*   Execute script code starting at the indicated input file line.
*
*   The script code will be run in the current execution block, unless there is
*   none.  In that case, the top level execution block is first created, then
*   the script run in it.  In either case, the execution block that the script
*   is run in will be deleted.
*
*   To guarantee the new code is run in a new clean context (no local variables,
*   arguments, etc), call ESCR_RUN_CLEAN before this routine.
}
procedure escr_run_atline (            {run starting at specific input files line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to first input files line to execute}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}
  if line_p = nil then return;         {nothing to do ?}
{
*   Create the new execution block to run the code in.
}
  if e.exblock_p = nil then begin      {no execution block ?}
    escr_exblock_new (e, stat);        {create top level execution block}
    if sys_error(stat) then return;
    escr_exblock_ulab_init (e);        {top level block has unique labels context}
    end;

  escr_exblock_inline_set (e, line_p, stat); {set next source line to execute}
  if sys_error(stat) then return;
  escr_run (e, stat);                  {run code, delete block when done}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_CONN (E, CONN, STAT)
*
*   Execute script code starting at the current position of a open file.  CONN
*   is the existing connection to the file.
}
procedure escr_run_conn (              {run at current line of open file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     conn: file_conn_t;           {pointer to I/O connection state}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  infile_p: escr_infile_p_t;           {pointer to input file descriptor}

begin
  escr_infile_new (e, conn.tnam, infile_p); {create new input file descriptor}
  escr_infile_add_lines (e, infile_p^, conn, stat); {save file contents in memory}
  if sys_error(stat) then return;

  e.exstat := 0;                       {init script exit status code}
  escr_run_atline (                    {run the code in this file}
    e,                                 {state for this use of the ESCR system}
    infile_p^.lfirst_p,                {pointer to line to start running at}
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

  if
      (e.exblock_p <> nil) and then    {a current execution block exists ?}
      (e.exblock_p^.bltype = escr_exblock_top_k) {it is the top block ?}
      then begin
    escr_exblock_arg_addn (            {add full script pathname as block argument -1}
      e,                               {state for this use of the ESCR system}
      infile_p^.tnam,                  {argument string}
      -1);                             {argument number}
    end;

  e.exstat := 0;                       {init script exit status code}
  escr_run_atline (                    {run the code in this file}
    e,                                 {state for this use of the ESCR system}
    infile_p^.lfirst_p,                {pointer to line to start running at}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_RUN_CLEAN (E, STAT)
*
*   Clean out any existing execution state.  This stops any execution in
*   progress.  There will be no execution context when this routine returns.
}
procedure escr_run_stop (              {unconditionally stop execution}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  sys_error_none (stat);               {init to no error encountered}

  while e.exblock_p <> nil do begin    {in a execution block ?}
    escr_exblock_close (e, stat);      {end this block and pop back to parent}
    if sys_error(stat) then return;
    end;                               {back to close parent block, if any}
  end;
