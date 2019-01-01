{   Routines that cause execution of code.
}
module escr_run;
define escr_run_atline;
define escr_run_atlabel;
define escr_run_conn;
define escr_run_file;
define escr_run_clean;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_RUN_ATLINE (E, LINE, STAT)
*
*   Execute script code starting at the indicated input file line.
*
*   If the current execution block is the top level block, then the new code is
*   run within it.  If no block exists, then a top level block is created, and
*   the new code run within it.  It is a error if other then the top execution
*   block exists.
*
*   To guarantee the new code is run in a new clean context (no local variables,
*   arguments, etc), call ESCR_RUN_CLEAN before this routine.
}
procedure escr_run_atline (            {run starting at specific input files line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to first input files line to execute}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  str_p: string_var_p_t;               {pointer to current source line}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  sym_p: escr_sym_p_t;                 {pointer to symbol in symbol table}
  buf: string_var8192_t;               {temp processed input line buffer}
  inh: boolean;                        {saved inhibit at start of command}

label
  loop_line, no_cmd;

begin
  buf.max := size_char(buf.str);       {init local var string}

  if                                   {in nested execution context ?}
      (e.exblock_p <> nil) and then    {current execution block exists ?}
      ( (e.exblock_p^.prev_p <> nil) or {not top block ?}
        (e.exblock_p^.bltype <> escr_exblock_top_k) {not top block type ?}
        )
      then begin
    sys_stat_set (escr_subsys_k, escr_err_run_nested_k, stat);
    return;
    end;

  if line_p = nil then return;         {nothing to do ?}
{
*   Create the top execution block if it does not already exist.
}
  if e.exblock_p = nil then begin      {no current execution block ?}
    escr_exblock_new (e, stat);        {create top level execution block}
    if sys_error(stat) then return;
    escr_exblock_ulab_init (e);        {top level block has unique labels context}
    end;

  escr_exblock_inline_set (e, line_p, stat); {set next source line to execute}
  if sys_error(stat) then return;
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
  while e.ip <= str_p^.len do begin    {loop until source line exhausted}
    if                                 {syntax exclusion here ?}
        escr_excl_check (              {skip over syntax exclusion, if present}
          e,                           {state for this use of the ESCR system}
          str_p^,                      {input string}
          e.ip,                        {input string parse index}
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
          e.ip,                        {input string parse index}
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
          e.ip,                        {input string parse index}
          e.commscr_p,                 {points to list of syntax exclusions}
          nil,                         {discard the comment characters}
          stat)                        {completion status, always OK on function false}
        then begin
      if sys_error(stat) then return;
      next;                            {back to check after this comment}
      end;

    string_append1 (buf, str_p^.str[e.ip]); {copy this source character to output string}
    e.ip := e.ip + 1;                  {advance to next source character}
    end;                               {back to process next source string character}

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
      escr_inline_expand_line (e, buf, e.ibuf, stat); {expand all inline functions on this line}
      if sys_error(stat) then return;
      end
    ;
  e.ip := 1;                           {init IBUF parse index}
{
*   Check for the line starts with a command.  If so, the upper case command
*   name will be left in E.CMD.  If not, jump to NO_CMD.
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
      while e.ip <= e.ibuf.len do begin {loop until source line exhausted}
        if                             {syntax exclusion here ?}
            escr_excl_check (          {skip over syntax exclusion, if present}
              e,                       {state for this use of the ESCR system}
              e.ibuf,                  {input string}
              e.ip,                    {input string parse index}
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
              e.ibuf,                  {input string}
              e.ip,                    {input string parse index}
              e.commdat_p,             {points to list of comments start/end}
              nil,                     {discard the comment characters}
              stat)                    {completion status, always OK on function false}
            then begin
          if sys_error(stat) then return;
          next;                        {back to check after this comment}
          end;
        string_append1 (buf, e.ibuf.str[e.ip]); {copy this source character to output string}
        e.ip := e.ip + 1;              {advance to next source character}
        end;                           {back to process next source string character}

      e.ip := 1;                       {init parse index to start of string}
      string_token (buf, e.ip, e.cmd, stat); {get first input line token into CMD}
      if sys_error(stat) then goto no_cmd; {definitely no preproc command on this line ?}

      if e.cmdst.len > 0 then begin    {commands start with special string ?}
        if e.cmd.len < (e.cmdst.len + 1) {not long enough to be a command ?}
          then goto no_cmd;
        for ii := 1 to e.cmdst.len do begin {check for command start string}
          if e.cmd.str[ii] <> e.cmdst.str[ii] {mismatch ?}
            then goto no_cmd;
          end;
        for ii := 1 to e.cmd.len - e.cmdst.len do begin {shift to delete command start string}
          e.cmd.str[ii] := e.cmd.str[ii + e.cmdst.len];
          end;
        e.cmd.len := e.cmd.len - e.cmdst.len; {update length to command start removed}
        end;

      string_copy (buf, e.ibuf);       {process line with data file comments stripped}
      end
    else begin
      {
      *   In script mode.  Data file comments don't exist in script mode.
      }
      string_token (e.ibuf, e.ip, e.cmd, stat); {get first input line token into CMD}
      if sys_error(stat) then goto no_cmd; {definitely no command on this line ?}

      if e.cmdst.len > 0 then begin    {command start with special string ?}
        if e.cmd.len < (e.cmdst.len + 1) {not long enough to be a command ?}
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
      end
    ;
{
*   Handle the specific preprocessor command in CMD.
}
  escr_sym_find_type (                 {look up command name}
    e,                                 {state for this use of the ESCR system}
    e.cmd,                             {name to look up}
    escr_sytype_cmd_k,                 {must be command}
    sym_p,                             {returned pointer to symbol data}
    stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {no such command ?}
    sys_stat_set (escr_subsys_k, escr_err_cmdbad_k, stat);
    sys_stat_parm_vstr (e.cmd, stat);
    return;
    end;

  case sym_p^.stype of                 {what kind of symbol is this ?}

escr_sym_icmd_k: begin                 {intrinsic command, implemented by compiled routine}
      inh := e.inhibit_p^.inh;         {save inhibit state at start of command}

      sys_error_none (stat);           {init to no error encountered}
      sym_p^.icmd_p^ (addr(e), stat);  {run the command routine}
      if sys_error(stat) then return;

      if not inh then begin            {check for extra parms if command was run}
        if not escr_get_end (e, stat) then return; {abort on extra parameter}
        end;
      if e.obuf.len > 0 then escr_write_obuf (e, stat); {write any line fragment left in out buffer}
      if sys_error(stat) then return;
      end;

escr_sym_cmd_k: begin                  {user-defined command, implemented with script code}
      if not e.inhibit_p^.inh then begin {only run command if execution not inhibited}
        escr_exblock_new (e, stat);    {create new  execution block}
        if sys_error(stat) then return;
        escr_exblock_refsym (e, sym_p^); {indicate referencing symbol for this command}
        e.exblock_p^.bltype := escr_exblock_cmd_k; {new block is a command}
        e.exblock_p^.args := true;     {this block can take arguments}
        escr_exblock_arg_addn (e, e.cmd, 0); {command name is special argument 0}
        while true do begin            {loop until argument list exhausted}
          if not escr_get_tkraw (e, buf) then exit; {get command parameter}
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
    sys_stat_parm_vstr (e.cmd, stat);
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
  escr_sym_find_type (                 {look up name in symbol table}
    e,                                 {state for this use of ESCR system}
    name,                              {name to look up}
    escr_sytype_label_k,               {must be a label}
    sym_p,                             {returned pointer to the symbol}
    stat);
  if sys_error(stat) then return;
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
    escr_exblock_close (e, stat);      {not anymore}
    if sys_error(stat) then return;
    end;                               {back to close parent block, if any}
  end;
