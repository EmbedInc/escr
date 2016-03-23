{   Program ESCR <input file name> [<output file name>]
*
*   Preprocess the Microchip PIC assembler source file to produce a file
*   that can be passed to the assembler.  See the ESCR documentation
*   file for details.
*
*   By default, the output file is written to the current directory.
}
program escr;
define com;
%include 'escr.ins.pas';

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  i: sys_int_machine_t;                {scratch integer and loop counter}
  im: sys_int_max_t;                   {integer value for variable in symbol table}
  fp: sys_fp_max_t;                    {floating point value for variable in symbol table}
  fnam_in,                             {input file name}
  fnam_out:                            {output file name}
    %include '(cog)lib/string_treename.ins.pas';
  conn: file_conn_t;                   {connection to top level input file}
  sym_p: sym_p_t;                      {pointer to symbol in symbol table}
  infile_p: infile_p_t;                {pointer to top level input file info}
  nclen: string_index_t;               {string length with comment removed}
  str_p: string_var_p_t;               {pointer to current input line string}
  osuff: string;                       {output file suffix}
  cmdlist:                             {all command names, upper case, space separated}
    %include '(cog)lib/string8192.ins.pas';

  opt:                                 {command line option name}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  tk:                                  {scratch token}
    %include '(cog)lib/string8192.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  iname_set: boolean;                  {TRUE if the input file name already set}
  oname_set: boolean;                  {TRUE if the output file name already set}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts,
  loop_line, done_cmd, no_cmd, error, leave, leave_all;
{
*******************************************************************************
*
*   Local subroutine ADDCMD (NAME)
*
*   Add NAME to the end of the command names list.  NAME is an ordinary string,
*   but must be all upper case.
}
procedure addcmd (                     {add command to list}
  in      name: string);               {command name, must be correct case}
  val_param; internal;

var
  vname: string_var32_t;               {var string command name}

begin
  vname.max := size_char(vname.str);   {init local var string}

  string_vstring (vname, name, size_char(name)); {convert name to var string}
  string_append_token (cmdlist, vname);
  end;
{
*******************************************************************************
*
*   Start of main routine.
}
begin
  cmd.max := size_char(cmd.str);       {init var strings in common block}
  cmd.len := 0;
  ibuf.max := size_char(ibuf.str);
  ibuf.len := 0;
  lparm.max := size_char(lparm.str);
  lparm.len := 0;
  obuf.max := size_char(obuf.str);
  obuf.len := 0;

  nflags := 0;                         {init to no /FLAG flags created}
  flag_byten := 0;                     {init to no GFLx flag bytes required}
  flag_bitn := 0;                      {number of next flag bit within flag byte}
  files_p := nil;                      {init list of input files to empty}
  exblock_p := nil;                    {init to no current execution block}
  inhibit_p := nil;                    {init to no execution inhibit}
  labeln := 1;                         {init globally unique number for next local label}
  util_mem_context_get (               {create mem context for symbol table}
    util_top_mem_context, mem_sytable_p);
  util_mem_context_get (               {create mem context for symbol data}
    mem_sytable_p^, mem_sym_p);
  string_hash_create (                 {create the symbol table}
    sym,                               {symbol table to create}
    sym_nbuck_k,                       {number of buckets in the hash table}
    max_namelen_k,                     {max allowed characters in symbol name}
    sizeof(sym_p_t),                   {size of data stored in each table entry}
    [string_hashcre_memdir_k],         {use the supplied memory context directly}
    mem_sytable_p^);                   {pointer to memory context to use}
  util_mem_context_get (               {create mem context for top level execution block}
    util_top_mem_context, mem_top_p);

  inh_new;                             {create the root execution inhibit state}
  inhibit_p^.inhty := inhty_root_k;
  exblock_new;                         {create top level execution block}
  exblock_loclab_init;                 {top level block always has local labels}
{
*   Initialize our state before reading the command line options.
}
  string_cmline_init;                  {init for reading the command line}
  iname_set := false;                  {no input file name specified}
  oname_set := false;                  {no output file name specified}
{
*   Back here each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  if (opt.len >= 1) and (opt.str[1] <> '-') then begin {implicit pathname token ?}
    if not iname_set then begin        {input file name not set yet ?}
      string_copy (opt, fnam_in);      {set input file name}
      iname_set := true;               {input file name is now set}
      goto next_opt;
      end;
    if not oname_set then begin        {output file name not set yet ?}
      string_copy (opt, fnam_out);     {set output file name}
      oname_set := true;               {output file name is now set}
      goto next_opt;
      end;
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (opt,                {pick command line option name from list}
    '-IN -OUT -SET -I -F -S -NOT',
    pick);                             {number of keyword picked from list}
  case pick of                         {do routine for specific option}
{
*   -IN filename
}
1: begin
  if iname_set then begin              {input file name already set ?}
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_cmline_token (fnam_in, stat);
  iname_set := true;
  end;
{
*   -OUT filename
}
2: begin
  if oname_set then begin              {output file name already set ?}
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_cmline_token (fnam_out, stat);
  oname_set := true;
  end;
{
*   -SET name
}
3: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not sym_name (parm) then goto parm_bad; {check for illegal symbol name}
  sym_new_var (parm, dtype_bool_k, 0, true, sym_p); {create the new variable}
  sym_p^.var_val.bool := true;         {set initial value of new variable}
  end;
{
*   -I name int
}
4: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not sym_name (parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;
  string_t_int_max (parm, im, stat);   {convert to integer}
  if sys_error(stat) then goto err_parm;

  sym_new_var (tk, dtype_int_k, 0, true, sym_p); {create the new variable}
  sym_p^.var_val.int := im;            {set variable value}
  end;
{
*   -F name val
}
5: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not sym_name (parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;
  string_t_fpmax (parm, fp, [], stat); {convert to floating point}
  if sys_error(stat) then goto err_parm;

  sym_new_var (tk, dtype_fp_k, 0, true, sym_p); {create the new variable}
  sym_p^.var_val.fp := fp;             {set variable value}
  end;
{
*   -S name string
}
6: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not sym_name (parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;

  sym_new_var (tk, dtype_str_k, parm.len, true, sym_p); {create the new variable}
  string_copy (parm, sym_p^.var_val.str); {set variable value}
  end;
{
*   -NOT name
}
7: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not sym_name (parm) then goto parm_bad; {check for illegal symbol name}
  sym_new_var (parm, dtype_bool_k, 0, true, sym_p); {create the new variable}
  sym_p^.var_val.bool := false;        {set initial value of new variable}
  end;
{
*   Unrecognized command line option.
}
otherwise
    string_cmline_opt_bad;             {unrecognized command line option}
    end;                               {end of command line option case statement}

err_parm:                              {jump here on error with parameter}
  string_cmline_parm_check (stat, opt); {check for bad command line option parameter}
  goto next_opt;                       {back for next command line option}

parm_bad:                              {jump here on got illegal parameter}
  string_cmline_reuse;                 {re-read last command line token next time}
  string_cmline_token (parm, stat);    {re-read the token for the bad parameter}
  sys_msg_parm_vstr (msg_parm[1], parm);
  sys_msg_parm_vstr (msg_parm[2], opt);
  sys_message_bomb ('string', 'cmline_parm_bad', msg_parm, 2);

done_opts:                             {done with all the command line options}
{
*   Process the input file name.
}
  if not iname_set then begin          {no input file name supplied ?}
    sys_message_bomb ('file', 'no_input_filename', nil, 0);
    end;

  file_open_read_text (fnam_in,        {test open the input file name}
    '.ins.aspic .aspic .ins.dspic .dspic',
    conn, stat);
  sys_error_abort (stat, '', '', nil, 0);
  file_close (conn);                   {close file now that we have info about it}

  case conn.ext_num of                 {which suffix did input file have ?}
1:  begin                              {.INS.ASPIC}
      lang := lang_aspic_k;            {set input file language ID}
      osuff := '.inc';                 {set output file suffix}
      end;
2:  begin                              {.ASPIC}
      lang := lang_aspic_k;            {set input file language ID}
      osuff := '.asm';                 {set output file suffix}
      end;
3:  begin                              {.INS.DSPIC}
      lang := lang_dspic_k;            {set input file language ID}
      osuff := '.inc';                 {set output file suffix}
      end;
4:  begin                              {.DSPIC}
      lang := lang_dspic_k;            {set input file language ID}
      osuff := '.S';                   {set output file suffix}
      end;
otherwise                              {anything else, like .ASPIC}
    sys_message_bomb ('pic', 'err_escr_insuff', nil, 0);
    end;

  infile_open (conn.tnam, infile_p, stat); {read and save top level input file}
  sys_error_abort (stat, '', '', nil, 0);
  exblock_inline_set (infile_p^.lines_p); {set input file line to start at}
{
*   Process the output file name.
}
  if oname_set
    then begin                         {output file name supplied explicitly}
      osuff := '';                     {no mandatory output suffix when fnam supplied}
      end
    else begin                         {use default output file name}
      string_copy (conn.gnam, fnam_out);
      end
    ;

  sys_mem_alloc (sizeof(out_p^), out_p); {allocate root output file descriptor}
  out_p^.prev_p := nil;                {indicate this is root output file}
  file_open_write_text (fnam_out, osuff, out_p^.conn, stat); {open the output file}
  sys_error_abort (stat, '', '', nil, 0);
{
*   Initialize list of input file command names.
}
  cmdlist.len := 0;                    {init command names list to empty}
  addcmd ('INBIT');                    {1}
  addcmd ('OUTBIT');                   {2}
  addcmd ('FLAG');                     {3}
  addcmd ('VAR');                      {4}
  addcmd ('SET');                      {5}
  addcmd ('CONST');                    {6}
  addcmd ('DEL');                      {7}
  addcmd ('SYLIST');                   {8}
  addcmd ('INCLUDE');                  {9}
  addcmd ('WRITE');                    {10}
  addcmd ('SHOW');                     {11}
  addcmd ('SUBROUTINE');               {12}
  addcmd ('ENDSUB');                   {13}
  addcmd ('CALL');                     {14}
  addcmd ('RETURN');                   {15}
  addcmd ('IF');                       {16}
  addcmd ('THEN');                     {17}
  addcmd ('ELSE');                     {18}
  addcmd ('ENDIF');                    {19}
  addcmd ('BLOCK');                    {20}
  addcmd ('REPEAT');                   {21}
  addcmd ('QUIT');                     {22}
  addcmd ('ENDBLOCK');                 {23}
  addcmd ('WRITETO');                  {24}
  addcmd ('WRITEEND');                 {25}
  addcmd ('STOP');                     {26}
  addcmd ('MACRO');                    {27}
  addcmd ('ENDMAC');                   {28}
  addcmd ('QUITMAC');                  {29}
  addcmd ('LOOP');                     {30}
  addcmd ('ENDLOOP');                  {31}

  inline_func_init;                    {init for processing inline functions}
{
*   Process the input file and write the output file.
}
loop_line:                             {back here each new line from input file}
  if not infile_getline(str_p) then begin {get next input line, end of input this block ?}
    if exblock_p^.prev_p = nil then goto leave; {end of top level execution block ?}
    err_atline ('pic', 'exblock_inend', nil, 0); {input end before execution block end}
    end;
{
*   Handle blank lines.  These are passed to the output file if not in a nested
*   execution block.  Otherwise, blank lines are completely ignored, and are not
*   copied to the output file.
}
  string_unpad (str_p);                {delete trailing space from the input line}
  if str_p^.len = 0 then begin         {this is a blank line ?}
    if exblock_p^.prev_p = nil
      then begin                       {in top level execution block}
        ibuf.len := 0;                 {"copy" the input line to the output line}
        goto no_cmd;                   {pass to output file}
        end
      else begin                       {in a nested execution block}
        goto loop_line;                {ignore this line}
        end
      ;
    end;
{
*   Check for "//" comment.  These lines are ignored at the preprocessor level
*   and not written to the output file.  This is different from regular
*   assembler comments, which are copied to the output file.
}
  ip := 1;                             {init input line parse index}
  while ip < str_p^.len do begin       {scan forwards to first non-blank}
    if str_p^.str[ip] <> ' ' then exit; {found first non-blank ?}
    ip := ip + 1;
    end;
  if                                   {this is a preprocessor comment line ?}
      (ip < str_p^.len) and then       {at least two chars starting at first non-blank ?}
      (str_p^.str[ip] = '/') and (str_p^.str[ip + 1] = '/') {starts with "//" ?}
    then goto loop_line;               {totally ignore this line}

  if inhibit_p^.inh
    then begin                         {execution is inhibited}
      string_copy (str_p^, ibuf);      {copy line without expanding inline functions}
      end
    else begin                         {normal execution}
      inline_expand_line (str_p^, ibuf); {expand all inline functions on this line}
      end
    ;
  ip := 1;                             {init IBUF parse index}
  string_token (ibuf, ip, cmd, stat);  {get first input line token into CMD}
  if sys_error(stat) then goto no_cmd; {definitely no preproc command on this line ?}
  if cmd.len < 2 then goto no_cmd;     {not long enough to be preproc command ?}
  if cmd.str[1] <> '/' then goto no_cmd; {token doesn't have preproc cmd prefix ?}

  for i := 1 to cmd.len - 1 do begin   {shift command name to delete leading "/"}
    cmd.str[i] := cmd.str[i + 1];
    end;
  cmd.len := cmd.len - 1;              {update command length for "/" removed}
  string_upcase (cmd);                 {make upper case for keyword matching}
{
*   The line in IBUF contains a preprocessor command.  The upper case command
*   name with the leading "/" stripped is in CMD.
}
  uptocomm (ibuf, nclen);              {get line length with comment stripped}
  ibuf.len := nclen;
{
*   Handle the specific preprocessor command.
}
  string_tkpick (cmd, cmdlist, pick);  {pick command name from list}
  sys_error_none (stat);               {init to no error in command routine}
  case pick of                         {which preprocessor command is it ?}
1:  escr_cmd_inbit (stat);
2:  escr_cmd_outbit (stat);
3:  escr_cmd_flag (stat);
4:  escr_cmd_var (stat);
5:  escr_cmd_set (stat);
6:  escr_cmd_const (stat);
7:  escr_cmd_del (stat);
8:  escr_cmd_sylist (stat);
9:  escr_cmd_include (stat);
10: escr_cmd_write (stat);
11: escr_cmd_show (stat);
12: escr_cmd_subroutine (stat);
13: escr_cmd_endsub (stat);
14: escr_cmd_call (stat);
15: escr_cmd_return (stat);
16: escr_cmd_if (stat);
17: escr_cmd_then (stat);
18: escr_cmd_else (stat);
19: escr_cmd_endif (stat);
20: escr_cmd_block (stat);
21: escr_cmd_repeat (stat);
22: escr_cmd_quit (stat);
23: escr_cmd_endblock (stat);
24: escr_cmd_writeto (stat);
25: escr_cmd_writeend (stat);
26: begin                              {/STOP}
      if inhibit_p^.inh then goto done_cmd; {execution is inhibited ?}
      goto leave_all;
      end;
27: escr_cmd_macro (stat);
28: escr_cmd_endmac (stat);
29: escr_cmd_quitmac (stat);
30: escr_cmd_loop (stat);
31: escr_cmd_endloop (stat);

otherwise
    sys_msg_parm_vstr (msg_parm[1], cmd);
    sys_message_parms ('file', 'test_client_cmd_bad', msg_parm, 1);
    sys_stat_set (sys_subsys_k, sys_stat_failed_k, stat);
    goto error;
    end;

done_cmd:                              {done processing the preprocessor command}
  if sys_error(stat) then goto error;
  if not inhibit_p^.inh then begin     {don't check unused tokens if not executed}
    get_end;                           {error if input line not exhausted}
    end;
  if obuf.len > 0 then write_obuf;     {write any line fragment left in out buffer}
  goto loop_line;
{
*   This input line does not contain a preprocessor command.
}
no_cmd:
  if inhibit_p^.inh then goto loop_line; {execution inhibited for this line ?}
  if macro_run (stat) then goto loop_line; {macro invocation on this line processed ?}
  if sys_error(stat) then goto error;
  write_vstr (ibuf, stat);
  if sys_error(stat) then goto error;
  goto loop_line;
{
*   An error has occurred.  STAT is set indicating an error.
}
error:
  sys_error_print (stat, '', '', nil, 0); {show the error indicated by STAT}
  err_atline ('', '', nil, 0);         {show current input hierarchy position and bomb}

leave:
  if inhibit_p <> nil then begin       {still inside execution inhibit ?}
    case inhibit_p^.inhty of           {what kind of inhibit it is ?}
inhty_if_k: begin                      {in IF construct}
        sys_message_bomb ('pic', 'err_end_inh_if', nil, 0);
        end;
      end;                             {end of inhibit type cases}
    end;                               {end of inhibit exists at end of source stream}

leave_all:                             {common exit point}
  close_out_all (false);               {close and keep all output files}
  end.
