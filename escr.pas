{   Program ESCR <input file name> [<output file name>]
*
*   Preprocess the Microchip PIC assembler source file to produce a file
*   that can be passed to the assembler.  See the ESCR documentation
*   file for details.
*
*   By default, the output file is written to the current directory.
}
program escr;
%include 'base.ins.pas';
%include 'escr.ins.pas';

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  e_p: escr_p_t;                       {points to state for this ESCR use}
  im: sys_int_max_t;                   {integer value for variable in symbol table}
  fp: sys_fp_max_t;                    {floating point value for variable in symbol table}
  fnam_in:                             {input file name}
    %include '(cog)lib/string_treename.ins.pas';
  sym_p: escr_sym_p_t;                 {pointer to symbol in symbol table}

  opt:                                 {command line option name}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  tk:                                  {scratch token}
    %include '(cog)lib/string8192.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  iname_set: boolean;                  {TRUE if the input file name already set}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts;

begin
  string_cmline_init;                  {init for reading the command line}
  iname_set := false;                  {no input file name specified}

  escr_open (util_top_mem_context, e_p, stat); {create ESCR system use state}
  sys_error_abort (stat, '', '', nil, 0);
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
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (opt,                {pick command line option name from list}
    '-IN -SET -I -F -S -NOT',
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
*   -SET name
}
2: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not escr_sym_name (e_p^, parm) then goto parm_bad; {check for illegal symbol name}
  escr_sym_new_var (                   {create the new variable}
    e_p^, parm, escr_dtype_bool_k, 0, true, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);
  sym_p^.var_val.bool := true;         {set initial value of new variable}
  end;
{
*   -I name int
}
3: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not escr_sym_name (e_p^, parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;
  string_t_int_max (parm, im, stat);   {convert to integer}
  if sys_error(stat) then goto err_parm;

  escr_sym_new_var (                   {create the new variable}
    e_p^, tk, escr_dtype_int_k, 0, true, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);
  sym_p^.var_val.int := im;            {set variable value}
  end;
{
*   -F name val
}
4: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not escr_sym_name (e_p^, parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;
  string_t_fpmax (parm, fp, [], stat); {convert to floating point}
  if sys_error(stat) then goto err_parm;

  escr_sym_new_var (                   {create the new variable}
    e_p^, tk, escr_dtype_fp_k, 0, true, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);
  sym_p^.var_val.fp := fp;             {set variable value}
  end;
{
*   -S name string
}
5: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not escr_sym_name (e_p^, parm) then goto parm_bad; {check for illegal symbol name}
  string_copy (parm, tk);              {save name of new variable}

  string_cmline_token (parm, stat);    {get value string}
  if sys_error(stat) then goto err_parm;

  escr_sym_new_var (                   {create the new variable}
    e_p^, tk, escr_dtype_str_k, parm.len, true, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);
  string_copy (parm, sym_p^.var_val.str); {set variable value}
  end;
{
*   -NOT name
}
6: begin
  string_cmline_token (parm, stat);    {get the variable name}
  if sys_error(stat) then goto err_parm;
  if not escr_sym_name (e_p^, parm) then goto parm_bad; {check for illegal symbol name}

  escr_sym_new_var (                   {create the new variable}
    e_p^, parm, escr_dtype_bool_k, 0, true, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);
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
  if not iname_set then begin          {no input file name supplied ?}
    sys_message_bomb ('file', 'no_input_filename', nil, 0);
    end;

  escr_incsuff (e_p^, '.escr .es');    {required include file name suffixes}

  escr_run_file (                      {execute from start of input file}
    e_p^,                              {state for this use of the ESCR system}
    fnam_in,                           {generic input file name}
    '.escr .es',                       {allowed file name suffixes}
    stat);
  escr_err_atline_abort (e_p^, stat, '', '', nil, 0);

  escr_close (e_p);                    {end this use of the ESCR system}
  end.
