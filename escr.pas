{   Program ESCR fnam arg ... arg
*
*   Run a ESCR script.  FNAM is the script file name.  Such file names always
*   end in ".escr" or ".es".  This file name suffix may be omitted from FNAM.
*
*   The remaining command line arguments become arguments to the top level ESCR
*   execution block.  These can be read by the script code as arguments 1-N in
*   the top level block.
*
*   The special block argument 0 will be FNAM.  This is the script name exactly
*   as referenced on the command line.
*
*   The special block argument -1 will be the full pathname of the script file.
}
program escr;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'strflex.ins.pas';
%include 'fline.ins.pas';
%include 'escr.ins.pas';

var
  fnam:                                {input file name}
    %include '(cog)lib/string_treename.ins.pas';
  e_p: escr_p_t;                       {points to state for this ESCR use}
  exstat: sys_int_machine_t;           {script exit status code}

  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  stat: sys_err_t;                     {completion status code}

begin
  string_cmline_init;                  {init for reading the command line}
  string_cmline_token (fnam, stat);    {get script file name}
  string_cmline_req_check (stat);      {this command line parameter is required}

  escr_open (util_top_mem_context, e_p, stat); {create ESCR system use state}
  sys_error_abort (stat, '', '', nil, 0);

  escr_exblock_new (e_p^, stat);       {create top level execution block}
  sys_error_abort (stat, '', '', nil, 0);
  escr_exblock_ulab_init (e_p^);       {create unique labels list for this block}

  escr_exblock_arg_addn (e_p^, fnam, 0); {add raw script name argument}
{
*   Read the remaining command line options and add them to the execution block
*   as sequential arguments.
}
  while true do begin                  {back here each new command line argument}
    string_cmline_token (parm, stat);  {get next command line argument}
    if string_eos(stat) then exit;     {exhausted the command line ?}
    escr_exblock_arg_add (e_p^, parm); {add to execution block as argument}
    end;                               {back to get next commande line argument}
{
*   Finish setting up the script execution state, and run the script.
}
  escr_set_incsuff (e_p^, '.escr .es'); {required include file name suffixes}

  escr_run_file (                      {execute from start of input file}
    e_p^,                              {state for this use of the ESCR system}
    fnam,                              {generic input file name}
    '.escr .es',                       {allowed file name suffixes}
    stat);
  escr_err_atline_abort (e_p^, stat, '', '', nil, 0);
  exstat := e_p^.exstat;               {save script exit status code}

  escr_close (e_p);                    {end this use of the ESCR system}
  sys_exit_n (exstat);                 {exit with the status code of the script}
  end.
