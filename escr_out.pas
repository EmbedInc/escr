{   Output file handling.
}
module escr_out;
define escr_out_open;
define escr_out_close;
define escr_out_close_all;
define escr_write_vstr;
define escr_write_obuf;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_OUT_OPEN (E, FNAM, STAT)
*
*   Switch the current output file to FNAM.  The previous output file state is
*   saved, and will be returned to when this new file is closed.
}
procedure escr_out_open (              {open new output file, save previous state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to switch writing to}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  o_p: escr_outfile_p_t;               {pointer to new output file descriptor}

begin
  util_mem_grab (                      {allocate new output file descriptor}
    sizeof(o_p^), e.mem_p^, true, o_p);

  file_open_write_text (fnam, '', o_p^.conn, stat); {open the file}
  if sys_error(stat) then begin        {error opening file ?}
    util_mem_ungrab (o_p, e.mem_p^);   {deallocate new descriptor}
    return;                            {return with the error}
    end;

  o_p^.prev_p := e.out_p;              {save pointer to old output file}
  e.out_p := o_p;                      {switch to new output file}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_OUT_CLOSE (E, DEL)
*
*   Close the current output file and pop back to the previous output file.
*   OUT_P will be NIL if the original output file is closed.  The closed file
*   will be deleted when DEL is TRUE.
}
procedure escr_out_close (             {close the current output file, pop previous}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the file}
  val_param;

var
  o_p: escr_outfile_p_t;               {pointer to state of output file to close}
  stat: sys_err_t;

begin
  o_p := e.out_p;                      {save pointer to state of the file to delete}
  if o_p = nil then return;            {no current output file, nothing to do ?}
  e.out_p := o_p^.prev_p;              {pop back to previous output file}

  file_close (o_p^.conn);              {close the file}
  if del then begin                    {supposed to delete the file ?}
    file_delete_name (o_p^.conn.tnam, stat); {try to delete the file}
    end;

  util_mem_ungrab (o_p, e.mem_p^);     {deallocate descriptor for old output file}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_OUT_CLOSE_ALL (E, DEL)
*
*   Close all the output files.  The files are deleted when DEL is TRUE.  OUT_P
*   will be NIL when this routine returns.
}
procedure escr_out_close_all (         {close all output files}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the file files}
  val_param;

begin
  while e.out_p <> nil do begin        {loop until all output files closed}
     escr_out_close (e, del);          {delete this output file, pop back to previous}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_WRITE_VSTR (E, S, STAT)
*
*   Write the string in S as the next line to the current output file.
}
procedure escr_write_vstr (            {write var string to current output file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {the string to write}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  if e.out_p = nil then begin          {no output file ?}
    sys_stat_set (escr_subsys_k, escr_err_noutwr_k, stat);
    return;
    end;

  file_write_text (s, e.out_p^.conn, stat); {write string as next output file line}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_WRITE_OBUF (E, STAT)
*
*   Write the contents of the global string OBUF as the next line to the
*   output file.  OBUF is reset to empty.
}
procedure escr_write_obuf (            {write line to output file from OBUF}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_write_vstr (e, e.obuf, stat);   {write OBUF as next output file line}
  if sys_error(stat) then return;
  e.obuf.len := 0;                     {reset the output line buffer to empty}
  end;
