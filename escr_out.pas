{   Output file handling.
}
module escr_out;
define escr_out_new;
define escr_out_remove;
define escr_out_open_file;
define escr_out_close;
define escr_out_close_all;
define escr_write_vstr;
define escr_write_obuf;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_OUT_NEW (E)
*
*   Low level routine to create a new output file descriptor.  E.OUT_P will
*   point to the new descriptor, which will point to the previous, if any.  The
*   new descriptor is allocated and linked into the system, but not otherwise
*   filled in.
*
*   ESCR_OUT_REMOVE performs the reverse operation of this routine.
}
procedure escr_out_new (               {make new output file level, not filled in}
  in out  e: escr_t);                  {ESCR lib use state, E.OUT_P pnt to new level}
  val_param;

var
  o_p: escr_outfile_p_t;               {pointer to new output file descriptor}

begin
  util_mem_grab (                      {allocate new output file descriptor}
    sizeof(o_p^), e.mem_p^, true, o_p);

  o_p^.prev_p := e.out_p;              {link new level back to previous}
  e.out_p := o_p;                      {switch to this new output level}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_OUT_REMOVE (E)
*
*   Low level routine to remove the current output file level.  This routine
*   only unlinks the descriptor and deallocates it.  The content of the existing
*   output descriptor is otherwise ignored.
*
*   This routine does the revers of ESCR_OUT_NEW.
}
procedure escr_out_remove (            {remove and dealloc curr out, back to previous}
  in out  e: escr_t);                  {ESCR lib use state, E.OUT_P pnt to previous}
  val_param;

var
  o_p: escr_outfile_p_t;               {pointer to descriptor to remove}

begin
  if e.out_p = nil then return;        {there is no current output, nothing to do ?}

  o_p := e.out_p;                      {save pointer to the existing level}
  e.out_p := o_p^.prev_p;              {point back to previous level}

  util_mem_ungrab (o_p, e.mem_p^);     {deallocate descriptor for old output level}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_OUT_OPEN_FILE (E, FNAM, STAT)
*
*   Switch the current output file to FNAM.  The previous output file state is
*   saved, and will be returned to when this new file is closed.
}
procedure escr_out_open_file (         {open new output file, save previous state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to switch writing to}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  escr_out_new (e);                    {create new blank output level}

  file_open_write_text (fnam, '', e.out_p^.conn, stat); {open the file}
  if sys_error(stat) then begin        {error opening file ?}
    escr_out_remove (e);               {remove new level, back to old}
    return;                            {return with the error}
    end;
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
  stat: sys_err_t;

begin
  if e.out_p = nil then return;        {no current output, nothing to do ?}

  file_close (e.out_p^.conn);          {close the file}
  if                                   {delete the file too ?}
      del and                          {caller requested deletion ?}
      (e.out_p^.conn.obty = file_obty_file_k) {is a file ?}
      then begin
    file_delete_name (e.out_p^.conn.tnam, stat); {try to delete the file}
    end;

  escr_out_remove (e);                 {delete this output level, back to previous}
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
    escr_out_close (e, del);           {delete this output file, pop back to previous}
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
