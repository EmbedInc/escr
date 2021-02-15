{   Routines that write the output to memory instead of files.  The FLINES
*   library is used to keep text lines in memory, and to manage collections of
*   such lines.
}
module wcoll;
define escr_out_to_coll;
%include 'escr2.ins.pas';

type
  dat_p_t = ^dat_t;
  dat_t = record                       {data passed to callback routines}
    e_p: escr_p_t;                     {pointer to ESCR library use state}
    coll_p: fline_coll_p_t;            {pointer to the collection to write to}
    end;

procedure escr_wcoll_line (            {callback routine for writing one line}
  in      context_p: univ_ptr;         {pointer to our context}
  in      buf: univ string_var_arg_t;  {string to write as text line}
  in out  conn: file_conn_t;           {I/O connection}
  out     stat: sys_err_t);            {completion status, initialized to no err}
  val_param; forward;

procedure escr_wcoll_close (           {callback routine for closing the connection}
  in      context_p: univ_ptr;         {pointer to our context}
  in out  conn: file_conn_t);          {I/O connection}
  val_param; forward;
{
********************************************************************************
*
*   Subroutine ESCR_OUT_TO_COLL (E, COLL)
*
*   Cause the preprocessor output to be saved in memory instead of written to a
*   file.  Subsequent output lines will be added to the end of the lines
*   collection COLL.
}
procedure escr_out_to_coll (           {route output to memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  coll: fline_coll_t);         {collection to write subsequent lines to}
  val_param;

var
  dat_p: dat_p_t;                      {pointer to our private context}
  stat: sys_err_t;

begin
  util_mem_grab (                      {allocate private context}
    sizeof(dat_p^), e.mem_p^, true, dat_p);
  dat_p^.e_p := addr(e);               {link back to ESCR library use state}
  dat_p^.coll_p := addr(coll);         {save pointer to where to write new lines}

  escr_out_new (e);                    {create new blank output level}

  file_open_call_wtxt (                {init callback I/O connection}
    coll.name_p^, dat_p, e.out_p^.conn, stat);
  sys_error_abort (stat, '', '', nil, 0);

  file_call_set_wtxt (                 {set callback routine for writing a line}
    e.out_p^.conn,                     {I/O connection}
    addr(escr_wcoll_line),             {pointer to callback routine}
    stat);
  sys_error_abort (stat, '', '', nil, 0);

  file_call_set_close (                {set callback routine for closing connection}
    e.out_p^.conn,                     {the I/O connection}
    addr(escr_wcoll_close),            {pointer to the callback routine}
    stat);
  sys_error_abort (stat, '', '', nil, 0);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_WCOLL_LINE (CONTEXT_P, BUF, CONN, STAT)
*
*   This routine is automatically called by the FILE library when a text line is
*   being written to a callback I/O connection.
*
*   In our case, CONTEXT_P is a pointer to our private context for this I/O
*   connection.  BUF is the text line being written.  CONN is the I/O connection
*   descriptor.  STAT has been initialized to no error before this routine is
*   called.
}
procedure escr_wcoll_line (            {callback routine for writing one line}
  in      context_p: univ_ptr;         {pointer to our context}
  in      buf: univ string_var_arg_t;  {string to write as text line}
  in out  conn: file_conn_t;           {I/O connection}
  out     stat: sys_err_t);            {completion status, initialized to no err}
  val_param;

var
  dat_p: dat_p_t;                      {pointer to our private context}

begin
  dat_p := context_p;                  {get pointer to our private context}

  fline_line_add_end (                 {add the line to the end of the collection}
    dat_p^.e_p^.fline_p^,              {FLINE library use state}
    dat_p^.coll_p^,                    {collection to add the line to}
    buf);                              {string to add as the new line}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_WCOLL_CLOSE (CONTEXT_P, CONN)
*
*   This routine is called automatically by the FILE library when a callback I/O
*   conntection is being closed.
}
procedure escr_wcoll_close (           {callback routine for closing the connection}
  in      context_p: univ_ptr;         {pointer to our context}
  in out  conn: file_conn_t);          {I/O connection}
  val_param;

var
  dat_p: dat_p_t;                      {pointer to our private data}
  mem_p: util_mem_context_p_t;         {mem context private data is under}

begin
  dat_p := context_p;                  {get pointer to our private data}
  mem_p := dat_p^.e_p^.mem_p;          {get pointer to ESCR mem context}

  util_mem_ungrab (                    {release our private data for this I/O conn}
    dat_p, mem_p^);
  end;
