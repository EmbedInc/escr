{   Routines for managing input files.
}
module escr_infile;
define escr_infile_open;
define escr_infile_pop;
define escr_infile_getline;
define escr_infile_skipline;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_OPEN (E, FNAM, INFILE_P, STAT)
*
*   Return the pointer to the input file descriptor in INFILE_P for the file
*   indicated by FNAM.  If the file was previously read, then INFILE_P is
*   returned pointing to the data already in memory.  If not, the file is
*   read into memory and INFILE_P is returned pointing to the newly created
*   input file descriptor.  Each unique input file is only read once and
*   stored in memory.
}
procedure escr_infile_open (           {find file data or read it into memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {file name}
  out     infile_p: escr_infile_p_t;   {returned pointer to input file descriptor}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  tnam: string_treename_t;             {full pathname of the input file}
  file_p: escr_infile_p_t;             {points to current input files list entry}
  conn: file_conn_t;                   {connection to the input file}
  line_p: escr_inline_p_t;             {points to current input line descriptor}
  line_pp: escr_inline_pp_t;           {points to where to link next line descriptor to}
  p: string_index_t;                   {index into BUF}
  buf: string_var8192_t;               {one line input buffer}

begin
  tnam.max := size_char(tnam.str);     {init local var strings}
  buf.max := size_char(buf.str);
  sys_error_none (stat);               {init to no error encountered}

  string_treename (fnam, tnam);        {make full input file treename}
  file_p := e.files_p;                 {init to first entry in cached files list}
  while file_p <> nil do begin         {scan thru list of files already read and cached}
    if string_equal (file_p^.tnam, tnam) then begin {found requested file in list ?}
      infile_p := file_p;              {pass back pointer to existing file info}
      return;
      end;
    file_p := file_p^.next_p;          {advance to next cached files list entry}
    end;                               {back and check this new list entry}
{
*   The requested file has not previously been read and saved in memory.
}
  file_open_read_text (tnam, '', conn, stat); {open the new input file}
  if sys_error(stat) then return;

  util_mem_grab (                      {alloc mem for the input file descriptor}
    sizeof(file_p^), e.mem_p^, false, file_p);
  file_p^.next_p := e.files_p;         {fill in input file descriptor}
  file_p^.tnam.max := size_char(file_p^.tnam.str);
  string_copy (tnam, file_p^.tnam);
  file_p^.lines_p := nil;

  e.files_p := file_p;                 {link this descriptor to start of files list}
{
*   Read all the lines from the file and save them in memory.
}
  line_pp := addr(file_p^.lines_p);    {init where to link first input line desc to}

  while true do begin                  {back here for each new input line}
    file_read_text (conn, buf, stat);  {read new input line}
    if file_eof(stat) then exit;       {done reading this file ?}
    if sys_error(stat) then exit;      {hard error ?}
    for p := 1 to buf.len do begin     {scan over all characters on the line}
      if ord(buf.str[p]) < 32 then buf.str[p] := ' '; {replace all control chars with blank}
      end;
    string_unpad (buf);                {remove trailing blanks from this line}

    util_mem_grab (                    {alloc mem for this input line descriptor}
      sizeof(line_p^), e.mem_p^, false, line_p);
    line_p^.next_p := nil;             {init to no following line}
    line_p^.file_p := file_p;          {point to parent file}
    line_p^.lnum := conn.lnum;         {save input line number}
    string_alloc (buf.len, e.mem_p^, false, line_p^.str_p); {alloc mem for input line string}
    string_copy (buf, line_p^.str_p^); {save this input line string in memory}
    line_pp^ := line_p;                {link new line descriptor to end of list}
    line_pp := addr(line_p^.next_p);   {update where to write next forward link}
    end;                               {back to do next input file line}
{
*   Done reading the input file one way or another.  STAT indicates whether there
*   was any error.
}
  file_close (conn);                   {close the input file}
  infile_p := file_p;                  {return pointer to the new file descriptor}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_POP (E)
*
*   Pop back one level of input file state within the current execution block.
*   If the top level of the block is popped, then its INPOS_P pointer is set to
*   NIL.  Nothing is done if this pointer is already NIL.
}
procedure escr_infile_pop (            {pop back one nested input file level}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  pos_p: escr_inpos_p_t;               {pointer to input position state}

begin
  if e.exblock_p = nil then return;    {no current execution block ?}

  pos_p := e.exblock_p^.inpos_p;       {get pointer to curr input position}
  if pos_p = nil then return;          {already popped everything ?}

  e.exblock_p^.inpos_p := pos_p^.prev_p; {pop back one level}
  util_mem_ungrab (pos_p, e.exblock_p^.mem_p^); {deallocate old input position descriptor}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_GETLINE (E, STR_P)
*
*   Get the next input stream source line by returning STR_P pointing to it.
*   STR_P will be returned NIL when the input stream has been exhausted.
*
*   LAST_P in the current input stream position state is updated to point to the
*   returned line.  LINE_P in the position state is updated to point to the next
*   input line.  LINE_P will be set to NIL when the last line in the current
*   input file is returned.
*
*   This function automatically pops back to the parent input file within the
*   current execution block as required.  STR_P is only returned NIL when the
*   end of the top level input file for the current execution block is
*   encountered.
}
procedure escr_infile_getline (        {get next input stream source line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     str_p: string_var_p_t);      {returned pointer to source line or NIL}
  val_param;

var
  pos_p: escr_inpos_p_t;               {pointer to input position state}
  line_p: escr_inline_p_t;             {pointer to input line info}

label
  retry;

begin
  str_p := nil;                        {init to end of input is reached}

  if e.exblock_p = nil then begin
    writeln ('INTERNAL ERROR: No execution block defined in INFILE_GETLINE.');
    sys_bomb;
    end;

retry:                                 {back here after popping back to prev input file}
  pos_p := e.exblock_p^.inpos_p;       {get pointer to input position state}
  if pos_p = nil then return;          {input stream exhausted this execution block ?}

  line_p := pos_p^.line_p;             {get pointer to next input stream line info}
  if line_p = nil then begin           {end of current input file ?}
    escr_infile_pop (e);               {pop back one input file level}
    goto retry;                        {try again with this new input file}
    end;
  str_p := line_p^.str_p;              {return pointer to this input line}

  pos_p^.last_p := line_p;             {save pointer to new current input line}
  pos_p^.line_p := line_p^.next_p;     {advance to next input line for next time}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_SKIPLINE (E)
*
*   Skip to the next input line.  This is the same as reading and ignoring the next
*   input line.  It is a error if currently at the input stream end of the current
*   execution block.
}
procedure escr_infile_skipline (       {skip next input file line}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  str_p: string_var_p_t;               {pointer to input line to ignore}

begin
  escr_infile_getline (e, str_p);      {get the next input line}
  if str_p = nil then begin
    writeln ('INTERNAL ERROR: End of execution block encountered in INFILE_SKIPLINE.');
    escr_err_atline (e, '', '', nil, 0);
    end;
  end;
