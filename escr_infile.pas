{   Routines for managing input files snippets.  Snippets can represent whole
*   files, or just a chunk of sequential lines that are to be executed unrelated
*   to other contents of the file.
*
*   Snippets that are really whole input files are kept on a chain pointed to
*   by FILES_P in the ESCR use state.  Whole files are read into memory the
*   first time they are referenced, then re-used for any subsequent references.
*
*   Snippets that are sections of a file are given symbolic names and are
*   referenced from the SRC symbol table.
}
module escr_infile;
define escr_infile_new;
define escr_infile_find;
define escr_infile_add_line;
define escr_infile_open;
define escr_infile_pop;
define escr_infile_getline;
define escr_infile_skipline;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_NEW (E, TNAM, INFILE_P)
*
*   Create a new input file snippett descriptor, initialize it, and return
*   INFILE_P pointing to it.  The descriptor is not linked to the rest of the
*   system.
}
procedure escr_infile_new (            {create new input file snippet descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      tnam: univ string_var_arg_t; {full unique treename of the file}
  out     infile_p: fline_coll_p_t);   {returned pointer to input file descriptor}
  val_param;

begin
  util_mem_grab (                      {alloc mem for the input file descriptor}
    sizeof(infile_p^), e.mem_p^, false, infile_p);
  sys_mem_error (infile_p, '', '', nil, 0);

  infile_p^.next_p := nil;             {init to not chained to another input file}
  infile_p^.tnam.max := size_char(infile_p^.tnam.str); {init var string}
  string_copy (tnam, infile_p^.tnam);  {save file name}
  infile_p^.lfirst_p := nil;           {init to no source lines yet}
  infile_p^.llast_p := nil;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_FIND (E, TNAM, INFILE_P)
*
*   Find previously-created source snippet for the whole file indicated by TNAM.
*   TNAM must be the full unique treename of the file.  INFILE_P is returned
*   pointing to the existing source snippet if found, or NIL to indicate not
*   found.
}
procedure escr_infile_find (           {find existing input file descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      tnam: univ string_var_arg_t; {full unique treename of the file}
  out     infile_p: fline_coll_p_t);   {points to snippet, or NIL for not found}
  val_param;

begin
  infile_p := e.files_p;               {init pointer to first file in list}
  while infile_p <> nil do begin       {scan the files list}
    if string_equal (infile_p^.tnam, tnam) then begin {found the file ?}
      return;                          {return with INFILE_P pointing to snippet}
      end;
    infile_p := infile_p^.next_p;      {advance to next file in list}
    end;                               {back to check this new file}
  end;                                 {return indicating no matching file found}
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_ADD_LINE (E, INFILE, LINE, LNUM)
*
*   Add source line to end of existing source snippet.  INFILE is the snippet to
*   add the line to.  LINE is the line string.  LNUM is the line number.
*
*   There is no requirement that lines in a snippet be sequential in the source
*   file, but they will be executed sequentially.  Hitting the end of a snippet
*   is handled the same as hitting the end of the current source file.
*
*   The saved line will be the input line with control characters replaced with
*   spaces, and trailing spaces stripped.
}
procedure escr_infile_add_line (       {add line to source file snippet}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     infile: fline_coll_t;        {snippet to add the line to}
  in      line: univ string_var_arg_t; {the source line}
  in      lnum: sys_int_machine_t);    {source line number within its file}
  val_param;

var
  line_p: fline_line_p_t;              {pointer to the new source line descriptor}
  ii: sys_int_machine_t;               {scratch integer}
  len: sys_int_machine_t;              {unpadded length of input line}

begin
  len := line.len;                     {init unpadded length of the input line}
  while len > 0 do begin
    if line.str[len] <> ' ' then exit; {at last non-blank ?}
    len := len - 1;                    {no, record line is one character shorter}
    end;

  util_mem_grab (                      {alloc mem for the input file descriptor}
    sizeof(line_p^), e.mem_p^, false, line_p);
  sys_mem_error (line_p, '', '', nil, 0);

  line_p^.next_p := nil;               {init this line to being at end of list}
  line_p^.file_p := addr(infile);      {point back to snippet this line is in}
  line_p^.lnum := lnum;                {save line number within file}
  string_alloc (len, e.mem_p^, false, line_p^.str_p); {alloc mem for line string}
  sys_mem_error (line_p^.str_p, '', '', nil, 0);

  for ii := 1 to len do begin          {copy the characters into memory}
    if ord(line.str[ii]) < 32
      then begin                       {control character}
        string_append1 (line_p^.str_p^, ' '); {replace with blank}
        end
      else begin                       {normal character}
        string_append1 (line_p^.str_p^, line.str[ii]);
        end
      ;
    end;                               {back to copy next character}

  if infile.llast_p = nil
    then begin                         {first line of snippet}
      infile.lfirst_p := line_p;
      end
    else begin                         {adding to existing list of lines}
      infile.llast_p^.next_p := line_p;
      end
    ;
  infile.llast_p := line_p;            {add this line to end of list}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_ADD_LINES (E, INFILE, CONN, STAT)
*
*   All the lines of a file from its current position to its end will be added
*   to the source file snippet INFILE.  CONN is the connection to the source
*   file, which must be open for reading text lines.
*
*   The file position will be left at the end of the file.
}
procedure escr_infile_add_lines (      {add lines to source file snippet}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     infile: fline_coll_t;        {snippet to add the lines to}
  var     conn: file_conn_t;           {existing connection to text file}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  buf: string_var8192_t;               {one line input buffer}

begin
  buf.max := size_char(buf.str);       {init local var strings}

  while true do begin                  {back here for each new input line}
    file_read_text (conn, buf, stat);  {read new input line}
    if file_eof(stat) then return;     {done reading this file ?}
    if sys_error(stat) then return;    {hard error ?}
    escr_infile_add_line (             {add this line to end of list for this file}
      e, infile, buf, conn.lnum);
    end;                               {back to do next input file line}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_OPEN (E, FNAM, SUFF, INFILE_P, STAT)
*
*   Return the pointer to the input file descriptor in INFILE_P for the file
*   indicated by FNAM.  If the file was previously read, then INFILE_P is
*   returned pointing to the data already in memory.  If not, the file is
*   read into memory and INFILE_P is returned pointing to the newly created
*   input file descriptor.  Each unique input file is only read once and
*   stored in memory.
*
*   SUFF is the list of suffixes allowed on the file name.  These are separated
*   by blanks.  The file must end in one of these suffixes.  When SUFF is blank,
*   then the file name is FNAM exactly.
}
procedure escr_infile_open (           {find file data or read it into memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {file name}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     infile_p: fline_coll_p_t;    {returned pointer to input file descriptor}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  conn: file_conn_t;                   {connection to the input file}

begin
  file_open_read_text (fnam, suff, conn, stat); {open the new input file}
  if sys_error(stat) then return;

  escr_infile_find (e, conn.tnam, infile_p); {look for file already in memory}
  if infile_p <> nil then begin        {this file has already been read in ?}
    file_close (conn);                 {close the file}
    return;                            {return pointing to existing descriptor}
    end;
{
*   The requested file has not previously been read and saved in memory.
}
  escr_infile_new (e, conn.tnam, infile_p); {create new descriptor}
  escr_infile_add_lines (e, infile_p^, conn, stat); {read file, save lines in mem}
  file_close (conn);                   {close the file}

  if not sys_error(stat) then begin    {no error ?}
    infile_p^.next_p := e.files_p;     {add this file to the list}
    e.files_p := infile_p;
    end;
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
  pos_p: fline_hier_p_t;               {pointer to input position state}

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
  pos_p: fline_hier_p_t;               {pointer to input position state}
  line_p: fline_line_p_t;              {pointer to input line info}

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
*   Skip to the next input line.  This is the same as reading and ignoring the
*   next input line.  It is a error if currently at the input stream end of the
*   current execution block.
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
