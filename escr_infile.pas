{   Routines for managing input files snippets.  This code is layered on the
*   FLINE library.  FLINE_P in the ESCR library use state points to the FLINE
*   library use state.
*
*   Input files are only read once, and stored in memory as a collection of
*   lines using the FLINE library facilities.  This in-memory data is re-used on
*   subsequent attempts to read the same file.
*
*   Collections of lines can also be named and referenced by a symbol in the SRC
*   symbol table.
*
*   The current position is maintained at the last line that was read.  To read
*   the next input line, the position must first be advanced.
}
module escr_infile;
define escr_infile_open;
define escr_infile_pop;
define escr_infile_getline;
define escr_in_line;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_OPEN (E, FNAM, SUFF, COLL_P, STAT)
*
*   Return COLL_P pointing to the lines of the file FNAM with suffix SUFF.  If
*   the file was previously read, then COLL_P is returned pointing to the data
*   already in memory.  If not, the file is read into memory and COLL_P is
*   returned pointing to the newly created list of lines.  Each unique input
*   file is only read once and stored in memory.
*
*   SUFF is the list of suffixes allowed on the file name.  These are separated
*   by blanks.  The file must end in one of these suffixes.  When SUFF is blank,
*   then the file name is FNAM exactly.
}
procedure escr_infile_open (           {find file data or read it into memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {file name}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     coll_p: fline_coll_p_t;      {returned pointer to file lines in memory}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  fline_file_get_suff (                {find or make stored lines of this file}
    e.fline_p^,                        {FLINE library use state}
    fnam, suff,                        {file name and optional suffixes}
    coll_p,                            {returned pointer to collection of lines}
    stat);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_POP (E)
*
*   Pop back one level of input file state within the current execution block.
*   If the top level of the block is popped, then its INSTK_P pointer is set to
*   NIL.  Nothing is done if this pointer is already NIL.
}
procedure escr_infile_pop (            {pop back one nested input file level}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  if e.exblock_p = nil then return;    {no current execution block ?}

  fline_block_pop (                    {pop one nesting level in this block}
    e.fline_p^, e.exblock_p^.instk_p);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INFILE_GETLINE (E, STR_P)
*
*   Get the next input stream source line by returning STR_P pointing to it.
*   STR_P will be returned NIL when the input stream has been exhausted.
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

begin
  if e.exblock_p = nil then begin
    writeln ('INTERNAL ERROR: No execution block defined in INFILE_GETLINE.');
    sys_bomb;
    end;

  fline_block_getnext_str (e.fline_p^, e.exblock_p^.instk_p, str_p);
  end;
{
********************************************************************************
*
*   Function ESCR_IN_LINE (E)
*
*   Returns the pointer to the current input line.
}
function escr_in_line (                {get pointer to current input line}
  in out  e: escr_t)                   {state for this use of the ESCR system}
  :fline_line_p_t;                     {pointer to the last-read input line}
  val_param;

var
  block_p: escr_exblock_p_t;           {pointer to execution block in hiearchy}
  line_p: fline_line_p_t;              {pointer to current input line}

begin
  block_p := e.exblock_p;
  while block_p^.instk_p = nil do begin {search upwards for first block with input defined}
    if block_p^.prev_p = nil then begin {at top level and still no input ?}
      writeln;
      writeln ('INTERNAL ERROR: No block with input in ESCR_IN_LINE.');
      escr_err_atline (e, '', '', nil, 0);
      end;
    block_p := block_p^.prev_p;        {go up to parent block}
    end;

  fline_hier_get_line (block_p^.instk_p^, line_p); {get pointer to the input line}
  escr_in_line := line_p;              {return pointer to input line}
  end;
