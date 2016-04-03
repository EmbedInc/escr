{   Syntax triggers.  These identify commands, comments, function, etc.
}
module escr_syt;
define escr_syrlist_clear;
define escr_syrlist_add;
define escr_commscr_clear;
define escr_commscr_add;
define escr_commdat_clear;
define escr_commdat_add;
define escr_syexcl_clear;
define escr_syexcl_add;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_SYRLIST_CLEAR (E, SYL_P)
*
*   Clear a syntax ranges list to empty.  SYL_P is the pointer to the first list
*   entry.  It will be returned NIL, and all list entries, if any, deallocated.
*   Nothing is done if SYL_P is already NIL.
}
procedure escr_syrlist_clear (         {clear syntax ranges list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  syl_p: escr_syrlist_p_t);    {pointer to list to clear, returned NIL}
  val_param;

var
  s_p: escr_syrlist_p_t;               {pointer to current list entry}

begin
  while syl_p <> nil do begin          {once for each list entry}
    s_p := syl_p;                      {save pointer to this list entry}
    syl_p := s_p^.next_p;              {remove this entry from the list}
    util_mem_ungrab (s_p, e.mem_p^);   {dealocate the removed list entry}
    end;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYRLIST_ADD (E, SYL_P, STAT)
*
*   Add a new entry to the syntax ranges list pointed to by SYL_P.  The new
*   entry will be added to the start of the list, so SYL_P will be returned
*   pointing to the new entry.  The new entry is initialized to both the start
*   and end strings empty.
}
procedure escr_syrlist_add (           {add new blank entry to syntax ranges list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  syl_p: escr_syrlist_p_t;     {pointer to list, will point to new entry}
  out     stat: sys_err_t);            {completion status}
  val_param;

var
  s_p: escr_syrlist_p_t;               {pointer to current list entry}

begin
  sys_error_none (stat);               {init to no error encountered}

  util_mem_grab (                      {allocate new list entry}
    sizeof(s_p^), e.mem_p^, true, s_p);
  if s_p = nil then begin
    sys_stat_set (escr_subsys_k, escr_err_nomem_k, stat);
    sys_stat_parm_int (sizeof(s_p^), stat);
    return;
    end;

  s_p^.range.st.max := size_char(s_p^.range.st.str); {init new entry to blank}
  s_p^.range.st.len := 0;
  s_p^.range.en.max := size_char(s_p^.range.en.str);
  s_p^.range.en.len := 0;
  s_p^.range.eol := true;

  s_p^.next_p := syl_p;                {link new entry to start of list}
  syl_p := s_p;
  end;
{
********************************************************************************
*
*   Subroutine ESCR_COMMSCR_CLEAR (E)
*
*   Clear the list of syntax ranges for identifying script comments.
}
procedure escr_commscr_clear (         {clear script comment syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  escr_syrlist_clear (e, e.commscr_p);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_COMMSCR_ADD (E, ST, EN, STAT)
*
*   Add one syntax range for identifying script comments.  ST is the string that
*   starts a comment and EN ends the comment.  If EN is the empty string, then
*   this type of comment is ended at the end of the line.  ST must not be empty.
*
*   Script comments are inherent to the scripting engine.  They are stripped
*   from the input before script processing is applied.  These comments are
*   always relevent, whether in script mode or preprocessor mode.  These
*   are considered part of the script source, not the data file be preprocessed,
*   so are not copied to the output file in preprocessor mode.
}
procedure escr_commscr_add (           {add identifying syntax for script comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start comment}
  in      en: univ string_var_arg_t;   {characters that end comment, blank for EOL}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  if st.len <= 0 then begin            {no starting string ?}
    sys_stat_set (escr_subsys_k, escr_err_nosystart_k, stat);
    return;
    end;

  escr_syrlist_add (e, e.commscr_p, stat); {add new entry to list}
  if sys_error(stat) then return;

  string_copy (st, e.commscr_p^.range.st); {save starting characters}
  string_copy (en, e.commscr_p^.range.en); {save ending characters}
  e.commscr_p^.range.eol := (en.len <= 0); {end of line ends comment ?}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_COMMDAT_CLEAR (E)
*
*   Clear the list of syntax ranges for identifying data comments.
}
procedure escr_commdat_clear (         {clear script comment syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  escr_syrlist_clear (e, e.commdat_p);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_COMMDAT_ADD (E, ST, EN, STAT)
*
*   Add one syntax range for identifying data comments.  ST is the string that
*   starts a comment and EN ends the comment.  If EN is the empty string, then
*   this type of comment is ended at the end of the line.  ST must not be empty.
*
*   Data file comments are only relevant in preprocessor mode (not script mode).
*   They are considered part of the data file, so are copied to the output file.
}
procedure escr_commdat_add (           {add identifying syntax for data comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start comment}
  in      en: univ string_var_arg_t;   {characters that end comment, blank for EOL}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  if st.len <= 0 then begin            {no starting string ?}
    sys_stat_set (escr_subsys_k, escr_err_nosystart_k, stat);
    return;
    end;

  escr_syrlist_add (e, e.commdat_p, stat); {add new entry to list}
  if sys_error(stat) then return;

  string_copy (st, e.commdat_p^.range.st); {save starting characters}
  string_copy (en, e.commdat_p^.range.en); {save ending characters}
  e.commdat_p^.range.eol := (en.len <= 0); {end of line ends comment ?}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYEXCL_CLEAR (E)
*
*   Clear the list of syntax exclusions.
}
procedure escr_syexcl_clear (          {clear script comment syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

begin
  escr_syrlist_clear (e, e.syexcl_p);
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYEXCL_ADD (E, ST, EN, EOL, STAT)
*
*   Add one syntax range for identifying characters excluded from other
*   syntaxes.  Within a exclusion, all other syntaxes are ignored except the end
*   of that exclusion.  A quoted string is a common example.
*
*   ST is the characters that start the exclusion, and EN the characters that
*   end it.  When EOL is TRUE, then the end of line also ends the exclusion.
*   When EN is the empty string, the end of line will end the exclusion
*   regardless of EOL.  ST must not be empty.
*
}
procedure escr_syexcl_add (            {add identifying syntax for data comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start comment}
  in      en: univ string_var_arg_t;   {characters that end comment, blank for EOL}
  in      eol: boolean;                {end of line ends exclusion}
  out     stat: sys_err_t);            {completion status}
  val_param;

begin
  if st.len <= 0 then begin            {no starting string ?}
    sys_stat_set (escr_subsys_k, escr_err_nosystart_k, stat);
    return;
    end;

  escr_syrlist_add (e, e.syexcl_p, stat); {add new entry to list}
  if sys_error(stat) then return;

  string_copy (st, e.syexcl_p^.range.st); {save starting characters}
  string_copy (en, e.syexcl_p^.range.en); {save ending characters}
  e.syexcl_p^.range.eol := eol or (en.len <= 0); {end of line ends exclusion ?}
  end;
