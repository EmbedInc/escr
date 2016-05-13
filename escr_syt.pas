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
define escr_excl_nextchar;
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
    syl_p := s_p^.next_p;              {point to next list entry}
    util_mem_ungrab (s_p, e.mem_p^);   {dealocate this list entry}
    end;                               {back to handle next list entry}
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
*
*   The list may be empty on entry (SYL_P = NIL).
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
*   are considered part of the script source, not the data file being
*   preprocessed, so are not copied to the output file in preprocessor mode.
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
  end;
{
********************************************************************************
*
*   Subroutine ESCR_SYEXCL_CLEAR (E)
*
*   Clear the list of syntax exclusions.
}
procedure escr_syexcl_clear (          {clear all syntax exclusions}
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
*   end it.  When EN is the empty string, the end of line ends the syntax
*   exclusion.  ST must not be empty.
*
}
procedure escr_syexcl_add (            {add syntax exclusion}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start exclusion}
  in      en: univ string_var_arg_t;   {characters that end exclusion, blank for EOL}
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
  end;
{
********************************************************************************
*
*   Function ESCR_EXCL_NEXTCHAR (E, STRI, P, STRO, STAT)
*
*   Indicate the next input string character, taking any syntax exclusions into
*   account.
*
*   STRI is the input string, and P its parse index.  STRO is the output string.
*   If the character at P does not start a syntax exclusion, then nothing is
*   done.  If a syntax exclusion or exclusions start at P, then the exclusions
*   are appended to the output string and P advanced to after all the
*   consecutive exclusions.  P is left one past the end of the input line if a
*   exclusion was skipped that ended at the end of the line.
*
*   The function returns TRUE iff P is altered, meaning one or more exclusions
*   were skipped over and appended to STRO.  Note that in this case the caller
*   should check P to determine whether the input line was exhausted.  If the
*   input line ended within a exclusion and end of input line is not the valid
*   end of the exclusion, then STAT is set to indicate the error.  STAT is
*   returned indicating no error in all other cases.  There is therefore no need
*   to check STAT when the function returns FALSE.
}
function escr_excl_nextchar (          {get next char from string, skip over exclusions}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stri: univ string_var_arg_t; {input string}
  in out  p: string_index_t;           {input string parse index}
  in out  stro: univ string_var_arg_t; {output string}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {P changed (exclusion processed)}
  val_param;

var
  cleft: sys_int_machine_t;            {number of characters left on line including curr}
  syr_p: escr_syrlist_p_t;             {points to current syntax ranges list entry}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  exstart: sys_int_machine_t;          {character index of exclusion start}

label
  retry, next_excls, excl_found, not_excle;

begin
  sys_error_none (stat);               {init to no error}
  escr_excl_nextchar := false;         {init to no exclusion found}

retry:                                 {back here after skipping over exclusion}
  cleft := stri.len - p + 1;           {number of characters left in input string}
  if cleft <= 0 then return;           {input string already exhausted ?}
{
*   Scan the list of syntax exclusions looking for one that starts here.
}
  syr_p := e.syexcl_p;                 {init to first entry in exclusions list}
  while syr_p <> nil do begin          {back here each new syntax exclusion to check}
    if syr_p^.range.st.len > cleft     {this exclusion doesn't fit here ?}
      then goto next_excls;
    for ii := 1 to syr_p^.range.st.len do begin {compare to exclusion start characters}
      if stri.str[p + ii - 1] <> syr_p^.range.st.str[ii] {mismatch ?}
        then goto next_excls;
      end;                             {back to check next exclusion character}
    goto excl_found;                   {this exclusion starts here, go handle it}
next_excls:                            {this exclusion doesn't match, go on to next}
    syr_p := syr_p^.next_p;            {advance to the next exclusion in the list}
    end;                               {back to check this new exclusion}

  return;                              {no syntax exclusion found here}
{
*   The exclusion pointed to by SYR_P starts here.  P has not been changed, and
*   is therefore at the start of the exclusion.
*
*   Copy the exclusion characters to the output while looking for the end of the
*   exclusion.
}
excl_found:
  exstart := p;                        {save index of exclusion start}
  escr_excl_nextchar := true;          {indicate returning with changed P}

  if syr_p^.range.en.len = 0 then begin {exclusion goes all the way to end of line ?}
    for ii := p to stri.len do begin   {scan remainder of input string}
      string_append1 (stro, stri.str[ii]); {copy this character to output string}
      end;
    p := stri.len + 1;                 {indicate input string exhausted}
    return;
    end;

  string_append (stro, syr_p^.range.st); {copy exclusion start to output string}
  p := p + syr_p^.range.st.len;        {skip over exclusion start}

  while p <= stri.len do begin         {scan forwards looking for exclusion end}
    cleft := stri.len - p + 1;         {number of characters left in input string}
    if syr_p^.range.en.len > cleft then goto not_excle; {end pattern doesn't fit here ?}
    for ii := 1 to syr_p^.range.en.len do begin {once for each character in end pattern}
      if stri.str[p + ii - 1] <> syr_p^.range.en.str[ii] {mismatch ?}
        then goto not_excle;
      end;
    {
    *   The syntax exclusion end pattern starts at the current position.
    }
    string_append (stro, syr_p^.range.en); {copy exclusion end to output string}
    p := p + syr_p^.range.en.len;      {skip over exclusion end pattern}
    goto retry;                        {back to check right after this exclusion}

not_excle:                             {exclusion doesn't end here}
    string_append1 (stro, stri.str[p]); {copy this excluded char to output string}
    p := p + 1;                        {advance to next input string char}
    end;                               {back and check next input string char}
{
*   The end of the input string was encountered before the exclusion ended.
}
  sys_stat_set (escr_subsys_k, escr_err_exclnend_k, stat); {return with error}
  sys_stat_parm_vstr (syr_p^.range.st, stat);
  sys_stat_parm_vstr (syr_p^.range.en, stat);
  sys_stat_parm_int (exstart, stat);
  end;
