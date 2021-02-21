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
define escr_excl_check;
define escr_quotesyn_clear;
define escr_quotesyn_add;
define escr_quote_start;
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
  escr_syrlist_clear (e, e.commdat_p); {deallocate the list}
  e.commdeol_p := nil;                 {indicate no end of line comments}
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

var
  syent_p: escr_syrlist_p_t;           {pointer to list entry}
  syent_pp: ^escr_syrlist_p_t;         {where to write forward list link}

begin
  if st.len <= 0 then begin            {no starting string ?}
    sys_stat_set (escr_subsys_k, escr_err_nosystart_k, stat);
    return;
    end;

  if en.len > 0
    then begin                         {not an end of line comment}
      escr_syrlist_add (e, e.commdat_p, stat); {add to data file comments list}
      if sys_error(stat) then return;
      syent_p := e.commdat_p;          {set pointer to the new list entry}
      end
    else begin                         {this is an end of line comment}
      escr_syrlist_add (e, e.commdeol_p, stat); {add to end of line comm list}
      if sys_error(stat) then return;
      syent_p := e.commdat_p;          {init to first dat file comments list entry}
      syent_pp := addr(e.commdat_p);
      while syent_p <> nil do begin    {scan the existing list}
        if syent_p^.range.en.len <= 0 then exit; {found first end of line comment ?}
        syent_pp := addr(syent_p^.next_p); {update where to write forward link}
        syent_p := syent_p^.next_p;    {to next list entry}
        end;                           {back to check this new list entry}
      syent_p := e.commdeol_p;         {set pointer to the new list entry}
      syent_pp^ := syent_p;            {forward link last non-EOL comment to new entry}
      end
    ;

  string_copy (st, syent_p^.range.st); {save starting characters}
  string_copy (en, syent_p^.range.en); {save ending characters}
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
*   Function ESCR_EXCL_CHECK (E, STRI, P, EXCL_P, STRO, STAT)
*
*   Check for a syntax exclusion starting at index P in the input string STRI.
*   EXCL_P points to a syntax ranges list.  Each range will be interepreted as
*   one exclusion.  All exclusions in the list are checked, but only the first
*   found is acted upon.  EXCL_P may be NIL, in which case there are no
*   exclusions to check against, so the function always returns with no
*   exclusion found.
*
*   When no syntax exclusion is found, not state is changed and the function
*   returns FALSE.
*
*   When a syntax exclusion is found, the following is done:
*
*     - P is advanced to the first character after the syntax exclusion.
*
*     - The exclusion characters are appended to the string pointed to by
*       STRO_P.  STRO_P may be NIL, in which case no attempt will be made to
*       copy the exclusion characters.
*
*     - The function returns TRUE.
}
function escr_excl_check (             {check for syntax exclusion at char}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stri: univ string_var_arg_t; {input string}
  in out  p: string_index_t;           {index to check for syntax exclusion at}
  in      excl_p: escr_syrlist_p_t;    {list of exclusions to check, may be NIL}
  in      stro_p: univ string_var_p_t; {points to output string, may be NIL}
  out     stat: sys_err_t)             {completion status, only err if excl found}
  :boolean;                            {excl found, P changed, excl appended to STRO}
  val_param;

var
  cleft: sys_int_machine_t;            {number of characters left on line including curr}
  syr_p: escr_syrlist_p_t;             {points to current syntax ranges list entry}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  exstart: sys_int_machine_t;          {character index of exclusion start}

label
  next_excls, excl_found, not_excle;

begin
  sys_error_none (stat);               {init to no error}
  escr_excl_check := false;            {init to no exclusion found}

  cleft := stri.len - p + 1;           {number of characters left in input string}
  if cleft <= 0 then return;           {input string already exhausted ?}
{
*   Scan the list of syntax exclusions looking for one that starts here.
}
  syr_p := excl_p;                     {init to first entry in exclusions list}
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
  escr_excl_check := true;             {indicate exclusion found}

  if syr_p^.range.en.len = 0 then begin {exclusion goes all the way to end of line ?}
    if stro_p <> nil then begin        {output string was provided ?}
      for ii := p to stri.len do begin {scan remainder of input string}
        string_append1 (stro_p^, stri.str[ii]); {copy this character to output string}
        end;
      end;
    p := stri.len + 1;                 {indicate input string exhausted}
    return;
    end;

  if stro_p <> nil then begin
    string_append (stro_p^, syr_p^.range.st); {copy exclusion start to output string}
    end;
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
    if stro_p <> nil then begin
      string_append (stro_p^, syr_p^.range.en); {copy exclusion end to output string}
      end;
    p := p + syr_p^.range.en.len;      {skip over exclusion end pattern}
    return;

not_excle:                             {exclusion doesn't end here}
    if stro_p <> nil then begin
      string_append1 (stro_p^, stri.str[p]); {copy this excluded char to output string}
      end;
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
{
********************************************************************************
*
*   Subroutine ESCR_QUOTESYN_CLEAR (E)
*
*   Clear any existing quoted string start/end syntaxes.  This makes recognizing
*   quoted strings impossible.
*
*   Defaults are set when the library use state is created:
*
*     Start: ("), End (").  Quote character starts and ends a string.
*
*     Start: ('), End (').  Apostrophy character starts and ends a string.
*
*   Applications can add to these without needing to clear all definitions.
*   However, if any of the default definitions are not desired, then the
*   application must clear all, and add only what is desired.
}
procedure escr_quotesyn_clear (        {clear all quoted string start/end syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param;

var
  qsyn_p: escr_quotesyn_p_t;           {points to curr quoted string syntax description}
  next_p: escr_quotesyn_p_t;           {points to next list entry}

begin
  qsyn_p := e.quotesyn_p;              {init to first list entry}
  while qsyn_p <> nil do begin         {back here each new list entry}
    next_p := qsyn_p^.next_p;          {save pointer to next list entry}
    util_mem_ungrab (qsyn_p, e.mem_p^); {deallocate this list entry}
    qsyn_p := next_p;                  {on to next list entry}
    end;                               {back to process this new list entry}

  e.quotesyn_p := nil;                 {indicate the list is empty}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_QUOTESYN_ADD (E, QST, QEN)
*
*   Add one quoted string syntax to the list.  QST is the quoted string start
*   character and QEN the corresponding quoted string end character.  Any text
*   after QST up to but not including the next QEN will be interpreted as a
*   literal string.
*
*   Attempts to add duplicate entries are ignored.
}
procedure escr_quotesyn_add (          {add one quoted string start/end syntax to list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      qst: char;                   {quoated string start character}
  in      qen: char);                  {corresponding quoted string end character}
  val_param;

var
  qsyn_p: escr_quotesyn_p_t;           {points to quotes string syntax list entry}

begin
{
*   Check for existing entry for the requested syntax.  If found, there is
*   nothing to do.  Duplicate entries don't change string interpretation, but
*   do cause wasted cycles looking for strings.
}
  qsyn_p := e.quotesyn_p;              {init to first existing list entry}
  while qsyn_p <> nil do begin         {back here each new existing list entry}
    if                                 {requested syntax is already in the list ?}
        (qsyn_p^.st = qst) and         {start character matches ?}
        (qsyn_p^.en = qen)             {end character matches ?}
        then begin
      return;                          {already in list, nothing to do}
      end;
    qsyn_p := qsyn_p^.next_p;          {to next list entry}
    end;                               {back to check this next entry for duplicate}
{
*   An entry with the requested syntax doesn't exist.  Create it.
}
  util_mem_grab (                      {allocate memory for new list entry}
    sizeof(qsyn_p^), e.mem_p^, true, qsyn_p);
  qsyn_p^.st := qst;                   {fill in this list entry}
  qsyn_p^.en := qen;

  qsyn_p^.next_p := e.quotesyn_p;      {link new entry to start of list}
  e.quotesyn_p := qsyn_p;
  end;
{
********************************************************************************
*
*   Function ESCR_QUOTE_START (E, STCHAR, QSYN_P)
*
*   Determine whether the character STCHAR starts a quoted string.
*
*   If STCHAR starts a quoted string, the function returns TRUE, and QSYN_P will
*   point to the applicable quoted string syntax description.  This can be used
*   to find the quoted string end character.
*
*   If STCHAR does not start a quoted string, then the function returns FALSE
*   and QSYN_P is set to NIL.
}
function escr_quote_start (            {check for start of quoted string character}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stchar: char;                {the character to check}
  out     qsyn_p: escr_quotesyn_p_t)   {points to applicable syntax, NIL not quoted}
  :boolean;                            {character starts a quoted string}
  val_param;

begin
  qsyn_p := e.quotesyn_p;              {init to first quoted string syntax list entry}
  while qsyn_p <> nil do begin         {scan the list}
    if stchar = qsyn_p^.st then begin  {char matches string start ?}
      escr_quote_start := true;
      return;
      end;
    qsyn_p := qsyn_p^.next_p;          {advance to next list entry}
    end;                               {back to check char against new list entry}

  escr_quote_start := false;           {char didn't match any string start chars}
  end;
