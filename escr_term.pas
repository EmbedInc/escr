{   Routines for handling terms in expressions or function parameters.
}
module escr_term;
define escr_term_raw;
define escr_term_rawc;
define escr_term_parse;
define escr_term_val;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Function ESCR_TERM_RAW (E, FSTR, P, TERM)
*
*   Return the raw characters of the next term in FSTR exactly as found.  P is
*   the starting parse index, and will be udpated to after the term.  The
*   function returns TRUE to indicate returning with a term normally.
*
*   Tokens are delimited by one or more spaces.  These spaces are not parts of
*   any token.
*
*   No interpretation of the token is performed, except that spaces within
*   quoted strings do not represet breaks between tokens.
*
*   When the end of the input string is encountered before a term is found, the
*   function returns FALSE with STAT indicating no error.  On error, STAT is set
*   to indicate the error, and the function returns FALSE.
}
function escr_term_raw (               {get chars of next token from input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
  in out  term: univ string_var_arg_t) {returned raw term characters}
  :boolean;                            {TRUE if term was available and no error}
  val_param;

var
  quote: boolean;                      {in a quoted string}
  qen: char;                           {quoted string end character}
  c: char;                             {current character}
  qsyn_p: escr_quotesyn_p_t;           {pointer to quoted string syntax list entry}

begin
  escr_term_raw := false;              {init to no token available}
  term.len := 0;

  while p <= fstr.len do begin         {skip over blanks at current position}
    if fstr.str[p] <> ' ' then exit;   {at a non-blank ?}
    p := p + 1;                        {skip over this blank}
    end;
  if p > fstr.len then return;         {no token before end of input string ?}
  escr_term_raw := true;               {will be returning with a token}

  quote := false;                      {init to not within a quoted string}
  while p <= fstr.len do begin         {scan the remainder of the input string}
    c := fstr.str[p];                  {fetch this character}
    if quote
      then begin                       {in a quoted string}
        string_append1 (term, c);      {copy this char to returned token}
        if c = qen then quote := false; {hit quoted string end ?}
        end
      else begin                       {not in a quoted string}
        if c = ' ' then begin          {unquoted blank ends token}
          p := p + 1;                  {skip over this blank}
          exit;
          end;
        string_append1 (term, c);      {copy this char to returned token}
        qsyn_p := e.quotesyn_p;        {init to first quoted string syntax list entry}
        while qsyn_p <> nil do begin   {scan the list of quoted string syntaxes}
          if c = qsyn_p^.st then begin {this character starts a quoted string ?}
            quote := true;             {indicate now in a quoted string}
            qen := qsyn_p^.en;         {set quoted string end character}
            exit;                      {stop looking for quoted string start}
            end;
          qsyn_p := qsyn_p^.next_p;    {to next quoted string syntax list entry}
          end;                         {back to check char against new quote start}
        end
      ;
    p := p + 1;                        {make index of the next char}
    end;                               {back to process this new char}
  end;
{
********************************************************************************
*
*   Function ESCR_TERM_RAWC (E, FSTR, P, TERM)
*
*   Like ESCR_TERM_RAWC except that tokens are delimited by single commas that
*   may be surrounded by any number of spaces.
}
function escr_term_rawc (              {get chars of next token from input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
  in out  term: univ string_var_arg_t) {returned raw term characters}
  :boolean;                            {TRUE if term was available and no error}
  val_param;

var
  quote: boolean;                      {in a quoted string}
  qen: char;                           {quoted string end character}
  c: char;                             {current character}
  qsyn_p: escr_quotesyn_p_t;           {pointer to quoted string syntax list entry}
  nspace: sys_int_machine_t;           {number of spaces read but not added to TERM}

label
  nextin;

begin
  escr_term_rawc := false;             {init to no token available}
  term.len := 0;

  while p <= fstr.len do begin         {skip over blanks at current position}
    if fstr.str[p] <> ' ' then exit;   {at a non-blank ?}
    p := p + 1;                        {skip over this blank}
    end;
  if p > fstr.len then return;         {no token before end of input string ?}
  escr_term_rawc := true;              {will be returning with a token}

  quote := false;                      {init to not within a quoted string}
  nspace := 0;                         {init to no pending trailing spaces}

  while p <= fstr.len do begin         {scan the remainder of the input string}
    c := fstr.str[p];                  {fetch this character}
    if quote
      then begin                       {in a quoted string}
        string_append1 (term, c);      {copy this char to returned token}
        if c = qen then quote := false; {hit quoted string end ?}
        end
      else begin                       {not in a quoted string}
        if c = ',' then begin          {unquoted comma ends token}
          p := p + 1;                  {start after the comma next time}
          exit;
          end;
        if c = ' ' then begin          {blank, could be padding before comma}
          nspace := nspace + 1;        {count one more trailing blank not in TERM}
          goto nextin;
          end;
        while nspace > 0 do begin      {there are unreported blanks ?}
          string_append1 (term, ' ');  {add one blank}
          nspace := nspace - 1;        {count one less unreported blank left}
          end;                         {back to add next ureported blank}
        string_append1 (term, c);      {copy this char to returned token}
        qsyn_p := e.quotesyn_p;        {init to first quoted string syntax list entry}
        while qsyn_p <> nil do begin   {scan the list of quoted string syntaxes}
          if c = qsyn_p^.st then begin {this character starts a quoted string ?}
            quote := true;             {indicate now in a quoted string}
            qen := qsyn_p^.en;         {set quoted string end character}
            exit;                      {stop looking for quoted string start}
            end;
          qsyn_p := qsyn_p^.next_p;    {to next quoted string syntax list entry}
          end;                         {back to check char against new quote start}
        end                            {end of was not in quoted string}
      ;
nextin:                                {done with this input char, on to next}
    p := p + 1;                        {advance input string index to next char}
    end;                               {back to process this new char}
  end;
{
********************************************************************************
*
*   Function ESCR_TERM_PARSE (E, FSTR, P, TERM, QUOTED, STAT)
*
*   Parse the next term from the input string FSTR.  The characters of the term
*   are returned in TERM, and QUOTED is set iff these were in a quotes string.
*   The enclosing quotes, if any, are not returned in TERM.  Also, escaped
*   literal quotes are returned interpreted in TERM, not exactly as found in
*   FSTR.
*
*   Examples using the default ESCR quoted string rules:
*
*     Characters in FSTR   TERM                QUOTED
*     ------------------   -----------------   ------
*     Abcd                 Abcd                false
*     "Abdc"               Abcd                true
*     "Don't do that"      Don't do that       true
*     'Don''t do that'     Don't do that       true
*
*   P is the FSTR parse index, and will be updated so that the next call to this
*   routine returns the next term.  The function returns TRUE when returning
*   with a term, and FALSE if no term was found before the end of the input
*   string.
*
*   The function returns FALSE when STAT is returned indicating error.
}
function escr_term_parse (             {parse next term from input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
  in out  term: univ string_var_arg_t; {returned raw term characters, unquoted}
  out     quoted: boolean;             {the characters were quoted}
  out     stat: sys_err_t)             {completion status, no error on func TRUE}
  :boolean;                            {TRUE if term was available and no error}
  val_param;

var
  c: char;                             {scratch character}
  qsyn_p: escr_quotesyn_p_t;           {pointer matching quoted string syntax}

label
  qstring;

begin
  sys_error_none (stat);               {init to no error encountered}
  escr_term_parse := false;            {init to no term found}
  term.len := 0;                       {init returned string to empty}
  quoted := false;                     {init to term is not a quoted string}

  if p > fstr.len then return;         {nothing left to parse from input string ?}
  while fstr.str[p] = ' ' do begin     {skip over blanks}
    p := p + 1;                        {advance to next input string char}
    if p > fstr.len then return;       {hit end of input string ?}
    end;                               {back to check this next input string char}

  c := fstr.str[p];                    {get the first token character}
  if escr_quote_start (e, c, qsyn_p)   {first character is start of quoted string ?}
    then goto qstring;
{
*   Not a quoted string.  P is the index of the first character, and C is that
*   character.
}
  while true do begin
    string_append1 (term, c);          {add this char to returned string}
    p := p + 1;                        {advance to next source character}
    if p > fstr.len then exit;         {hit end of input string ?}
    c := fstr.str[p];                  {fetch this new source character}
    if c = ' ' then begin              {hit blank ending the term ?}
      p := p + 1;                      {start after the blank next time}
      exit;
      end;
    end;                               {back to handle this new input string char}

  escr_term_parse := true;             {returning with a term}
  return;
{
*   Quoted string.  P is the index of the quote start character, and QSYN_P is
*   pointing to the quoted string syntax description.
}
qstring:
  quoted := true;                      {indicate returning quoted string contents}

  while true do begin
    p := p + 1;                        {skip over the quote start character}
    if p > fstr.len then begin         {hit end of input string ?}
      sys_stat_set (string_subsys_k, string_stat_no_endquote_k, stat);
      return;
      end;
    c := fstr.str[p];                  {fetch this new source character}
    if c = qsyn_p^.en then begin       {ending quoted string character ?}
      p := p + 1;                      {go to next character}
      if p > fstr.len then exit;       {end of string right after closing quote ?}
      c := fstr.str[p];                {fetch next char after ending quote}
      if c = qsyn_p^.en then begin     {two consecutive end quotes ?}
        string_append1 (term, c);      {interpret as single end quote char and continue}
        next;
        end;
      if c = ' ' then begin            {blank after closing quote ?}
        p := p + 1;                    {start after the blank next time}
        exit;
        end;
      sys_stat_set (string_subsys_k, string_stat_bad_quote_k, stat);
      return;
      end;                             {end of char is quote end}
    string_append1 (term, c);          {copy this char to the output string}
    end;                               {back to do next input char}

  escr_term_parse := true;             {returning with a term}
  end;
{
********************************************************************************
*
*   Function ESCR_TERM_VAL (E, FSTR, P, VAL, STAT)
*
*   Get the next token in FSTR and interpret it as a term or inline function
*   argument.  P is the FSTR parse index and VAL is the returned value of the
*   term.  The function returns TRUE when returning with a value, and FALSE when
*   no term was found before the end of the input string.  If the term could not
*   be interpreted as any value, then the function returns FALSE and STAT
*   indicates error.  STAT is not set to error when there is no term.
*
*   VAL is assumed to be completely uninitialized and the full VAL_T structure
*   as defined in the include file.
}
function escr_term_val (               {get value of next term in input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index}
  out     val: escr_val_t;             {returned value of the term}
  out     stat: sys_err_t)             {completion status, no error on func TRUE}
  :boolean;                            {TRUE if term was available}
  val_param;

var
  pick: sys_int_machine_t;             {number of token picked from list}
  sym_p: escr_sym_p_t;                 {pointer to constant or variable descriptor}
  tk: string_var8192_t;                {token parsed from input string}
  tku: string_var32_t;                 {scratch upper case token}
  quoted: boolean;                     {term characters were quoted}

label
  not_sym;

begin
  tk.max := size_char(tk.str);         {init local var string}
  tku.max := size_char(tku.str);
  escr_term_val := false;              {init to no term found}

  if not escr_term_parse (e, fstr, p, tk, quoted, stat)
    then return;
  escr_term_val := true;               {default is now returning with a value}
{
*   Check for token is text string.
}
  if quoted then begin
    val.dtype := escr_dtype_str_k;     {pass back value as text string}
    val.str.max := size_char(val.str.str);
    string_copy (tk, val.str);         {return the string}
    return;
    end;
{
*   Check for integer.
}
  string_t_int_max (tk, val.int, stat); {try converting to integer}
  if not sys_error(stat) then begin    {integer conversion succeeded ?}
    val.dtype := escr_dtype_int_k;
    return;
    end;
{
*   Check for floating point.
}
  string_t_fpmax (tk, val.fp, [], stat); {try converting to floating point}
  if not sys_error(stat) then begin    {floating point conversion succeeded ?}
    val.dtype := escr_dtype_fp_k;
    return;
    end;
{
*   Check for time.
}
  if escr_str_to_time (e, tk, val.time) then begin
    val.dtype := escr_dtype_time_k;
    sys_error_none (stat);
    return;
    end;
{
*   Check for boolean.
}
  string_copy (tk, tku);               {convert to upper case for keyword matching}
  string_upcase (tku);
  string_tkpick80 (tku, 'FALSE TRUE', pick);
  if pick > 0 then begin               {token matched one of the keywords ?}
    val.bool := pick = 2;
    val.dtype := escr_dtype_bool_k;
    sys_error_none (stat);
    return;
    end;
{
*   Check for symbol reference.
}
  escr_sym_find (e, tk, sym_p);        {try to look up the symbol}
  if sym_p = nil then goto not_sym;    {no such symbol ?}
  case sym_p^.stype of                 {what kind of symbol is it ?}
escr_sym_var_k: begin                  {symbol is a variable}
      escr_vcon_val (e, sym_p^.var_val, val); {return the variable's value}
      sys_error_none (stat);
      return;
      end;
escr_sym_const_k: begin                {symbol is a constant}
      escr_vcon_val (e, sym_p^.const_val, val); {return the constant's value}
      sys_error_none (stat);
      return;
      end;
otherwise                              {this symbol can't be used as a term}
    sys_stat_set (escr_subsys_k, escr_err_notval_k, stat); {symbol has no value}
    sys_stat_parm_vstr (tk, stat);     {symbol name}
    escr_term_val := false;
    return;
    end;

not_sym:                               {skip to here on not a symbol reference}
{
*   The token is not a recognizable term.
}
  sys_stat_set (escr_subsys_k, escr_err_termbad_k, stat); {not a valid term}
  sys_stat_parm_vstr (tk, stat);       {term string}
  escr_term_val := false;
  end;
