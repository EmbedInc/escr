{   Routines to process inline preprocessor functions.
}
module escr_inline;
define escr_inline_expand_line;
%include 'escr2.ins.pas';
{
*   Private routines used inside this module only.
}
procedure inline_expand_lrest (        {expand inline functions in rest of line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  in      linst: string_index_t;       {LIN index at which to start}
  out     lot: string_var8192_t);      {output string to append expansion to}
  val_param; internal; forward;

procedure inline_expand_func (         {expand rest of line starting at inline func}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input string containing inline function}
  in      linst: string_index_t;       {start index of func right after "["}
  in out  lot: string_var8192_t);      {output string to append expansion to}
  val_param; internal; forward;
{
****************************************************************************
*
*   Subroutine ESCR_INLINE_EXPAND_LINE (E, LIN, LOT)
*
*   Expand all the inline functions in LIN into LOT.
}
procedure escr_inline_expand_line (    {expand all inline functions of a line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  out     lot: string_var8192_t);      {output line, contains no inline functions}
  val_param;

begin
  lot.len := 0;                        {init output line to empty}
  inline_expand_lrest (                {expand input line into the output line}
    e,                                 {state for this use of ESCR system}
    lin,                               {input line to expand}
    1,                                 {input line index to start at}
    lot);                              {output string to append expansion to}
  end;
{
****************************************************************************
*
*   Subroutine INLINE_EXPAND_LREST (E, LIN, LINST, LOT)
*
*   Expand the rest of the input line LIN starting at LINST and append the
*   result to LOT.
}
procedure inline_expand_lrest (        {expand inline function in rest of line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  in      linst: string_index_t;       {LIN index at which to start}
  out     lot: string_var8192_t);      {output string to append expansion to}
  val_param; internal;

var
  p: string_index_t;                   {input line parse index}
  quote: char;                         {character surrounding the quoted string}
  c: char;                             {current input line character}
  inquote: boolean;                    {TRUE if inside a quoted string}
  comment: boolean;                    {in the end of line comment}

begin
  p := linst;                          {init input string parse index}
  inquote := false;                    {init to not within a quoted string}
  comment := false;                    {init to not within an end of line comment}

  while p <= lin.len do begin          {loop until input line exhausted}
    c := lin.str[p];                   {fetch this input character}
    case c of                          {check for special handling characters}

'''', '"': begin                       {start and end of quotes strings}
        if inquote
          then begin                   {currently in a quoted string}
            inquote := c <> quote;     {FALSE if this is closing quote}
            end
          else begin                   {not currently in a quoted string}
            inquote := true;           {now in a quoted string}
            quote := c;                {set close quote character}
            end
          ;
        end;

';':  begin                            {start of the end of line comment}
        comment := not inquote;        {this is comment start if not within a quote}
        end;

'[':  begin                            {start of inline function}
        if (not comment) and (not inquote) then begin {interpret as inline func ?}
          inline_expand_func (         {expand inline function and rest of line}
            e,                         {state for this use of ESCR system}
            lin,                       {input string}
            p + 1,                     {start of function to interpret}
            lot);                      {string to append expansion to}
          return;                      {all done}
          end;
        end;                           {end of "[" special character case}

      end;                             {end of special handling character cases}
    string_append1 (lot, c);           {append input char to output line}
    p := p + 1;                        {advance to next input character index}
    end;                               {back to do next input char}
  end;
{
****************************************************************************
*
*   Subroutine INLINE_EXPAND_FUNC (E, LIN, LINST, LOT)
*
*   Expand an inline function and the remainder of the input line.  The
*   input line is in LIN, and LINST is the index of the first character
*   after the "[" indicating the inline function.  The expansion of the
*   inline function and everything following it is appended to LOT.
}
procedure inline_expand_func (         {expand rest of line starting at inline func}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input string containing inline function}
  in      linst: string_index_t;       {start index of func right after "["}
  in out  lot: string_var8192_t);      {output string to append expansion to}
  val_param; internal;

var
  exp: string_var8192_t;               {expansion of any new inline functions}
  p: string_index_t;                   {input line parse index}
  oldlen: string_index_t;              {old length of EXP before truncated to func}
  quote: char;                         {character surrounding the quoted string}
  c: char;                             {current input line character}
  inquote: boolean;                    {TRUE if inside a quoted string}

label
  fnd_end;

begin
  exp.max := size_char(exp.str);       {init local var string}

  exp.len := 0;                        {make rest of line with functions resolved}
  inline_expand_lrest (e, lin, linst, exp);
{
*   Look thru the expanded remainder of the input line in EXP looking for the
*   end of the function.
}
  p := 1;                              {init input string parse index}
  inquote := false;                    {init to not within a quoted string}

  while p <= exp.len do begin          {loop until input line exhausted}
    c := exp.str[p];                   {fetch this input character}
    case c of                          {check for special handling characters}

'''', '"': begin                       {start and end of quotes strings}
        if inquote
          then begin                   {currently in a quoted string}
            inquote := c <> quote;     {FALSE if this is closing quote}
            end
          else begin                   {not currently in a quoted string}
            inquote := true;           {now in a quoted string}
            quote := c;                {set close quote character}
            end
          ;
        end;

']':  begin                            {end of inline function character}
        if not inquote then goto fnd_end; {found end of inline function ?}
        end;

      end;                             {end of special handling character cases}
    p := p + 1;                        {advance to next input character index}
    end;                               {back to do next input char}
{
*   The closing "]" indicating the end of this function is missing.  Interpret
*   the function as raw text.
}
  string_append1 (lot, '[');           {append the result to the output}
  string_append (lot, exp);
  return;
{
*   P is the EXP index of the "]" character that closes this inline function.
*   EXP will be temporarily truncated to only include the function string to
*   expand.  This will be expanded and appended to the output line, then the
*   rest of EXP after the "]" will be appended to the output line.
}
fnd_end:
  oldlen := exp.len;                   {save true length of remainder of line}
  exp.len := p - 1;                    {truncate at end of function body}
  escr_inline_func (e, exp, lot);      {perform function, append result to out str}
  exp.len := oldlen;                   {restore true length of line remainder}
  p := p + 1;                          {make first index after the whole function}
  while p <= exp.len do begin          {copy text after function to output line}
    string_append1 (lot, exp.str[p]);  {copy this character}
    p := p + 1;                        {advance to next character}
    end;                               {back to copy next character from EXP}
  end;
