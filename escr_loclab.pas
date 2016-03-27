{   Routines that deal with local labels.  Some types of execution blocks can
*   have a list of local labels.  These have generic names within the namespace
*   of the execution block, but the actual label name in the output file is
*   made globally unique by appending "_XXX", where XXX is a globally unique
*   sequential label number.
*
*   The local labels are maintained in a STRING_HASH list.  The generic label
*   name is the index to the list.  Each list entry contains a pointer to a
*   string that is the full expansion of the label as written to the output
*   file.
*
*   Execution blocks that do not hold a local labels list inherit the local
*   labels list of the first parent that does hold such a list.  The top level
*   execution block always holds a local labels list.
}
module escr_loclab;
define loclab_get;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine LOCLAB_GET (NAME, STR_P)
*
*   Get the expansion of the local label NAME.  STR_P is returned pointing to
*   the expansion string.
*
*   If the local label does not previously exist, the expansion will be created
*   first, using the globally unique sequential local label number LABELN.  Each
*   LABELN value is only used to create one local label.
*
*   The local local label context of the innermost execution block that has one
*   is used.
*
*   Local label names are case_sensitive when labels in the output language are
*   case-sensitive.
}
procedure loclab_get (                 {get expansion of generic local label name}
  in      name: univ string_var_arg_t; {generic local label name}
  in out  exp: univ string_var_arg_t); {returned full label name (expansion)}
  val_param;

var
  lname: string_var80_t;               {local copy of label name, generic and full}
  fname: string_var80_t;               {full label name, name used in output file}
  tk: string_var32_t;                  {scratch token}
  ex_p: escr_exblock_p_t;              {pointer to execution block with labels table}
  pos: string_hash_pos_t;              {position within local labels table}
  name_p: string_var_p_t;              {scratch pointer to name string}
  dat_p: ^string_var_p_t;              {pointer data for a table entry}
  ii, jj: sys_int_machine_t;           {scratch integers}
  sz: sys_int_adr_t;                   {amount of memory to allocate}
  found: boolean;                      {TRUE if entry found in table}
  stat: sys_err_t;

begin
  lname.max := size_char(lname.str);   {init local var strings}
  fname.max := size_char(fname.str);
  tk.max := size_char(tk.str);

  string_copy (name, lname);           {make local copy of generic label name}
  case lang of                         {which output language is this?}
lang_aspic_k: begin                    {all the case-insensitive languages}
      string_upcase (lname);           {upper case to avoid case aliases}
      end;
    end;
{
*   Find the local labels table to use.  This is the first table that exists
*   from the current execution block to successively higher parents.  The top
*   execution block always has a local labels table.
}
  ex_p := e.exblock_p;                 {init to look for labels table in current block}
  while ex_p^.loclab = nil do begin    {no local labels table here ?}
    ex_p := ex_p^.prev_p;              {go to parent execution block and try again}
    end;

  string_hash_pos_lookup (             {try to find existing entry for this name}
    ex_p^.loclab,                      {handle to the table}
    lname,                             {name to look up}
    pos,                               {returned position in the table}
    found);                            {TRUE iff the name was found in the table}

  if found then begin                  {this label is already in the list ?}
    string_hash_ent_atpos (            {get the data for this entry}
      pos,                             {position of entry to get info about}
      name_p,                          {returned pointer to name in table (unused)}
      dat_p);                          {returned pointer to data for this entry}
    name_p := dat_p^;                  {get pointer to the label expansion}
    string_copy (name_p^, exp);        {return the expansion}
    return;
    end;
{
*   The generic label name is not in the table, so we have to create the label
*   expansion string and store it in a new table entry.
}
  {
  *   Make the label expansion in FNAME.
  }
  string_copy (lname, fname);          {init full name to generic name}
  string_append1 (fname, '_');         {append underscore}

  string_f_int_max_base (              {make unique label number string}
    tk,                                {output string}
    e.labeln,                          {input integer}
    10,                                {radix}
    0,                                 {no fixed field width, use what it takes}
    [string_fi_unsig_k],               {the input number is unsigned}
    stat);
  escr_err_atline_abort (stat, '', '', nil, 0);
  e.labeln := e.labeln + 1;            {update unique number of next time}

  jj := 3 - tk.len;                    {number of leading zeros to add}
  for ii := 1 to jj do begin           {add the leading zeros}
    string_append1 (fname, '0');
    end;
  string_append (fname, tk);           {add the sequence number}
  {
  *   Create the table entry for this label.  The name the label will be
  *   referred to in ESCR source code (generic name) is in LNAME, and the full
  *   expanded name of the label in FNAME.  POS is the position within the table
  *   where the new entry is to be added.
  }
  string_hash_ent_add (                {create the new table entry}
    pos,                               {position at which to create the new entry}
    name_p,                            {returned pointer to reference name in the table}
    dat_p);                            {returned pointer to data for this entry}

  string_copy (lname, name_p^);        {set the name of this table entry}

  sz := string_size(fname.len);        {find memory size required for expansion string}
  string_hash_mem_alloc_ndel (         {allocate memory for the label expansion}
    ex_p^.loclab,                      {handle to the table}
    sz,                                {amount of memory to allocate}
    name_p);                           {returned pointer to the new memory}
  name_p^.max := fname.len;            {save the expansion string}
  string_copy (fname, name_p^);
  dat_p^ := name_p;                    {entry data is the address of the exp string}

  string_copy (name_p^, exp);          {return the label expansion}
  end;
