{   Subroutine management.
}
module prepic_cmd_subr;
define prepic_cmd_subroutine;
define prepic_cmd_endsub;
define prepic_cmd_call;
define prepic_cmd_return;
%include 'prepic.ins.pas';
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_SUBROUTINE (STAT)
*
*   /SUBROUTINE name
*
*   Define the start of a subroutine.  The subroutine definition is stored,
*   but is not executed now.
}
procedure prepic_cmd_subroutine (
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {subroutine name}
  sz: sys_int_adr_t;                   {size of new descriptor}
  sym_p: sym_p_t;                      {pointer to new subroutine name symbol}

begin
  name.max := size_char(name.str);     {init local var string}

  inh_new;                             {create new execution inhibit layer}
  inhibit_p^.inhty := inhty_blkdef_k;  {inhibit it due to reading block definition}
  inhibit_p^.blkdef_type := exblock_sub_k; {block type is subroutine}
  if inhibit_p^.inh then return;       {previously inhibited, don't define subroutine}
  inhibit_p^.inh := true;              {inhibit execution during subroutine definition}

  if not get_token (name)              {get subroutine name}
    then err_parm_missing ('', '', nil, 0);
  err_check_symname (name);            {check for valid symbol name}

  sz :=                                {make size of whole subroutine symbol}
    offset(sym_t.subr_line_p) + size_min(sym_t.subr_line_p);
  sym_new (name, sz, false, sym_p);    {create new symbol for subroutine name}
  sym_p^.stype := sym_subr_k;          {this symbol is a subroutine name}
  sym_p^.subr_line_p :=                {save pointer to subroutine definition line}
    exblock_p^.inpos_p^.last_p;
  end;
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_ENDSUB (STAT)
*
*   /ENDSUB
*
*   Indicate the end of a subroutine definition.
}
procedure prepic_cmd_endsub (
  out     stat: sys_err_t);
  val_param;

begin
  if not inhibit_p^.inh then begin     {executing code normally ?}
    prepic_cmd_return (stat);          {acts just like RETURN}
    return;
    end;

  if                                   {not defining a subroutine ?}
      (inhibit_p^.inhty <> inhty_blkdef_k) or {not in a block definition ?}
      (inhibit_p^.blkdef_type <> exblock_sub_k) {block is not a subroutine ?}
      then begin
    err_atline ('pic', 'not_in_subdef', nil, 0);
    end;

  inh_end;                             {end excution inhibit due to subroutine def}
  end;
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_CALL (STAT)
*
*   /CALL name [arg ... arg]
*
*   Execute the indicated subroutine, then return to after this command when the
*   subroutine completes.
}
procedure prepic_cmd_call (
  out     stat: sys_err_t);
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  name: string_var80_t;                {subroutine name}
  tk: string_var8192_t;                {scratch token}
  sym_p: sym_p_t;                      {pointer to definition symbol}
  str_p: string_var_p_t;               {pointer to input line}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);

  if not get_token (name)              {get subroutine name}
    then err_parm_missing ('', '', nil, 0);
  err_check_symname (name);            {check for valid symbol name}
  sym_find (name, sym_p);              {get symbol from subroutine name}
  if sym_p = nil then err_sym_not_found (name); {no such symbol ?}
  if sym_p^.stype <> sym_subr_k then begin {symbol not a subroutine ?}
    sys_msg_parm_vstr (msg_parm[1], name);
    err_atline ('pic', 'sym_not_subr', msg_parm, 1);
    end;

  exblock_new;                         {create new execution block}
  exblock_p^.sym_p := sym_p;           {set pointer to symbol for this block}
  exblock_p^.bltype := exblock_sub_k;  {new block is a subroutine}
  exblock_p^.args := true;             {this block can take arguments}
  exblock_arg_addn (name, 0);          {subroutine name is special argument 0}
  while true do begin                  {loop until argument list exhausted}
    if not get_tkraw (tk) then exit;   {get next argument}
    exblock_arg_add (tk);              {add as next argument to new execution block}
    end;
  exblock_inline_set (sym_p^.subr_line_p); {go to subroutine definition line}
  discard( infile_getline (str_p) );   {advance past subroutine definition line}
  exblock_loclab_init;                 {create table for local labels}
  end;
{
********************************************************************************
*
*   Subroutine PREPIC_CMD_RETURN (STAT)
*
*   /RETURN
*
*   Return from the innermost subroutine currently in.
}
procedure prepic_cmd_return (
  out     stat: sys_err_t);
  val_param;

begin
  if inhibit_p^.inh then return;       {execution is inhibited ?}

  while exblock_p^.bltype <> exblock_sub_k do begin {up thru blocks until first subr}
    if exblock_p^.prev_p = nil then begin {at top execution block ?}
      err_atline ('pic', 'not_in_sub', nil, 0); {complain not in a subroutine}
      end;
    exblock_close;                     {end this execution block, make previous current}
    end;                               {back to check this new execution block}

  exblock_close;                       {end the subroutine execution block}
  end;
