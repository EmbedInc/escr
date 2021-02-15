{   Commands that manage subroutines.
}
module escr_cmd_subr;
define escr_cmd_subroutine;
define escr_cmd_endsub;
define escr_cmd_call;
define escr_cmd_return;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_CMD_SUBROUTINE (E, STAT)
*
*   Command SUBROUTINE name
*
*   Define the start of a subroutine.  The subroutine definition is stored, but
*   is not executed now.
}
procedure escr_cmd_subroutine (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {subroutine name}
  sym_p: escr_sym_p_t;                 {pointer to new subroutine name symbol}
  stat2: sys_err_t;                    {to avoid corrupting STAT}

label
  error;

begin
  name.max := size_char(name.str);     {init local var string}

  escr_inh_new (e);                    {create new execution inhibit layer}
  e.inhibit_p^.inhty := escr_inhty_blkdef_k; {inhibit is due to reading block definition}
  e.inhibit_p^.blkdef_type := escr_exblock_sub_k; {block type is subroutine}
  if e.inhibit_p^.inh then return;     {previously inhibited, don't define subroutine}
  e.inhibit_p^.inh := true;            {inhibit execution during subroutine definition}

  if not escr_get_token (e, name) then begin {get subroutine name}
    escr_stat_cmd_noarg (e, stat);
    goto error;
    end;

  escr_sym_new (                       {create new symbol for subroutine name}
    e, name, escr_sym_subr_k, false, sym_p, stat);
  if sys_error(stat) then goto error;

  sym_p^.subr_line_p :=                {save pointer to subroutine definition line}
    escr_in_line (e);
  return;

error:                                 {error after inhibit created, STAT set}
  escr_inh_end (e, stat2);             {delete the execution inhibit}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_ENDSUB (E, STAT)
*
*   Command ENDSUB
*
*   Indicate the end of a subroutine definition.
}
procedure escr_cmd_endsub (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if not e.inhibit_p^.inh then begin   {executing code normally ?}
    escr_cmd_return (e, stat);         {acts just like RETURN}
    return;
    end;

  if                                   {not defining a subroutine ?}
      (e.inhibit_p^.inhty <> escr_inhty_blkdef_k) or {not in a block definition ?}
      (e.inhibit_p^.blkdef_type <> escr_exblock_sub_k) {block is not a subroutine ?}
      then begin
    sys_stat_set (escr_subsys_k, escr_err_notsubdef_k, stat);
    return;
    end;

  escr_inh_end (e, stat);              {end excution inhibit due to subroutine def}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_CALL (E, STAT)
*
*   Command CALL name [arg ... arg]
*
*   Execute the indicated subroutine, then return to after this command when the
*   subroutine completes.
}
procedure escr_cmd_call (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  name: string_var80_t;                {subroutine name}
  tk: string_var8192_t;                {scratch token}
  sym_p: escr_sym_p_t;                 {pointer to definition symbol}

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  tk.max := size_char(tk.str);

  if not escr_get_token (e, name) then begin {get subroutine name}
    escr_stat_cmd_noarg (e, stat);
    return;
    end;

  escr_sym_find_type (                 {get symbol from subroutine name}
    e, name, escr_sytype_subr_k, sym_p, stat);
  if sys_error(stat) then return;
  if sym_p = nil then begin            {not found ?}
    escr_stat_sym_nfound (name, stat);
    return;
    end;

  escr_exblock_new (e, stat);          {create new execution block}
  if sys_error(stat) then return;
  escr_exblock_refsym (e, sym_p^);     {indicate referencing symbol for this subroutine}
  e.exblock_p^.bltype := escr_exblock_sub_k; {new block is a subroutine}
  e.exblock_p^.args := true;           {this block can take arguments}
  escr_exblock_arg_addn (e, name, 0);  {subroutine name is special argument 0}
  while true do begin                  {loop until argument list exhausted}
    if not escr_get_tkraw (e, tk) then exit; {get next argument}
    escr_exblock_arg_add (e, tk);      {add as next argument to new execution block}
    end;

  fline_lpos_push (                    {one logical input location level lower}
    e.fline_p^,                        {FLINE library use state}
    e.lpos_p,                          {logical position chain to update}
    escr_in_line(e));                  {pointer to current input line}
  e.exblock_p^.lpos_p := e.lpos_p;     {set the new position as the parent pos this block}

  escr_exblock_goto_line_aft (         {start at first line of subroutine}
    e, sym_p^.subr_line_p, stat);
  if sys_error(stat) then return;
  escr_exblock_ulab_init (e);          {create table for local labels}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_CMD_RETURN (E, STAT)
*
*   Command RETURN
*
*   Return from the innermost routine currently in.  This command performs the
*   run-time exit from any of these blocks:
*
*     Subroutine
*     Command
*     Function
}
procedure escr_cmd_return (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}

  if not escr_get_end (e, stat) then return; {check for no extra command line tokens}

  while e.exblock_p <> nil do begin    {up thru the nested blocks}
    case e.exblock_p^.bltype of        {what kind of block is this ?}
escr_exblock_sub_k,                    {this is the block to close}
escr_exblock_cmd_k,
escr_exblock_func_k,
escr_exblock_mac_k: begin
        escr_exblock_close (e, stat);  {end this routine}
        return;
        end;
escr_exblock_top_k: begin              {invalid block type for RETURN}
        sys_stat_set (escr_subsys_k, escr_err_nretblk_k, stat);
        return;
        end;
otherwise                              {all other blocks, close and keep looking}
      escr_exblock_close (e, stat);    {close this block}
      if sys_error(stat) then return;
      end;                             {end of block type cases}
    end;                               {back to close new current block}
  end;
