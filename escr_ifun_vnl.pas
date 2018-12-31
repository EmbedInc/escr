{   Intrinsic function VNL arg
*
*   Evaluates the argument with local symbols ignored.  Specifically, for any
*   symbol that has a local version, the current version of that symbol is the
*   one before the local version.  The local versions are restore to the current
*   versions before the function returns.
*
*   This function is like V, except for the handling of local symbols.  The
*   purpose of this function is to be able to evaluate expressions in the
*   context of the parent block.  This is necessary for the proper evaluation of
*   call arguments in most cases.
}
module escr_ifun_vnl;
define escr_ifun_vnl;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_IFUN_VNL (E, STAT)
*
*   Implements the intrinsic function VNL.  See the header comments of
*   ESCR_IFUN.PAS for a description of the interface to intrinsic function
*   routines.
}
procedure escr_ifun_vnl (              {evaluate without local variables}
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

var
  lsym_p: escr_sylist_p_t;             {points to current local symbol}
  val: escr_val_t;                     {argument value}
  gotit: boolean;                      {got argument value}

begin
{
*   Set the current version of any symbol that has a local version to one before
*   the local version.
}
  lsym_p := e.exblock_p^.locsym_p;     {init to first local symbol in the list}
  while lsym_p <> nil do begin         {loop over the local symbols}
    lsym_p^.sym_p^.ent_p^.curr_p :=    {make curr version one before local}
      lsym_p^.sym_p^.prev_p;
    lsym_p := lsym_p^.next_p;          {advance to next local symbol in list}
    end;
{
*   Get the function argument value.
}
  gotit := escr_ifn_get_val (e, val, stat); {try to get the argument value}
{
*   Make the local versions of local symbols the current version again.
}
  lsym_p := e.exblock_p^.locsym_p;     {init to first local symbol in the list}
  while lsym_p <> nil do begin         {loop over the local symbols}
    lsym_p^.sym_p^.ent_p^.curr_p :=    {make curr version the local version}
      lsym_p^.sym_p;
    lsym_p := lsym_p^.next_p;          {advance to next local symbol in list}
    end;
{
*   Handle error getting function value.
}
  if not gotit then begin              {no function argument ?}
    escr_ifn_stat_required (e, stat);  {this argument is required}
    return;
    end;
  if sys_error(stat) then return;      {error getting function argument ?}

  escr_ifn_ret_val (e, val);           {return the value of the argument}
  end;
