{   Routines for managing execution inhibits.  Execution inhibts are used to skip
*   over sections of code that are not to be executed, like the ELSE section of
*   a IF command if the condition was true.
}
module escr_inh;
%include 'escr2.ins.pas';
define escr_inh_new;
define escr_inh_end;
{
********************************************************************************
*
*   Subroutine ESCR_INH_NEW (E)
*
*   Create a new execution inhibit, initialize it, and set it as current.
*   Execution enabled/disabled will be preserved as it was previously.  The
*   inhibit type will be set to that of a generic execution block.
}
procedure escr_inh_new (               {create new execution inhibit}
  in out e: escr_t);                   {state for this use of the ESCR system}
  val_param;

var
  inh_p: escr_inh_p_t;                 {pointer to new inhibit descriptor}

begin
  util_mem_grab (                      {allocate memory for the new inhibit}
    sizeof(inh_p^), e.mem_top_p^, true, inh_p);
  inh_p^.prev_p := e.inhibit_p;        {point back to previous layered inhibit}
  inh_p^.inh := false;                 {init to execution enabled}
  if e.inhibit_p <> nil then inh_p^.inh := e.inhibit_p^.inh; {copy previous enable if available}
  inh_p^.inhty := escr_inhty_blk_k;    {init to ihibit for execution block}
  inh_p^.blk_p := e.exblock_p;

  e.inhibit_p := inh_p;                {set the new inhibit as current}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_INH_END
*
*   Delete the current execution inhibit and pop back to the previous one.
}
procedure escr_inh_end (               {end the current execution inhibit}
  in out e: escr_t);                   {state for this use of the ESCR system}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  inh_p: escr_inh_p_t;                 {pointer to execution inhibit to delete}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if e.inhibit_p = e.exblock_p^.inh_p^.prev_p then begin {going too far for this block ?}
    case e.inhibit_p^.inhty of         {what type of inhibit is it ?}
escr_inhty_if_k: begin                 {IF construct}
        escr_err_atline ('pic', 'err_nest_ih_if', nil, 0);
        end;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(e.inhibit_p^.inhty));
      escr_err_atline ('pic', 'err_nest_inh', msg_parm, 1);
      end;
    end;

  inh_p := e.inhibit_p;                {save pointer to inhibit to delete}
  e.inhibit_p := inh_p^.prev_p;        {make previous inhibit current}
  util_mem_ungrab (inh_p, e.mem_top_p^); {deallocate the old inhibit descriptor}
  end;
