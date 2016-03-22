{   Routines for managing execution inhibits.  Execution inhibts are used to skip
*   over sections of code that are not to be executed, like the ELSE section of
*   a IF command if the condition was true.
}
module prepic_inh;
%include 'prepic.ins.pas';
define inh_new;
define inh_end;
{
********************************************************************************
*
*   Subroutine INH_NEW
*
*   Create a new execution inhibit, initialize it, and set it as current.
*   Execution enabled/disabled will be preserved as it was previously.  The
*   inhibit type will be set to that of a generic execution block.
}
procedure inh_new;                     {create new execution inhibit}
  val_param;

var
  inh_p: inh_p_t;                      {pointer to new inhibit descriptor}

begin
  util_mem_grab (                      {allocate memory for the new inhibit}
    sizeof(inh_p^), mem_top_p^, true, inh_p);
  inh_p^.prev_p := inhibit_p;          {point back to previous layered inhibit}
  inh_p^.inh := false;                 {init to execution enabled}
  if inhibit_p <> nil then inh_p^.inh := inhibit_p^.inh; {copy previous enable if available}
  inh_p^.inhty := inhty_blk_k;         {init to ihibit for execution block}
  inh_p^.blk_p := exblock_p;

  inhibit_p := inh_p;                  {set the new inhibit as current}
  end;
{
********************************************************************************
*
*   Subroutine INH_END
*
*   Delete the current execution inhibit and pop back to the previous one.
}
procedure inh_end;                     {end the current execution inhibit}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  inh_p: inh_p_t;                      {pointer to execution inhibit to delete}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if inhibit_p = exblock_p^.inh_p^.prev_p then begin {going too far for this block ?}
    case inhibit_p^.inhty of           {what type of inhibit is it ?}
inhty_if_k: begin                      {IF construct}
        err_atline ('pic', 'err_nest_ih_if', nil, 0);
        end;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(inhibit_p^.inhty));
      err_atline ('pic', 'err_nest_inh', msg_parm, 1);
      end;
    end;

  inh_p := inhibit_p;                  {save pointer to inhibit to delete}
  inhibit_p := inh_p^.prev_p;          {make previous inhibit current}
  util_mem_ungrab (inh_p, mem_top_p^); {deallocate the old inhibit descriptor}
  end;
