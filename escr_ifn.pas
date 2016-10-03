{   Module of helper routine for implementing intrinsic functions.
}
module escr_ifn;
define escr_ifn_val;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Function ESCR_IFN_VAL (E, VAL, STAT)
*
*   Get the next function parameter and return its value in VAL.  The function
*   value is TRUE iff a term was available.  When the function returns TRUE,
*   STAT always indicates no error.
}
function escr_ifn_val (                {get arbitrary value of next func parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t;             {returned term value}
  out     stat: sys_err_t)
  :boolean;                            {term was available}
  val_param;

begin
  escr_ifn_val := escr_term_get (e, e.funstr.s, e.funstr.p, val, stat);
  end;
{
********************************************************************************
*
*   Function ESCR_IFN_BOOL (E, B, STAT)
*
*   Get the next function parameter as a boolean value into B.  The function
*   returns TRUE iff a boolean term was found.  STAT is returned with error if
*   a term was found but it could not be converted to a boolean value.  If no
*   term was found, the function returns FALSE and STAT does not indicate error.
}
function escr_ifn_bool (               {get boolean value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean;                  {returned boolean value}
  out     stat: sys_err_t)
  :boolean;                            {term was available}
  val_param;

begin
  escr_ifn_bool := false;
  sys_stat_set (sys_subsys_k, sys_stat_not_impl_name_k, stat);
  sys_stat_parm_str ('ESCR_IFN_BOOL', stat);
  end;
