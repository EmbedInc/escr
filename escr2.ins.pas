{   Private include file for use by the modules that implement the ESCR
*   subsystem.
}
%include 'base.ins.pas';
%include 'escr.ins.pas';
{
*   Subroutines that implement the individual preprocessor commands.  Each
*   of these have the same interface.  See the header comments in module
*   ESCR_CMD for the details.
}
procedure escr_cmd_block (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_call (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_const (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_del (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_else (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endblock (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endif (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endloop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endmac (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endsub (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_if (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_include (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_loop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_macro (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_quit (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_quitmac (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_repeat (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_return (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_set (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_show (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_stop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_subroutine (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_sylist (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_then (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_var (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_write (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_writepop (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_writepush (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;
{
*   Subroutines that implement the individual intrinsic functions.  Each
*   of these have the same interface.  See the header comments in module
*   ESCR_IFUN for the details.
}
procedure escr_ifun_abs(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_and(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_arg(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_ccode(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_char(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_chars(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_cos(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_date(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_degr(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_div(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_divide(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_e(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_eng(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_eq(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_evar(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_exist(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_exp(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_fp(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_ge(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_gt(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_if(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_int(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_inv(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_lab(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_lcase(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_le(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_lnam(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_log(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_log2(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_lt(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_max(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_min(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_minus(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_ne(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_not(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_now(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_or(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_pi(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_plus(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_postdec(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_postinc(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_predec(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_preinc(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_qstr(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_rdeg(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_rnd(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_seq(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_shiftl(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_shiftr(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_sin(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_sindx(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_slen(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_sqrt(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_str(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_substr(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_sym(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_tan(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_times(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_tnam(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_trunc(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_ucase(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_v(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_xor(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;
{
*   Other entry points.
}
procedure escr_err_parm_last_bad (     {last parameter parsed was bad}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  options (val_param, noreturn, extern);

procedure escr_err_parm_missing (      {a required command parameter not found}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      subsys: string;              {name of subsystem, used to find message file}
  in      msg: string;                 {message name withing subsystem file}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn, extern);

procedure escr_format_fp (             {create specifically formatted floating point string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fp: sys_fp_max_t;            {input floating point value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned floating point string}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_format_int (            {create specifically formatted integer string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      i: sys_int_max_t;            {input integer value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned integer string}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_get_tkrawc (             {get next raw token, comma delimited}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

procedure escr_ulab_get (              {get expansion of generic unique label name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {generic unique label name}
  in out  exp: univ string_var_arg_t); {returned full label name (expansion)}
  val_param; extern;

function escr_macro_run (              {run macro if present on curr input line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {macro was processed}
  val_param; extern;

procedure escr_str_from_time (         {make string from absolute time descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      time: sys_clock_t;           {input absolute time descriptor}
  in out  s: univ string_var_arg_t);   {returned string representation of the time}
  val_param; extern;

procedure escr_str_from_fp (           {make string from floating point value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fp: sys_fp_max_t;            {floating point input value}
  in out  s: univ string_var_arg_t);   {returned string representation}
  val_param; extern;

function escr_str_to_time (            {make absolute time descriptor from string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {input string}
  out     time: sys_clock_t)           {returned time descriptor}
  :boolean;                            {TRUE on success}
  val_param; extern;

