{   Private include file for use by the modules that implement the ESCR
*   subsystem.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'strflex.ins.pas';
%include 'fline.ins.pas';
%include 'escr.ins.pas';
{
*   Routines that are really internal to ESCR, but need to be visible between
*   modules.  These routines are here because they are not intended for use by
*   applications.
}
procedure escr_out_new (               {make new output file level, not filled in}
  in out  e: escr_t);                  {ESCR lib use state, E.OUT_P pnt to new level}
  val_param; extern;

procedure escr_out_remove (            {remove and dealloc curr out, back to previous}
  in out  e: escr_t);                  {ESCR lib use state, E.OUT_P pnt to previous}
  val_param; extern;
{
*   Subroutines that implement the individual preprocessor commands.  Each
*   of these have the same interface.  See the header comments in module
*   ESCR_CMD for the details.
}
procedure escr_cmd_append (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_block (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_call (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_command (
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

procedure escr_cmd_dir (
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

procedure escr_cmd_endcmd (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endif (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endfunc (
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

procedure escr_cmd_endpick (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_endsub (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_funcval (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_funcstr (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_if (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_function (
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

procedure escr_cmd_option (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_optionelse (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_optionif (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_pick (
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

procedure escr_cmd_quitopt (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_quitpick (
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

procedure escr_cmd_run (
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

procedure escr_ifun_dent(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_dir(
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

procedure escr_ifun_file(
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

procedure escr_ifun_in(
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

procedure escr_ifun_isint(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_isnum(
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

procedure escr_ifun_pick(
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

procedure escr_ifun_qtk(
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

procedure escr_ifun_runex(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_runso(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_runtf(
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

procedure escr_ifun_unquote(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_v(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_vnl(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifun_xor(
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;
