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

procedure escr_cmd_writeend (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_cmd_writeto (
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

procedure escr_exblock_arg_add (       {add argument to current block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {argument string}
  val_param; extern;

procedure escr_exblock_arg_addn (      {add argument to current block, specific number}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t;  {argument string}
  in      n: sys_int_machine_t);       {argument number}
  val_param; extern;

procedure escr_exblock_arg_get (       {get value of currently visible argument}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param; extern;

procedure escr_exblock_arg_get_bl (    {get value of argument to specific block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      bl: escr_exblock_t;          {execution block to get argument of}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param; extern;

procedure escr_exblock_close (         {close curr execution block and delete temp state}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_inline_push (   {push new source line location for exec block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t);    {pointer to next input line to use}
  val_param; extern;

procedure escr_exblock_inline_set (    {go to new input source position in curr block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t);    {pointer to next input line to use}
  val_param; extern;

procedure escr_exblock_ulab_init (     {create unique labels list in this block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_new (           {create and install new execution block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_quit (          {stop executing in the current block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_repeat (        {loop back to start of block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

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

procedure escr_infile_find (           {find existing input file descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      tnam: univ string_var_arg_t; {full unique treename of the file}
  out     infile_p: escr_infile_p_t);  {points to snippet, or NIL for not found}
  val_param; extern;

procedure escr_infile_getline (        {get next input stream source line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     str_p: string_var_p_t);      {returned pointer to source line or NIL}
  val_param; extern;

procedure escr_infile_add_line (       {add line to source file snippet}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     infile: escr_infile_t;       {snippet to add the line to}
  in      line: univ string_var_arg_t; {the source line}
  in      lnum: sys_int_machine_t);    {source line number within its file}
  val_param; extern;

procedure escr_infile_add_lines (      {add lines to source file snippet}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     infile: escr_infile_t;       {snippet to add the lines to}
  var     conn: file_conn_t;           {existing connection to text file}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_infile_new (            {create new input file snippet descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      tnam: univ string_var_arg_t; {full unique treename of the file}
  out     infile_p: escr_infile_p_t);  {returned pointer to input file descriptor}
  val_param; extern;

procedure escr_infile_open (           {find file data or read it into memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {file name}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     infile_p: escr_infile_p_t;   {returned pointer to input file descriptor}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_infile_pop (            {pop back one nested input file level}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_infile_skipline (       {skip next input file line}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_inh_new (               {create new execution inhibit}
  in out e: escr_t);                   {state for this use of the ESCR system}
  val_param; extern;

procedure escr_inh_end (               {end the current execution inhibit}
  in out e: escr_t);                   {state for this use of the ESCR system}
  val_param; extern;

procedure escr_inline_expand_line (    {expand all inline functions of a line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  out     lot: string_var8192_t;       {output line, contains no inline functions}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_inline_func (           {perform inline function operation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  lot: string_var8192_t;       {string to append function expansion to}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_ulab_get (              {get expansion of generic unique label name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {generic unique label name}
  in out  exp: univ string_var_arg_t); {returned full label name (expansion)}
  val_param; extern;

function escr_loop_iter (              {advance to next loop iteration}
  in out  e: escr_t)                   {state for this use of the ESCR system}
  :boolean;                            {looped back, not terminated}
  val_param; extern;

function escr_macro_run (              {run macro if present on curr input line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {macro was processed}
  val_param; extern;

function escr_excl_check (             {check for syntax exclusion at char}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stri: univ string_var_arg_t; {input string}
  in out  p: string_index_t;           {index to check for syntax exclusion at}
  in      excl_p: escr_syrlist_p_t;    {list of syntax ranges, each one exclusion}
  in      stro_p: univ string_var_p_t; {points to output string}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {excl found, P changed, excl appended to STRO}
  val_param; extern;

procedure escr_out_close (             {close the current output file, pop previous}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the file}
  val_param; extern;

procedure escr_run_atlabel (           {run starting at label}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of label to start running at}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_atline (            {run starting at specific input files line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to first input files line to execute}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_stop (              {unconditionally stop execution}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_show_obuf (             {write line to standard output from OBUF}
  in out  e: escr_t);                  {state for this use of the ESCR system}
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

procedure escr_sym_del_name (          {delete symbol by name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sytable: escr_sytable_t;     {symbol table to delete symbol from}
  in      name: univ string_var_arg_t; {symbol name}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_find_any (          {look up symbol in all symbol tables}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  out     sym_p: escr_sym_p_t);        {returned pointer to symbol, NIL if not found}
  val_param; extern;

procedure escr_sym_new (               {create new symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      sz: sys_int_adr_t;           {size of the whole symbol descriptor}
  in      global: boolean;             {create global, not local symbol}
  in out  sytable: escr_sytable_t;     {symbol table to add symbol to}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_new_cmd (           {create new user-defined command symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      line_p: escr_inline_p_t;     {pointer to first line of command definition}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_new_func (          {create new user-defined function symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      line_p: escr_inline_p_t;     {pointer to first line of function definition}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_new_icmd (          {create new intrinsic command symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      routine_p: escr_icmd_p_t;    {pointer to command routine}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_new_ifunc (         {create new intrinsic function symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      routine_p: escr_ifunc_p_t;   {pointer to function routine}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_syrlist_add (           {add new blank entry to syntax ranges list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  syl_p: escr_syrlist_p_t;     {pointer to list, will point to new entry}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_syrlist_clear (         {clear syntax ranges list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  syl_p: escr_syrlist_p_t);    {pointer to list to clear, returned NIL}
  val_param; extern;

function escr_term_get (               {get value of next term in list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index}
  out     val: escr_val_t;             {returned value of the term}
  out     stat: sys_err_t)             {completion status, no error on func TRUE}
  :boolean;                            {TRUE if term was available}
  val_param; extern;

procedure escr_uptocomm (              {find line length without comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {the input string}
  out     nclen: string_index_t);      {string length with comment removed}
  val_param; extern;

procedure escr_val_copy (              {copy and convert value to target data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      ival: escr_val_t;            {the input value}
  out     oval: escr_val_t;            {output val, must be set up except actual data}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_val_bool (               {convert to boolean value or return error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {the boolean value of VAL}
  val_param; extern;

function escr_val_fp (                 {convert to FP value or return error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_fp_max_t;                       {floating point value of VAL}
  val_param; extern;

procedure escr_val_init (              {initialize full VAL_T descriptor to data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {data type to set up VAL for}
  out     val: escr_val_t);            {full value descriptor to initialize}
  val_param; extern;

function escr_val_int (                {convert to integer value or return with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_int_max_t;                      {integer value of VAL}
  val_param; extern;

function escr_val_time (               {convert to time value or return with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {source value}
  out     stat: sys_err_t)             {completion status}
  :sys_clock_t;                        {time value of VAL}
  val_param; extern;

function escr_val_isbool (             {check for VAL can be converted to boolean}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {input value to check}
  out     b: boolean)                  {boolean value, unaltered if VAL not boolean}
  :boolean;                            {TRUE if returning boolean value in B}
  val_param; extern;

function escr_val_isint (              {distinguish between INT or FP, error if neither}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  out     vi: sys_int_max_t;           {returned integer value, if integer}
  out     vf: sys_fp_max_t;            {returned floating point value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if integer, FALSE on FP or error}
  val_param; extern;

function escr_val_size (               {return minimum required size of VAL_T structure}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {data type of the value}
  in      len: sys_int_machine_t)      {max string chars to hold, ignored other dtypes}
  :sys_int_adr_t;
  val_param; extern;

procedure escr_val_str (               {convert value to its string representation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param; extern;

procedure escr_val_text (              {make output language text representation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param; extern;
