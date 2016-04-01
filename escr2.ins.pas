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
*
*   Other entry points.
}
procedure escr_close_out (             {close the current output file, pop previous}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the file}
  val_param; extern;

procedure escr_close_out_all (         {close all output files}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the files}
  val_param; extern;

procedure escr_err_atline (            {show error followed by source line number}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      subsys: string;              {name of subsystem, used to find message file}
  in      msg: string;                 {message name within subsystem file}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn, extern);

procedure escr_err_check_symname (     {abort with error on invalid symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t); {symbol name to check}
  val_param; extern;

procedure escr_err_dtype_unimp (       {unimplemented data type internal error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {unimplemented data type}
  in      routine: string);            {name of the routine where data type unimplemented}
  options (val_param, noreturn, extern);

procedure escr_err_lang (              {unexpected input language identifier}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      lang: escr_lang_k_t;         {the language identifier}
  in      module: string;              {source module name where error encountered}
  in      checkpoint: sys_int_machine_t); {unique number for this occurrence}
  options (val_param, noreturn, extern);

procedure escr_err_sym_not_found (     {symbol not found}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t); {symbol name that was not found}
  options (val_param, noreturn, extern);

procedure escr_err_parm_bad (          {bomb with bad parameter to command error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      parm: univ string_var_arg_t); {the offending parameter}
  options (val_param, noreturn, extern);

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

procedure escr_err_val (               {show value and data type of offending value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t);            {the value}
  val_param; extern;

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

procedure escr_infile_pop (            {pop back one nested input file level}
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

procedure escr_get_args_str (          {get string representation of remaining parameters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t); {concatenation of remaining args converted to strings}
  val_param; extern;

function escr_get_bool (               {get next parameter as boolean}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean)                  {returned boolean value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_dtype (              {get next parameter as data type ID}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     dtype: escr_dtype_k_t)       {returned data type ID}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

procedure escr_get_end (               {make sure no more tokens left on input line}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

function escr_get_fp (                 {get next parameter as floating point}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     fp: sys_fp_max_t)            {returned floating point value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_int (                {get next parameter as an integer}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     i: sys_int_max_t)            {returned integer value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_time (               {get the next token as a time value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     time: sys_clock_t)           {returned time value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

procedure escr_get_keyword (           {get next parameter as keyword in a list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      klist: string;               {keywords, upper case separated by blanks}
  out     pick: sys_int_machine_t);    {1-N keyword number, 0 = no token available}
  val_param; extern;

function escr_get_str (                {get string representation of next parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t)  {returned string}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_tkraw (              {get next raw input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function escr_get_tkrawc (             {get next raw token, comma delimited}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function escr_get_token (              {get next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function escr_get_val (                {get value of next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t)             {returned value}
  :boolean;                            {TRUE if token available}
  val_param; extern;

function escr_get_val_dtype (          {get next input value, to dtype of VAL}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  val: escr_val_t)             {returned value, DTYPE specifies data type}
  :boolean;                            {TRUE if token available}
  val_param; extern;

procedure escr_infile_getline (        {get next input stream source line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     str_p: string_var_p_t);      {returned pointer to source line or NIL}
  val_param; extern;

procedure escr_infile_open (           {find file data or read it into memory}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {file name}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     infile_p: escr_infile_p_t;   {returned pointer to input file descriptor}
  out     stat: sys_err_t);            {completion status}
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
  out     lot: string_var8192_t);      {output line, contains no inline functions}
  val_param; extern;

procedure escr_inline_func_init (      {one-time init for processing inline funcs}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_inline_func (           {perform inline function operation}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {function source string, delimiters removed}
  in out  lot: string_var8192_t);      {string to append function expansion to}
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
  :boolean; extern;                    {macro was processed}

procedure escr_run (                   {run starting at first line of first input file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
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
  in      fp: double;                  {floating point input value}
  in out  s: univ string_var_arg_t);   {returned string representation}
  val_param; extern;

function escr_str_to_time (            {make absolute time descriptor from string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {input string}
  out     time: sys_clock_t)           {returned time descriptor}
  :boolean;                            {TRUE on success}
  val_param; extern;

procedure escr_sym_del (               {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t);        {pointer to symbol to delete, returned NIL}
  val_param; extern;

procedure escr_sym_del_name (          {delete symbol by name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sytable: escr_sytable_t;     {symbol table to delete symbol from}
  in      name: univ string_var_arg_t; {symbol name}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_find (              {look up symbol in symbol table}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in out  sytable: escr_sytable_t;     {symbol table to look up name in}
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

procedure escr_sym_new_const (         {create new symbol for a constant}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: escr_dtype_k_t;       {data type of the constant}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_new_icmd (          {create new intrinsic command symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      routine_p: escr_icmd_p_t;    {pointer to routine to command routine}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_term_get (               {get value of next term in list}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index}
  out     val: escr_val_t)             {returned value of the term}
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
  out     oval: escr_val_t);           {output val, must be set up except actual data}
  val_param; extern;

function escr_val_bool (               {convert to boolean value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t)             {source value}
  :boolean;                            {the boolean value of VAL}
  val_param; extern;

function escr_val_fp (                 {convert to FP value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t)             {source value}
  :sys_fp_max_t;                       {floating point value of VAL}
  val_param; extern;

procedure escr_val_init (              {initialize full VAL_T descriptor to data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {data type to set up VAL for}
  out     val: escr_val_t);            {full value descriptor to initialize}
  val_param; extern;

function escr_val_int (                {convert to integer value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t)             {source value}
  :sys_int_max_t;                      {integer value of VAL}
  val_param; extern;

function escr_val_time (               {convert to time value or bomb with error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t)             {source value}
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
  out     vf: sys_fp_max_t)            {returned floating point value}
  :boolean;                            {TRUE if integer, FALSE if floating point}
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

procedure escr_write_obuf (            {write line to output file from OBUF}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_write_vstr (            {write var string to current output file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {the string to write}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;
