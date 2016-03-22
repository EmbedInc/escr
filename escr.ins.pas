{   Private include file for all the modules of the PREPIC program.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'pic.ins.pas';

const
  max_symvers_k = 32;                  {max allowed symbol versions of same name}
  max_inclev_k = 32;                   {max include file nesting level per block}
  max_blklev_k = 128;                  {max execution block nesting level}
  max_namelen_k = 80;                  {max characters in symbol name}
  sym_log2buck_k = 7;                  {Log2 buckets in symbol tables}
  lab_log2buck_k = 5;                  {Log2 buckets in local labels tables}
  lab_maxlen_k = 32;                   {maximum length of undecorated local label names}
  string_var_len_k = 1024;             {number of characters a string variable can hold}

  sym_nbuck_k = lshft(1, sym_log2buck_k); {number of buckets in symbol table}
  lab_nbuck_k = lshft(1, lab_log2buck_k); {number of buckets in local labels tables}

type
  inline_p_t = ^inline_t;              {pointer to line within input file}
  inline_pp_t = ^inline_p_t;
  infile_p_t = ^infile_t;              {pointer to one input file stored in memory}
  exblock_p_t = ^exblock_t;            {pointer to info about one nested executable block}
  inh_p_t = ^inh_t;                    {pointer to state for one execution inhibit layer}

  suff_k_t = (                         {input file suffix ID}
    suff_ins_aspic_k,                  {.ins.aspic}
    suff_aspic_k,                      {.aspic}
    suff_ins_dspic_k,                  {.ins.dspic}
    suff_dspic_k,                      {.dspic}
    suff_es_k,                         {.es}
    suff_escr_k);                      {.escr}

  lang_k_t = (                         {input source language ID}
    lang_aspic_k,                      {MPASM}
    lang_dspic_k,                      {ASM30}
    lang_escr_k);                      {Embed script}

  dtype_k_t = (                        {data type ID}
    dtype_bool_k,                      {boolean}
    dtype_int_k,                       {integer}
    dtype_fp_k,                        {floating point}
    dtype_str_k,                       {text string}
    dtype_time_k);                     {absolute time descriptor}

  val_t = record                       {any data value}
    dtype: dtype_k_t;                  {data type}
    case dtype_k_t of                  {different data for each data type}
dtype_bool_k: (                        {boolean}
      bool: boolean;
      );
dtype_int_k: (                         {integer}
      int: sys_int_max_t;
      );
dtype_fp_k: (                          {floating point}
      fp: sys_fp_max_t;
      );
dtype_str_k: (                         {character string}
      str: string_var8192_t;           {may actually be shorter}
      );
dtype_time_k: (                        {absolute time descriptor}
      time: sys_clock_t;
      );
    end;

  sym_k_t = (                          {symbol type}
    sym_var_k,                         {variable}
    sym_const_k,                       {constant}
    sym_subr_k,                        {subroutine}
    sym_macro_k);                      {macro}

  sym_p_t = ^sym_t;
  sym_pp_t = ^sym_p_t;
  sym_t = record                       {all the info about one symbol}
    prev_p: sym_p_t;                   {points to previous (older) symbol of this name}
    next_p: sym_p_t;                   {points to next (newer) symbol of this name}
    name_p: string_var_p_t;            {pointer to symbol name}
    vern: sys_int_machine_t;           {1-N number of symbol with this name}
    scope_p: exblock_p_t;              {points to block sym local in, NIL = global}
    stype: sym_k_t;                    {ID for the type of this symbol}
    case sym_k_t of                    {different fields depending on symbol type}
sym_var_k: (                           {variable}
      var_val: val_t;                  {the variable's data type and current value}
      );
sym_const_k: (                         {constant}
      const_val: val_t;                {the constant's data type and value}
      );
sym_subr_k: (                          {subroutine}
      subr_line_p: inline_p_t;         {points to first line of subroutine definition}
      );
sym_macro_k: (                         {macro}
      macro_line_p: inline_p_t;        {points to first line of macro definition}
      );
    end;

  infile_t = record                    {information about one input file}
    next_p: infile_p_t;                {points to next input file in the list}
    tnam: string_treename_t;           {full treename of the input file}
    lines_p: inline_p_t;               {pointer to first line of file}
    end;

  inline_t = record                    {info about one input file line}
    next_p: inline_p_t;                {pointer to next input line this file, NIL = last}
    file_p: infile_p_t;                {pointer to file this line is from}
    lnum: sys_int_machine_t;           {1-N line number of this line}
    str_p: string_var_p_t;             {pointer to string for this line}
    end;

  sylist_p_t = ^sylist_t;
  sylist_pp_t = ^sylist_p_t;
  sylist_t = record                    {one entry in list of local symbols of a block}
    next_p: sylist_p_t;                {points to next list entry, NIL = last}
    sym_p: sym_p_t;                    {points to symbol local to this block}
    end;

  inpos_p_t = ^inpos_t;
  inpos_t = record                     {one level in current input files position}
    prev_p: inpos_p_t;                 {points to previous level, back there on EOF}
    level: sys_int_machine_t;          {nesting depth, top = 0}
    line_p: inline_p_t;                {points to next input line}
    last_p: inline_p_t;                {points to last input line read}
    end;

  arg_p_t = ^arg_t;
  arg_t = record                       {one argument for a execution block}
    next_p: arg_p_t;                   {points to next sequential arg, NIL at end}
    argn: sys_int_machine_t;           {number of this argument}
    val_p: string_var_p_t;             {points to argument expansion string}
    end;

  looptype_k_t = (                     {type of explicit loop}
    looptype_unc_k,                    {unconditional, no terminating condition}
    looptype_sym_k,                    {looping over list of symbols}
    looptype_for_k);                   {integer variable fixed increment count}

  loop_p_t = ^loop_t;
  loop_t = record                      {info about a loop}
    looptype: looptype_k_t;            {type of loop}
    var_p: sym_p_t;                    {points to the loop variable, if any}
    case looptype_k_t of               {unique data for each type of loop}
looptype_unc_k: (                      {unconditional loop}
      );
looptype_sym_k: (                      {loop over all preprocessor symbols}
      sym_list_p: string_list_p_t;     {points to list of symbol name strings}
      );
looptype_for_k: (                      {loop over integer values with fixed increment}
      for_start: sys_int_max_t;        {starting value}
      for_curr: sys_int_max_t;         {current value}
      for_end: sys_int_max_t;          {ending value}
      for_inc: sys_int_max_t;          {increment per iteration}
      );
    end;

  exblock_k_t = (                      {types of execution block}
    exblock_top_k,                     {top level initial unnested block}
    exblock_sub_k,                     {subroutine}
    exblock_mac_k,                     {macro}
    exblock_blk_k,                     {BLOCK ... ENDBLOCK}
    exblock_loop_k);                   {LOOP ... ENDLOOP}

  exblock_t = record                   {info about one nested execution block}
    prev_p: exblock_p_t;               {pointer to previous execution block, NIL at top}
    level: sys_int_machine_t;          {nesting level, 0 at top}
    start_p: inline_p_t;               {pointer to first line of block definition}
    sym_p: sym_p_t;                    {points to sym for name of this block, if any}
    mem_p: util_mem_context_p_t;       {mem context for block, deleted when block closed}
    arg_p: arg_p_t;                    {points to list of arguments for this block}
    arg_last_p: arg_p_t;               {points to last argument in list}
    nargs: sys_int_machine_t;          {number of arguments in arguments list}
    locsym_p: sylist_p_t;              {points to list of symbols local to this block}
    inpos_p: inpos_p_t;                {points to current nested input file position}
    inh_p: inh_p_t;                    {points to original execution inhibit for this block}
    loop_p: loop_p_t;                  {points to loop definition, NIL = none}
    bltype: exblock_k_t;               {type of execution block}
    loclab: string_hash_handle_t;      {table of local labels, NIL for none, use parent}
    args: boolean;                     {this block takes arguments}
    iter1: boolean;                    {executing first iteration, not subsequent}
    end;

  ifflag_k_t = (                       {flags used for processing IF command}
    ifflag_true_k,                     {condition was TRUE}
    ifflag_nothen_k,                   {THEN command not allowed anymore}
    ifflag_noelse_k);                  {ELSE command not allowed anymore}
  ifflag_t = set of ifflag_k_t;

  inhty_k_t = (                        {types of execution inhibits}
    inhty_root_k,                      {the root inhibit, execution always enabled}
    inhty_blkdef_k,                    {reading block defintion, not executing block}
    inhty_if_k,                        {in IF construct}
    inhty_blk_k);                      {in execution block}

  inh_t = record                       {info about one execution inhibit}
    prev_p: inh_p_t;                   {points to parent inhibit}
    inh: boolean;                      {execution is inhibited}
    inhty: inhty_k_t;                  {type of this inhibit}
    case inhty_k_t of                  {data unique to each inhbit type}
inhty_blkdef_k: (                      {in block definition not executing block}
      blkdef_type: exblock_k_t;        {ID for type of block being defined}
      );
inhty_if_k: (                          {in part of IF construct}
      if_flags: ifflag_t;              {set of operational flags for IF command}
      );
inhty_blk_k: (                         {in execution block}
      blk_p: exblock_p_t;              {points to the execution block}
      );
    end;

  outfile_p_t = ^outfile_t;
  outfile_t = record                   {info about one nested output file}
    prev_p: outfile_p_t;               {points to previous output file, NIL at root}
    conn: file_conn_t;                 {connection to this output file}
    end;

var (com)                              {state visible to all modules}
  out_p: outfile_p_t;                  {points to current output file info, NIL = none}
  nflags: sys_int_machine_t;           {total number of flags bits created}
  flag_byten: sys_int_machine_t;       {number of flag bytes (words on PIC 30) created}
  flag_bitn: sys_int_machine_t;        {0-N bit number of next flag within flag byte/word}
  sym: string_hash_handle_t;           {symbol table}
  mem_sytable_p: util_mem_context_p_t; {pointer to mem context for global symbol table}
  mem_sym_p: util_mem_context_p_t;     {pointer to mem context for global symbol data}
  mem_top_p: util_mem_context_p_t;     {points to general mem context for top level data}
  files_p: infile_p_t;                 {points to list of input files}
  exblock_p: exblock_p_t;              {points to info about current execution block}
  inhibit_p: inh_p_t;                  {points to current execution inhibit info}
  lang: lang_k_t;                      {input file language ID}
  labeln: sys_int_conv32_t;            {sequential number for next unique label}
  cmd: string_var32_t;                 {name of current command, upper case}
  ibuf: string_var8192_t;              {current input line after function expansions}
  ip: string_index_t;                  {IBUF parse index}
  lparm: string_var8192_t;             {last parameter parsed from input line}
  obuf: string_var8192_t;              {one line output buffer}
{
*   Subroutines that implement the individual preprocessor commands.  Each
*   of these have the same interface.  See the header comments in module
*   PREPIC_CMD for the details.
}
procedure prepic_cmd_block (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_call (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_const (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_del (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_else (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_endblock (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_endif (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_endloop (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_endmac (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_endsub (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_flag (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_if (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_inbit (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_include (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_loop (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_macro (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_outbit (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_quit (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_quitmac (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_repeat (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_return (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_set (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_show (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_subroutine (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_sylist (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_then (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_var (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_write (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_writeend (
  out     stat: sys_err_t);
  val_param; extern;

procedure prepic_cmd_writeto (
  out     stat: sys_err_t);
  val_param; extern;
{
*
*   Other entry points.
}
procedure close_out (                  {close the current output file, pop previous}
  in      del: boolean);               {delete the file}
  val_param; extern;

procedure close_out_all (              {close all output files}
  in      del: boolean);               {delete the files}
  val_param; extern;

procedure err_atline (                 {show error followed by source line number}
  in      subsys: string;              {name of subsystem, used to find message file}
  in      msg: string;                 {message name within subsystem file}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn, extern);

procedure err_atline_abort (           {bomb with msg and source line on error}
  in      stat: sys_err_t;             {error code, nothing done if no error}
  in      subsys: string;              {subsystem name of caller's message}
  in      msg: string;                 {name of caller's message within subsystem}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  val_param; extern;

procedure err_check_symname (          {abort with error on invalid symbol name}
  in      name: univ string_var_arg_t); {symbol name to check}
  val_param; extern;

procedure err_dtype_unimp (            {unimplemented data type internal error}
  in      dtype: dtype_k_t;            {unimplemented data type}
  in      routine: string);            {name of the routine where data type unimplemented}
  options (val_param, noreturn, extern);

procedure err_lang (                   {unexpected input language identifier}
  in      lang: lang_k_t;              {the language identifier}
  in      module: string;              {source module name where error encountered}
  in      checkpoint: sys_int_machine_t); {unique number for this occurrence}
  options (val_param, noreturn, extern);

procedure err_sym_not_found (          {symbol not found}
 in      name: univ string_var_arg_t); {symbol name that was not found}
  options (val_param, noreturn, extern);

procedure err_parm_bad (               {bomb with bad parameter to command error}
  in      parm: univ string_var_arg_t); {the offending parameter}
  options (val_param, noreturn, extern);

procedure err_parm_last_bad;           {last parameter parsed was bad}
  options (val_param, noreturn, extern);

procedure err_parm_missing (           {a required command parameter not found}
  in      subsys: string;              {name of subsystem, used to find message file}
  in      msg: string;                 {message name withing subsystem file}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn, extern);

procedure err_val (                    {show value and data type of offending value}
  in      val: val_t);                 {the value}
  val_param; extern;

procedure exblock_arg_add (            {add argument to current block}
  in      str: univ string_var_arg_t); {argument string}
  val_param; extern;

procedure exblock_arg_addn (           {add argument to current block, specific number}
  in      str: univ string_var_arg_t;  {argument string}
  in      n: sys_int_machine_t);       {argument number}
  val_param; extern;

procedure exblock_arg_get (            {get value of currently visible argument}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param; extern;

procedure exblock_arg_get_bl (         {get value of argument to specific block}
  in      bl: exblock_t;               {execution block to get argument of}
  in      n: sys_int_machine_t;        {1-N sequential argument number}
  out     val_p: string_var_p_t);      {pointer to argument value, NIL if not exist}
  val_param; extern;

procedure exblock_close;               {close curr execution block and delete temp state}
  val_param; extern;

procedure exblock_inline_push (        {push new source line location for exec block}
  in      line_p: inline_p_t);         {pointer to next input line to use}
  val_param; extern;

procedure exblock_inline_set (         {go to new input source position in curr block}
  in      line_p: inline_p_t);         {pointer to next input line to use}
  val_param; extern;

procedure exblock_loclab_init;         {create and init local symbols list in this block}
  val_param; extern;

procedure exblock_new;                 {create and install new execution block}
  val_param; extern;

procedure exblock_quit;                {stop executing in the current block}
  val_param; extern;

procedure exblock_repeat;              {loop back to start of block}
  val_param; extern;

procedure format_fp (                  {create specifically formatted floating point string}
  in      fp: sys_fp_max_t;            {input floating point value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned floating point string}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure format_int (                 {create specifically formatted integer string}
  in      i: sys_int_max_t;            {input integer value}
  in      fmt: univ string_var_arg_t;  {format string}
  in out  s: univ string_var_arg_t;    {returned integer string}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure get_args_str (               {get string representation of remaining parameters}
  in out  str: univ string_var_arg_t); {concatenation of remaining args converted to strings}
  val_param; extern;

function get_bool (                    {get next parameter as boolean}
  out     b: boolean)                  {returned boolean value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function get_dtype (                   {get next parameter as data type ID}
  out     dtype: dtype_k_t)            {returned data type ID}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

procedure get_end;                     {make sure no more tokens left on input line}
  val_param; extern;

function get_fp (                      {get next parameter as floating point}
  out     fp: sys_fp_max_t)            {returned floating point value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function get_int (                     {get next parameter as an integer}
  out     i: sys_int_max_t)            {returned integer value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function get_time (                    {get the next token as a time value}
  out     time: sys_clock_t)           {returned time value}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

procedure get_keyword (                {get next parameter as keyword in a list}
  in      klist: string;               {keywords, upper case separated by blanks}
  out     pick: sys_int_machine_t);    {1-N keyword number, 0 = no token available}
  val_param; extern;

function get_str (                     {get string representation of next parameter}
  in out  str: univ string_var_arg_t)  {returned string}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function get_tkraw (                   {get next raw input stream token}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function get_tkrawc (                  {get next raw token, comma delimited}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function get_token (                   {get next input stream token}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function get_val (                     {get value of next input stream token}
  out     val: val_t)                  {returned value}
  :boolean;                            {TRUE if token available}
  val_param; extern;

function get_val_dtype (               {get next input value, to dtype of VAL}
  in out  val: val_t)                  {returned value, DTYPE specifies data type}
  :boolean;                            {TRUE if token available}
  val_param; extern;

function infile_getline (              {get next input stream source line}
  in out  str_p: string_var_p_t)       {returned pointer to source line}
  :boolean;                            {TRUE if returning with line, FALSE for end input}
  val_param; extern;

procedure infile_open (                {get input file descriptor for input file}
  in      fnam: univ string_var_arg_t; {file name}
  out     infile_p: infile_p_t;        {returned pointer to input file descriptor}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure infile_skipline;             {skip next input file line}
  val_param; extern;

procedure inh_new;                     {create new execution inhibit}
  val_param; extern;

procedure inh_end;                     {end the current execution inhibit}
  val_param; extern;

procedure inline_expand_line (         {expand all inline functions of a line}
  in      lin: univ string_var_arg_t;  {input line, may contain inline functions}
  out     lot: string_var8192_t);      {output line, contains no inline functions}
  val_param; extern;

procedure inline_func_init;            {one-time init for processing inline funcs}
  val_param; extern;

procedure inline_func (                {perform inline function operation}
  in      fstr: univ string_var_arg_t; {function source string, brackets removed}
  in out  lot: string_var8192_t);      {string to append function expansion to}
  val_param; extern;

procedure loclab_get (                 {get expansion of generic local label name}
  in      name: univ string_var_arg_t; {generic local label name}
  in out  exp: univ string_var_arg_t); {returned full label name (expansion)}
  val_param; extern;

function loop_iter                     {advance to next loop iteration}
  :boolean;                            {looped back, not terminated}
  val_param; extern;

function macro_run (                   {run macro if present on curr input line}
  out     stat: sys_err_t)             {completion status}
  :boolean; extern;                    {macro was processed}

procedure show_obuf;                   {write line to standard output from OBUF}
  val_param; extern;

procedure str_from_time (              {make string from absolute time descriptor}
  in      time: sys_clock_t;           {input absolute time descriptor}
  in out  s: univ string_var_arg_t);   {returned string representation of the time}
  val_param; extern;

procedure str_from_fp (                {make string from floating point value}
  in      fp: double;                  {floating point input value}
  in out  s: univ string_var_arg_t);   {returned string representation}
  val_param; extern;

function str_to_time (                 {make absolute time descriptor from string}
  in      s: univ string_var_arg_t;    {input string}
  out     time: sys_clock_t)           {returned time descriptor}
  :boolean;                            {TRUE on success}
  val_param; extern;

procedure sym_del (                    {delete specific symbol version}
  in out  sym_p: sym_p_t);             {pointer to symbol to delete, returned NIL}
  val_param; extern;

procedure sym_del_name (               {delete symbol by name}
  in      name: univ string_var_arg_t); {symbol name}
  val_param; extern;

procedure sym_find (                   {look up symbol in symbol table}
  in      name: univ string_var_arg_t; {symbol name}
  out     sym_p: sym_p_t);             {returned pointer to symbol, NIL if not found}
  val_param; extern;

function sym_name (                    {check for valid symbol name}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
  val_param; extern;

procedure sym_new (                    {create new symbol}
  in      name: univ string_var_arg_t; {symbol name}
  in      sz: sys_int_adr_t;           {size of the whole symbol descriptor}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: sym_p_t);             {returned pointer to symbol info}
  val_param; extern;

procedure sym_new_const (              {create new symbol for a constant}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: dtype_k_t;            {data type of the constant}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: sym_p_t);             {returned pointer to symbol info}
  val_param; extern;

procedure sym_new_var (                {create new symbol for a variable}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: dtype_k_t;            {data type of the variable}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: sym_p_t);             {returned pointer to symbol info}
  val_param; extern;

function term_get (                    {get value of next term in list}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index}
  out     val: val_t)                  {returned value of the term}
  :boolean;                            {TRUE if term was available}
  val_param; extern;

procedure uptocomm (                   {find line length without comment}
  in      s: univ string_var_arg_t;    {the input string}
  out     nclen: string_index_t);      {string length with comment removed}
  val_param; extern;

procedure val_copy (                   {copy and convert value to target data type}
  in      ival: val_t;                 {the input value}
  out     oval: val_t);                {output val, must be set up except actual data}
  val_param; extern;

function val_bool (                    {convert to boolean value or bomb with error}
  in      val: val_t)                  {source value}
  :boolean;                            {the boolean value of VAL}
  val_param; extern;

function val_fp (                      {convert to FP value or bomb with error}
  in      val: val_t)                  {source value}
  :sys_fp_max_t;                       {floating point value of VAL}
  val_param; extern;

procedure val_init (                   {initialize full VAL_T descriptor to data type}
  in      dtype: dtype_k_t;            {data type to set up VAL for}
  out     val: val_t);                 {full value descriptor to initialize}
  val_param; extern;

function val_int (                     {convert to integer value or bomb with error}
  in      val: val_t)                  {source value}
  :sys_int_max_t;                      {integer value of VAL}
  val_param; extern;

function val_time (                    {convert to time value or bomb with error}
  in      val: val_t)                  {source value}
  :sys_clock_t;                        {time value of VAL}
  val_param; extern;

function val_isbool (                  {check for VAL can be converted to boolean}
  in      val: val_t;                  {input value to check}
  out     b: boolean)                  {boolean value, unaltered if VAL not boolean}
  :boolean;                            {TRUE if returning boolean value in B}
  val_param; extern;

function val_isint (                   {distinguish between INT or FP, error if neither}
  in      val: val_t;                  {the source value}
  out     vi: sys_int_max_t;           {returned integer value, if integer}
  out     vf: sys_fp_max_t)            {returned floating point value}
  :boolean;                            {TRUE if integer, FALSE if floating point}
  val_param; extern;

function val_size (                    {return minimum required size of VAL_T structure}
  in      dtype: dtype_k_t;            {data type of the value}
  in      len: sys_int_machine_t)      {max string chars to hold, ignored other dtypes}
  :sys_int_adr_t;
  val_param; extern;

procedure val_str (                    {convert value to its PREPIC string representation}
  in      val: val_t;                  {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param; extern;

procedure val_text (                   {make output language text representation}
  in      val: val_t;                  {the source value}
  in out  str: univ string_var_arg_t); {returned string}
  val_param; extern;

procedure write_obuf;                  {write line to output file from OBUF}
  val_param; extern;

procedure write_vstr (                 {write var string to current output file}
  in      s: univ string_var_arg_t;    {the string to write}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;
