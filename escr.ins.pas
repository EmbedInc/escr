{   Application interface to the Embed Inc scripting engine, ESCR.
}
const
{
*   Fixed configuration parameters.
}
  escr_max_symvers_k = 32;             {max allowed symbol versions of same name}
  escr_max_inclev_k = 32;              {max include file nesting level per block}
  escr_max_blklev_k = 128;             {max execution block nesting level}
  escr_max_namelen_k = 80;             {max characters in symbol name}
  escr_sym_log2buck_k = 7;             {Log2 buckets in symbol tables}
  escr_ulab_log2buck_k = 5;            {Log2 buckets in unique labels tables}
  escr_ulab_maxlen_k = 32;             {maximum length of undecorated unique label names}
  escr_string_var_len_k = 1024;        {number of characters a string variable can hold}
{
*   Status codes.
}
  escr_subsys_k = -62;                 {ESCR subsystem ID}

  escr_err_nomcontext_k = 1;           {unable to allocate dynamic memory context}
  escr_err_nomem_k = 2;                {unable to allocate dynamic memory}
  escr_err_sytoomany_k = 3;            {max versions of symbol already exist}
  escr_err_nfsym_k = 4;                {symbol not found, <name>}
  escr_err_inend_k = 5;                {end of input before end of block}
  escr_err_cmdbad_k = 6;               {unrecognized command}
  escr_err_notcmd_k = 7;               {symbol is not a command}
  escr_err_nflab_k = 8;                {label not found}
  escr_err_notlab_k = 9;               {symbol is not a label}
  escr_err_nestblc_k = 10;             {in nested input on block close}
  escr_err_noutwr_k = 11;              {no output file on attempt to write to it}
  escr_err_noutcl_k = 12;              {no output file on attempt to close it}
  escr_err_topoutcl_k = 13;            {top output file on attempt to close it}
  escr_err_badinline_k = 14;           {bad input line}
  escr_err_nosystart_k = 15;           {no syntax range start characters}
  escr_err_exclnend_k = 16;            {syntax exclusion not ended}
  escr_err_scommnend_k = 17;           {script comment not ended}
  escr_err_funcnend_k = 18;            {function not ended}
  escr_err_funcnfnd_k = 19;            {function not found}
  escr_err_notfunc_k = 20;             {symbol is not a function}
  escr_err_notval_k = 21;              {symbol does not have a value}
  escr_err_termbad_k = 22;             {string is not a valid term}
  escr_err_missingparm_k = 23;         {missing cmd parameter, <cmd>}
  escr_err_notbool_k = 24;             {not convertable to BOOL type, <term>}
  escr_err_notint_k = 25;              {not convertable to INTEGER type, <term>}
  escr_err_nottime_k = 26;             {not convertable to TIME type, <term>}
  escr_err_notfp_k = 27;               {not convertable to REAL type, <term>}
  escr_err_notintfp_k = 28;            {not convertable to INTEGER or REAL type, <term>}
  escr_err_badparm_k = 29;             {bad parameter, <cmd name> <parm>}
  escr_err_baddtype_k = 30;            {bad data type name, <parm>}
  escr_err_extra_k = 31;               {too many parameters, <first extra>}
  escr_err_not1char_k = 32;            {string not 1 char long, <string>}
  escr_err_noparmfun_k = 33;           {missing function parameter, <funcname>}
  escr_err_exparmfun_k = 34;           {extra function parm, <parm> <funcname>}
  escr_err_badtype_k = 35;             {term has bad data type, <term>}
  escr_err_div0_k = 36;                {attempt to divide by 0}
  escr_err_sqrtneg_k = 37;             {attempt to take square root of negative value}
  escr_err_negexpnint_k = 38;          {non-integer exponent to negative value}
  escr_err_lognpos_k = 39;             {logarithm of non-positive value}
  escr_err_afdate_k = 40;              {date-altering arg after date set, <argname>}
  escr_err_badparmfun_k = 41;          {bad function parameter, <parm> <funcname>}
  escr_err_sym_nvar_k = 42;            {symbol is not a variable, <name>}
  escr_err_endblock_root_k = 43;       {trying to end root block, <cmd>}
  escr_err_endblock_type_k = 44;       {wrong block type, <cmd>}
  escr_err_endblock_include_k = 45;    {trying to end block in include file, <cmd>}
  escr_err_notinif_k = 46;             {not in IF block}
  escr_err_then2_k = 47;               {second THEN command}
  escr_err_else2_k = 48;               {second ELSE command}
  escr_err_nothenelse_k = 49;          {no THEN or ELSE in IF block}
  escr_err_loopinc0_k = 50;            {loop iteration increment = 0}
  escr_err_notloop_k = 51;             {not in LOOP block}
  escr_err_notmacro_k = 52;            {not in MACRO block}
  escr_err_notsubdef_k = 53;           {not in subroutine definition}
  escr_err_sym_nsub_k = 54;            {symbol is not a subroutine, <name>}
  escr_err_notsub_k = 55;              {not in a subroutine}
  escr_err_bltoomany_k = 56;           {block nesting too deep, <level>}
  escr_err_inftoomany_k = 57;          {input file nesting too deep, <level>}
  escr_err_fmtint_k = 58;              {bad integer format string keyword, <name>}
  escr_err_fmtfp_k = 59;               {bad FP format string keyword, <name>}
  escr_err_inh_nest_k = 60;            {inhibit nesting error}
  escr_err_if_nend_k = 61;             {IF not ended before end of execution block}
  escr_err_badsym_k = 62;              {not a valid symbol name, <name>}
{
*   Derived constants.
}
  escr_sym_nbuck_k =                   {number of buckets in symbol tables}
    lshft(1, escr_sym_log2buck_k);
  escr_ulab_nbuck_k =                  {number of buckets in unique labels tables}
    lshft(1, escr_ulab_log2buck_k);

type
  escr_inline_p_t = ^escr_inline_t;    {pointer to line within input file}
  escr_inline_pp_t = ^escr_inline_p_t;
  escr_exblock_p_t = ^escr_exblock_t;  {pointer to info about one nested executable block}
  escr_inh_p_t = ^escr_inh_t;          {pointer to state for one execution inhibit layer}
  escr_p_t = ^escr_t;                  {pointer to ESCR system use state}

  escr_dtype_k_t = (                   {data type ID}
    escr_dtype_bool_k,                 {boolean}
    escr_dtype_int_k,                  {integer}
    escr_dtype_fp_k,                   {floating point}
    escr_dtype_str_k,                  {text string}
    escr_dtype_time_k);                {absolute time descriptor}

  escr_val_t = record                  {any data value}
    dtype: escr_dtype_k_t;             {data type}
    case escr_dtype_k_t of             {different data for each data type}
escr_dtype_bool_k: (                   {boolean}
      bool: boolean;
      );
escr_dtype_int_k: (                    {integer}
      int: sys_int_max_t;
      );
escr_dtype_fp_k: (                     {floating point}
      fp: sys_fp_max_t;
      );
escr_dtype_str_k: (                    {character string}
      str: string_var8192_t;           {may actually be shorter}
      );
escr_dtype_time_k: (                   {absolute time descriptor}
      time: sys_clock_t;
      );
    end;

  escr_infile_p_t = ^escr_infile_t;
  escr_infile_t = record               {information about one input file or snippet thereof}
    next_p: escr_infile_p_t;           {points to next input file in the list}
    tnam: string_treename_t;           {full treename of the input file}
    lfirst_p: escr_inline_p_t;         {pointer to first line of snippet}
    llast_p: escr_inline_p_t;          {pointer to last line of snippet}
    end;

  escr_inline_t = record               {info about one input file line}
    next_p: escr_inline_p_t;           {pointer to next input line this file, NIL = last}
    file_p: escr_infile_p_t;           {pointer to file this line is from}
    lnum: sys_int_machine_t;           {1-N line number of this line}
    str_p: string_var_p_t;             {pointer to string for this line}
    end;

  escr_inpos_p_t = ^escr_inpos_t;
  escr_inpos_t = record                {one level in current input files position}
    prev_p: escr_inpos_p_t;            {points to previous level, back there on EOF}
    level: sys_int_machine_t;          {nesting level within block, top = 0}
    line_p: escr_inline_p_t;           {points to next input line}
    last_p: escr_inline_p_t;           {points to last input line read}
    end;

  escr_sytable_p_t = ^escr_sytable_t;
  escr_sytable_t = record              {symbol table}
    mem_p: util_mem_context_p_t;       {top mem context, used for symbol data directly}
    hash: string_hash_handle_t;        {handle to symbol names hash table}
    end;

  escr_sym_k_t = (                     {symbol type}
    escr_sym_var_k,                    {variable}
    escr_sym_const_k,                  {constant}
    escr_sym_subr_k,                   {subroutine, defined by user code}
    escr_sym_isubr_k,                  {intrinsic subroutine, compiled code}
    escr_sym_cmd_k,                    {command, defined by user code}
    escr_sym_icmd_k,                   {intrinsic command, compiled routine}
    escr_sym_func_k,                   {function, defined by user code}
    escr_sym_ifunc_k,                  {intrinsic function, compiled routine}
    escr_sym_macro_k,                  {macro, defined by user code}
    escr_sym_imacro_k,                 {intrinsic macro, compiled routine}
    escr_sym_label_k,                  {label for a specific input files line}
    escr_sym_src_k);                   {label for a source code snippet}

  escr_instr_t = record                {input string to be parsed}
    p: string_index_t;                 {1-N string string parse index}
    s: string_var8192_t;               {the string}
    end;

  escr_isubr_p_t = ^procedure (        {template for subroutine implemented by compiled code}
    in    e_p: escr_p_t;               {points to state for this use of the ESCR system}
    out   stat: sys_err_t);            {completion status}
    val_param;

  escr_icmd_p_t = ^procedure (         {template for compiled command routine}
    in    e_p: escr_p_t;               {points to state for this use of the ESCR system}
    out   stat: sys_err_t);            {completion status}
    val_param;

  escr_ifunc_p_t = ^procedure (        {template for compiled function routine}
    in    e_p: escr_p_t;               {points to state for this use of the ESCR system}
    out   stat: sys_err_t);            {completion status}
    val_param;

  escr_imacro_p_t = ^procedure (       {template for compiled macro routine}
    in    e_p: escr_p_t;               {points to state for this use of the ESCR system}
    out   stat: sys_err_t);            {completion status}
    val_param;

  escr_sym_p_t = ^escr_sym_t;
  escr_sym_pp_t = ^escr_sym_p_t;
  escr_sym_t = record                  {all the info about one symbol}
    table_p: escr_sytable_p_t;         {points to symbol table this symbol is in}
    prev_p: escr_sym_p_t;              {points to previous (older) symbol of this name}
    next_p: escr_sym_p_t;              {points to next (newer) symbol of this name}
    name_p: string_var_p_t;            {pointer to symbol name}
    vern: sys_int_machine_t;           {1-N number of symbol with this name}
    scope_p: escr_exblock_p_t;         {points to block sym local in, NIL = global}
    stype: escr_sym_k_t;               {ID for the type of this symbol}
    case escr_sym_k_t of               {different fields depending on symbol type}
escr_sym_var_k: (                      {variable}
      var_val: escr_val_t;             {the variable's data type and current value}
      );
escr_sym_const_k: (                    {constant}
      const_val: escr_val_t;           {the constant's data type and value}
      );
escr_sym_subr_k: (                     {subroutine defined by user code}
      subr_line_p: escr_inline_p_t;    {points to first line of subroutine definition}
      );
escr_sym_isubr_k: (                    {intrinsic subroutine, implemented by compiled code}
      isubr_p: escr_isubr_p_t;         {points to routine that implements the subroutine}
      );
escr_sym_macro_k: (                    {macro}
      macro_line_p: escr_inline_p_t;   {points to first line of macro definition}
      );
escr_sym_imacro_k: (                   {intrinsic macro, implemented by compiled routine}
      imacro_p: escr_imacro_p_t;       {pointer to routine that implements the macro}
      );
escr_sym_func_k: (                     {function defined by user code}
      func_line_p: escr_inline_p_t;    {points to first line of function definition}
      );
escr_sym_ifunc_k: (                    {intrisic function, implemented by compiled routine}
      ifunc_p: escr_ifunc_p_t;         {pointer to routine that implements the function}
      );
escr_sym_cmd_k: (                      {command defined by user code}
      cmd_line_p: escr_inline_p_t;     {points to first line of command definition}
      );
escr_sym_icmd_k: (                     {intrisic command, implemented by compiled routine}
      icmd_p: escr_icmd_p_t;           {pointer to routine that implements the command}
      );
escr_sym_label_k: (                    {label for specific line in input files}
      label_line_p: escr_inline_p_t;   {points to first line of command definition}
      );
escr_sym_src_k: (                      {label for a source code snippet}
      src_p: escr_infile_p_t;          {pointer to source code snippet}
      );
    end;

  escr_sylist_p_t = ^escr_sylist_t;
  escr_sylist_pp_t = ^escr_sylist_p_t;
  escr_sylist_t = record               {one entry in list of local symbols of a block}
    next_p: escr_sylist_p_t;           {points to next list entry, NIL = last}
    sym_p: escr_sym_p_t;               {points to symbol local to this block}
    end;

  escr_arg_p_t = ^escr_arg_t;
  escr_arg_t = record                  {one argument for a execution block}
    next_p: escr_arg_p_t;              {points to next sequential arg, NIL at end}
    argn: sys_int_machine_t;           {number of this argument}
    val_p: string_var_p_t;             {points to argument expansion string}
    end;

  escr_looptype_k_t = (                {type of explicit loop}
    escr_looptype_unc_k,               {unconditional, no terminating condition}
    escr_looptype_sym_k,               {looping over list of symbols}
    escr_looptype_for_k);              {integer variable fixed increment count}

  escr_loop_p_t = ^escr_loop_t;
  escr_loop_t = record                 {info about a loop}
    looptype: escr_looptype_k_t;       {type of loop}
    var_p: escr_sym_p_t;               {points to the loop variable, if any}
    case escr_looptype_k_t of          {unique data for each type of loop}
escr_looptype_unc_k: (                 {unconditional loop}
      );
escr_looptype_sym_k: (                 {loop over all script processor symbols}
      sym_list_p: string_list_p_t;     {points to list of symbol name strings}
      );
escr_looptype_for_k: (                 {loop over integer values with fixed increment}
      for_start: sys_int_max_t;        {starting value}
      for_curr: sys_int_max_t;         {current value}
      for_end: sys_int_max_t;          {ending value}
      for_inc: sys_int_max_t;          {increment per iteration}
      );
    end;

  escr_exblock_k_t = (                 {types of execution block}
    escr_exblock_top_k,                {top level initial unnested block}
    escr_exblock_blk_k,                {BLOCK ... ENDBLOCK}
    escr_exblock_loop_k,               {LOOP ... ENDLOOP}
    escr_exblock_sub_k,                {subroutine}
    escr_exblock_cmd_k,                {command}
    escr_exblock_func_k,               {function}
    escr_exblock_mac_k);               {macro}

  escr_exblock_t = record              {info about one nested execution block}
    prev_p: escr_exblock_p_t;          {pointer to previous execution block, NIL at top}
    level: sys_int_machine_t;          {nesting level, 0 at top}
    start_p: escr_inline_p_t;          {pointer to first line of block definition}
    sym_p: escr_sym_p_t;               {points to sym for name of this block, if any}
    mem_p: util_mem_context_p_t;       {mem context for block, deleted when block closed}
    arg_p: escr_arg_p_t;               {points to list of arguments for this block}
    arg_last_p: escr_arg_p_t;          {points to last argument in list}
    nargs: sys_int_machine_t;          {number of arguments in arguments list}
    locsym_p: escr_sylist_p_t;         {points to list of symbols local to this block}
    inpos_p: escr_inpos_p_t;           {points to current nested input file position}
    previnh_p: escr_inh_p_t;           {points to previous inhibit before this block}
    loop_p: escr_loop_p_t;             {points to loop definition, NIL = none}
    bltype: escr_exblock_k_t;          {type of execution block}
    ulab: string_hash_handle_t;        {table of local labels, NIL for none, use parent}
    args: boolean;                     {this block takes arguments}
    iter1: boolean;                    {executing first iteration, not subsequent}
    end;

  escr_ifflag_k_t = (                  {flags used for processing IF command}
    escr_ifflag_true_k,                {condition was TRUE}
    escr_ifflag_nothen_k,              {THEN command not allowed anymore}
    escr_ifflag_noelse_k);             {ELSE command not allowed anymore}
  escr_ifflag_t = set of escr_ifflag_k_t;

  escr_inhty_k_t = (                   {types of execution inhibits}
    escr_inhty_root_k,                 {the root inhibit, execution always enabled}
    escr_inhty_blkdef_k,               {reading block defintion, not executing block}
    escr_inhty_if_k,                   {in IF construct}
    escr_inhty_blk_k);                 {in execution block}

  escr_inh_t = record                  {info about one execution inhibit}
    prev_p: escr_inh_p_t;              {points to parent inhibit}
    inh: boolean;                      {execution is inhibited}
    inhty: escr_inhty_k_t;             {type of this inhibit}
    case escr_inhty_k_t of             {data unique to each inhbit type}
escr_inhty_blkdef_k: (                 {in block definition not executing block}
      blkdef_type: escr_exblock_k_t;   {ID for type of block being defined}
      );
escr_inhty_if_k: (                     {in part of IF construct}
      if_flags: escr_ifflag_t;         {set of operational flags for IF command}
      );
escr_inhty_blk_k: (                    {in execution block}
      blk_p: escr_exblock_p_t;         {points to the execution block}
      );
    end;

  escr_outfile_p_t = ^escr_outfile_t;
  escr_outfile_t = record              {info about one nested output file}
    prev_p: escr_outfile_p_t;          {points to previous output file, NIL at root}
    conn: file_conn_t;                 {connection to this output file}
    end;

  escr_flag_k_t = (                    {individual system-wide flags}
    escr_flag_preproc_k);              {preprocessor, non-commands written to out file}
  escr_flags_t = set of escr_flag_k_t; {all the flags in one word}

  escr_syrange_p_t = ^escr_syrange_t;
  escr_syrange_t = record              {identification for special syntax range}
    st: string_var4_t;                 {characters to start}
    en: string_var4_t;                 {characters to end, EOL when empty}
    end;

  escr_syrlist_p_t = ^escr_syrlist_t;
  escr_syrlist_t = record              {list of special syntax ranges}
    next_p: escr_syrlist_p_t;          {points to next list entry}
    range: escr_syrange_t;             {start and end sequences for this range}
    end;

  escr_syfunc_st_p_t = ^function (     {template for app routine to detect function start}
    in    e_p: escr_p_t;               {points to state for this use of the ESCR system}
    in    lin: string_var_arg_t;       {input line possibly containing function}
    in out p: string_index_t;          {LIN parse index, after func start iff found}
    out   stat: sys_err_t)             {completion status}
    :boolean;                          {function start found, P moved to immediately after}
    val_param;

  escr_t = record                      {state for one use of the ESCR system}
    mem_p: util_mem_context_p_t;       {top mem context for all dynamic mem}
    sym_var: escr_sytable_t;           {symbol table for variables and constants}
    sym_sub: escr_sytable_t;           {symbol table for subroutines}
    sym_mac: escr_sytable_t;           {symbol table for macros}
    sym_fun: escr_sytable_t;           {symbol table for functions}
    sym_cmd: escr_sytable_t;           {symbol table for commands}
    sym_lab: escr_sytable_t;           {symbol table for input file line labels}
    sym_src: escr_sytable_t;           {symbol table for input file source snippets}
    files_p: escr_infile_p_t;          {points to list of input files}
    ibuf: string_var8192_t;            {current input line after function expansions}
    funarg: escr_instr_t;              {function and arguments with parse state}
    funame: string_var80_t;            {name of function currently being executed}
    funret: string_var8192_t;          {function return string}
    ip: string_index_t;                {IBUF parse index}
    lparm: string_var8192_t;           {last parameter parsed from input line}
    cmd: string_var32_t;               {name of current command}
    exblock_p: escr_exblock_p_t;       {points to info about current execution block}
    inhroot: escr_inh_t;               {root execution inhibit, always enabled}
    inhibit_p: escr_inh_p_t;           {points to current execution inhibit}
    out_p: escr_outfile_p_t;           {points to current output file info, NIL = none}
    obuf: string_var8192_t;            {one line output buffer}
    ulabn: sys_int_conv32_t;           {sequential number for next unique label}
    incsuff: string;                   {allowed suffixes for include file names}
    cmdst: string_var4_t;              {special characters to start a command}
    syexcl_p: escr_syrlist_p_t;        {list of syntax exclusions (like quotes)}
    commscr_p: escr_syrlist_p_t;       {list of script engine comment identifiers}
    commdat_p: escr_syrlist_p_t;       {list of data file comments, preproc mode only}
    syfunc: escr_syrange_t;            {start/end syntax for script functions}
    syfunc_st_p: escr_syfunc_st_p_t;   {app routine to identify function start}
    flags: escr_flags_t;               {system-wide control flags}
    end;
{
*   Routines to help implement intrinsic commands.
}
procedure escr_get_args_str (          {get string representation of remaining parameters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {concatenation of remaining args converted to strings}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_get_bool (               {get next parameter as boolean}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean;                  {returned boolean value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_dtype (              {get next parameter as data type ID}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     dtype: escr_dtype_k_t;       {returned data type ID}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_end (                {check for no more tokens on input line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {set to error if token found}
  :boolean;                            {no more tokens on the input line}
  val_param; extern;

function escr_get_fp (                 {get next parameter as floating point}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     fp: sys_fp_max_t;            {returned floating point value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_int (                {get next parameter as an integer}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     i: sys_int_max_t;            {returned integer value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_time (               {get the next token as a time value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     time: sys_clock_t;           {returned time value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

procedure escr_get_keyword (           {get one of list of keywords from input stream}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      klist: string;               {keywords, upper case separated by blanks}
  out     pick: sys_int_machine_t;     {1-N keyword number, 0 = no token, -1 = no match}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_get_str (                {get string representation of next parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned string}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if input token was available}
  val_param; extern;

function escr_get_tkraw (              {get next raw input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token, strings not unquoted}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function escr_get_token (              {get next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     tk: univ string_var_arg_t)   {the returned token}
  :boolean;                            {TRUE if token was available}
  val_param; extern;

function escr_get_val (                {get value of next input stream token}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t;             {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if token available}
  val_param; extern;

function escr_get_val_dtype (          {get next input value, to dtype of VAL}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  val: escr_val_t;             {returned value, DTYPE specifies data type}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {TRUE if token available}
  val_param; extern;

procedure escr_stat_cmd_noarg (        {missing command argument}
  in      e: escr_t;                   {state for this use of the ESCR system}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param; extern;

procedure escr_write_vstr (            {write var string to current output file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      s: univ string_var_arg_t;    {the string to write}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;
{
*   Routines to help implement intrinsic functions.
}
procedure escr_ifn_bad_keyw (          {set STAT for bad keyword}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      keyw: univ string_var_arg_t; {the bad keyword or string}
  out     stat: sys_err_t);            {returned error status}
  val_param; extern;

procedure escr_ifn_bad_dtype (         {set STAT for bad data type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t;             {value with the wrong data type}
  out     stat: sys_err_t);            {returned error status}
  val_param; extern;

procedure escr_ifn_stat_required (     {set STAT according to missing required argument}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_ifn_get_bool (           {get boolean value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     b: boolean;                  {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with boolean value}
  val_param; extern;

function escr_ifn_get_fp (             {get floating point value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     fp: sys_fp_max_t;            {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with floating point value}
  val_param; extern;

function escr_ifn_get_int (            {get integer value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     ii: sys_int_max_t;           {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with integer value}
  val_param; extern;

function escr_ifn_get_keyw (           {get next parameter as upper case keyword}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  keyw: univ string_var_arg_t; {returned keyword, upper case}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with a keyword}
  val_param; extern;

function escr_ifn_get_str (            {get string value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with string value}
  val_param; extern;

procedure escr_ifn_get_strs (          {get string of all remaining parameters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  str: univ string_var_arg_t;  {returned string}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_ifn_get_time (           {get time value of next function parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     t: sys_clock_t;              {returned value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with time value}
  val_param; extern;

function escr_ifn_get_val (            {get arbitrary value of next func parameter}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     val: escr_val_t;             {returned term value}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {term was available}
  val_param; extern;

procedure escr_ifn_ret_bool (          {return boolean value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      b: boolean);                 {the boolean value to return}
  val_param; extern;

procedure escr_ifn_ret_char (          {return one-character string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      c: char);                    {the single character of the string}
  val_param; extern;

procedure escr_ifn_ret_chars (         {return raw characters}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {the characters to return}
  val_param; extern;

procedure escr_ifn_ret_charsp (        {return raw characters from Pascal string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      strp: string);               {the characters to return}
  val_param; extern;

procedure escr_ifn_ret_fp (            {return floating point value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fp: sys_fp_max_t);           {the floating point value to return}
  val_param; extern;

procedure escr_ifn_ret_empty (         {return the empty string}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_ifn_ret_int (           {return integer value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      ii: sys_int_max_t);          {the integer value to return}
  val_param; extern;

procedure escr_ifn_ret_str (           {return string value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      str: univ string_var_arg_t); {the string value to return}
  val_param; extern;

procedure escr_ifn_ret_strp (          {return value from Pascal string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      strp: string);               {the string value to return}
  val_param; extern;

procedure escr_ifn_ret_time (          {return time value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      t: sys_clock_t);             {the time value to return}
  val_param; extern;

procedure escr_ifn_ret_val (           {return arbitrary value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t);            {the value to return}
  val_param; extern;
{
*   Entry points.
}
procedure escr_close (                 {end a use of the ESCR system}
  in out  e_p: escr_p_t);              {pointer to ESCR use state, returned NIL}
  val_param; extern;

procedure escr_commdat_add (           {add identifying syntax for data comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start comment}
  in      en: univ string_var_arg_t;   {characters that end comment, blank for EOL}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_commdat_clear (         {clear script comment syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_commscr_add (           {add identifying syntax for script comment}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start comment}
  in      en: univ string_var_arg_t;   {characters that end comment, blank for EOL}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_commscr_clear (         {clear script comment syntaxes}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_err_atline (            {show error followed by source line number}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      subsys: string;              {name of subsystem, used to find message file}
  in      msg: string;                 {message name within subsystem file}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn, extern);

procedure escr_err_atline_abort (      {bomb with msg and source line on error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stat: sys_err_t;             {error code, nothing done if no error}
  in      subsys: string;              {subsystem name of caller's message}
  in      msg: string;                 {name of caller's message within subsystem}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  val_param; extern;

procedure escr_err_dtype_unimp (       {unimplemented data type internal error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      dtype: escr_dtype_k_t;       {unimplemented data type}
  in      routine: string);            {name of the routine where data type unimplemented}
  options (val_param, noreturn, extern);

procedure escr_err_parm_bad (          {bomb with bad parameter to command error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      parm: univ string_var_arg_t); {the offending parameter}
  options (val_param, noreturn, extern);

procedure escr_err_val (               {show value and data type of offending value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      val: escr_val_t);            {the value}
  val_param; extern;

procedure escr_icmd_add (              {add intrinsic command}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of the command to add}
  in      routine_p: escr_icmd_p_t;    {pointer to routine that implements the command}
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_ifunc_add (             {add intrinsic function}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {name of the function to add}
  in      routine_p: escr_ifunc_p_t;   {pointer to routine that implements the function}
  out     stat: sys_err_t);
  val_param; extern;

procedure escr_open (                  {start a new use of the ESCR system}
  in out  mem: util_mem_context_t;     {parent memory context, will make sub context}
  out     e_p: escr_p_t;               {will point to new initialized ESCR use state}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_out_close_all (         {close all output files}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the files}
  val_param; extern;

procedure escr_out_open (              {open new output file, save previous state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to switch writing to}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_conn (              {run at current line of open file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     conn: file_conn_t;           {pointer to I/O connection state}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_file (              {run starting at first line file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to run script code from}
  in      suff: string;                {allowed file name suffixes, blank separated}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_set_incsuff (           {set allowed suffixes for include file names}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      suff: string);               {suffixes, blank separated}
  val_param; extern;

procedure escr_set_func_detect (       {set routine for detecting function start}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      p: escr_syfunc_st_p_t);      {routine to detect function, NIL for none}
  val_param; extern;

procedure escr_set_preproc (           {set preprocessor mode on/off}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      on: boolean);                {enables preprocessor mode, default off}
  val_param; extern;

procedure escr_stat_sym_nfound (       {symbol not found}
  in      name: univ string_var_arg_t; {symbol name}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param; extern;

procedure escr_syexcl_add (            {add syntax exclusion}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      st: univ string_var_arg_t;   {characters that start exclusion}
  in      en: univ string_var_arg_t;   {characters that end exclusion, blank for EOL}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_syexcl_clear (          {clear all syntax exclusions}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_sym_del (               {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t);        {pointer to symbol to delete, returned NIL}
  val_param; extern;

function escr_sym_name (               {check for valid symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
  val_param; extern;

procedure escr_sym_find (              {look up symbol in symbol table}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in out  sytable: escr_sytable_t;     {symbol table to look up name in}
  out     sym_p: escr_sym_p_t);        {returned pointer to symbol, NIL if not found}
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

procedure escr_sym_new_var (           {create new symbol for a variable}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: escr_dtype_k_t;       {data type of the variable}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_write_obuf (            {write line to output file from OBUF}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;
