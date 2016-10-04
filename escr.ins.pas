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
  escr_err_nfsym_k = 4;                {symbol not found}
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
  escr_err_missingparm_k = 23;         {required parameter is missing}
  escr_err_notbool_k = 24;             {not convertable to BOOL type}
  escr_err_notint_k = 25;              {not convertable to INTEGER type}
  escr_err_nottime_k = 26;             {not convertable to TIME type}
  escr_err_notfp_k = 27;               {not convertable to REAL type}
  escr_err_notintfp_k = 28;            {not convertable to INTEGER or REAL type}
  escr_err_badparm_k = 29;             {bad parameter, cmd name, parm string}
  escr_err_baddtype_k = 30;            {bad data type name, parm}
  escr_err_extra_k = 31;               {too many parameters, first extra}
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
    escr_sym_label_k);                 {label for a specific input files line}

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
    end;

  escr_infile_p_t = ^escr_infile_t;
  escr_infile_t = record               {information about one input file}
    next_p: escr_infile_p_t;           {points to next input file in the list}
    tnam: string_treename_t;           {full treename of the input file}
    suffn: sys_int_machine_t;          {file name suffix number actually used}
    lines_p: escr_inline_p_t;          {pointer to first line of file}
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
escr_looptype_sym_k: (                 {loop over all preprocessor symbols}
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

  escr_t = record                      {state for one use of the ESCR system}
    mem_p: util_mem_context_p_t;       {top mem context for all dynamic mem}
    sym_var: escr_sytable_t;           {symbol table for variables and constants}
    sym_sub: escr_sytable_t;           {symbol table for subroutines}
    sym_mac: escr_sytable_t;           {symbol table for macros}
    sym_fun: escr_sytable_t;           {symbol table for functions}
    sym_cmd: escr_sytable_t;           {symbol table for commands}
    sym_lab: escr_sytable_t;           {symbol table for input file line labels}
    files_p: escr_infile_p_t;          {points to list of input files}
    ibuf: string_var8192_t;            {current input line after function expansions}
    funarg: escr_instr_t;              {function and arguments with parse state}
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
    flags: escr_flags_t;               {system-wide control flags}
    end;
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

procedure escr_err_atline_abort (      {bomb with msg and source line on error}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      stat: sys_err_t;             {error code, nothing done if no error}
  in      subsys: string;              {subsystem name of caller's message}
  in      msg: string;                 {name of caller's message within subsystem}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  val_param; extern;

procedure escr_open (                  {start a new use of the ESCR system}
  in out  mem: util_mem_context_t;     {parent memory context, will make sub context}
  out     e_p: escr_p_t;               {will point to new initialized ESCR use state}
  out     stat: sys_err_t);            {completion status}
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

procedure escr_out_close_all (         {close all output files}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the files}
  val_param; extern;

procedure escr_out_open (              {open new output file, save previous state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fnam: univ string_var_arg_t; {name of file to switch writing to}
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

procedure escr_set_preproc (           {set preprocessor mode on/off}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      on: boolean);                {enables preprocessor mode, default off}
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

function escr_sym_name (               {check for valid symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
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
