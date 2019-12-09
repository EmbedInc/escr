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
  escr_err_nretblk_k = 55;             {not in a block where RETURN is valid}
  escr_err_bltoomany_k = 56;           {block nesting too deep, <level>}
  escr_err_inftoomany_k = 57;          {input file nesting too deep, <level>}
  escr_err_fmtint_k = 58;              {bad integer format string keyword, <name>}
  escr_err_fmtfp_k = 59;               {bad FP format string keyword, <name>}
  escr_err_inh_nest_k = 60;            {inhibit nesting error}
  escr_err_if_nend_k = 61;             {IF not ended before end of execution block}
  escr_err_badsym_k = 62;              {not a valid symbol name, <name>}
  escr_err_loop_keyw_k = 63;           {LOOP keyword incompatible with previous keyword <keyw>}
  escr_err_loop_n_k = 64;              {LOOP overconstrained by N keyword}
  escr_err_sytype_k = 65;              {invalid specific symbol type <required> <actual>}
  escr_err_delsymblk_k = 66;           {attempt to delete name of execution block <name>}
  escr_err_run_nested_k = 67;          {trying to run new code in nested execution context}
  escr_err_var_not_int_k = 68;         {variable is not integer <name>}
  escr_err_var_nfound_k = 69;          {variable not found <name>}
  escr_err_var_nstring_k = 70;         {variable is not string <name>}
  escr_err_notcmddef_k = 71;           {not in command definition}
  escr_err_notfuncdef_k = 72;          {not in function definition}
  escr_err_noblock_k = 73;             {not in any execution block}
  escr_err_parsesaved_k = 74;          {parsing state already saved in curr block}
  escr_err_nfunc_k = 75;               {not in a function}
  escr_err_nparse_fval_k = 76;         {no parse state for FUNCVAL value}
  escr_err_ndirloop_k = 77;            {not in a DIR loop}
  escr_err_sytypemm_k = 78;            {sytype mismatch with name <name>}
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
  escr_sym_p_t = ^escr_sym_t;          {pointer to a symbol instance}
  escr_exblock_p_t = ^escr_exblock_t;  {pointer to info about one nested executable block}
  escr_inh_p_t = ^escr_inh_t;          {pointer to state for one execution inhibit layer}
  escr_p_t = ^escr_t;                  {pointer to ESCR system use state}

  escr_cmdpos_t = record               {command line parsing position}
    p: string_index_t;                 {1-N index of next char to parse}
    end;

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

  escr_sytable_data_p_t = ^escr_sytable_data_t;
  escr_sytable_data_t = record         {data stored in each symbol table entry}
    first_p: escr_sym_p_t;             {points to first (oldest) version of symbol}
    curr_p: escr_sym_p_t;              {points to current version of symbol}
    last_p: escr_sym_p_t;              {points to last (newest) version of symbol}
    end;

  escr_sytable_scan_t = record         {saved data for scanning a symbol table}
    pos: string_hash_pos_t;            {hash table position state}
    valid: boolean;                    {POS is valid}
    end;

  escr_sytype_k_t = (                  {user-visible symbol type ID}
    escr_sytype_unsp_k,                {unspecified}
    escr_sytype_var_k,                 {variable}
    escr_sytype_const_k,               {constant}
    escr_sytype_vcon_k,                {variable or constant}
    escr_sytype_subr_k,                {subroutine}
    escr_sytype_macro_k,               {macro}
    escr_sytype_func_k,                {function}
    escr_sytype_cmd_k,                 {command}
    escr_sytype_label_k);              {label}
  escr_sytype_t = set of escr_sytype_k_t;

  escr_syver_t = record                {user-specified symbol version info}
    ver: sys_int_machine_t;            {abs or relative version number}
    set: boolean;                      {version info is set (not left default)}
    abs: boolean;                      {VER is absolute 1-N number, not relative}
    end;

  escr_sym_k_t = (                     {internal symbol type ID}
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
  escr_symty_t = set of escr_sym_k_t;

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

  escr_sym_t = record                  {all the info about one symbol}
    table_p: escr_sytable_p_t;         {points to symbol table this symbol is in}
    ent_p: escr_sytable_data_p_t;      {points to symbol table data for this symbol}
    prev_p: escr_sym_p_t;              {points to previous (older) symbol of this name}
    next_p: escr_sym_p_t;              {points to next (newer) symbol of this name}
    name_p: string_var_p_t;            {pointer to symbol name in symbol table}
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
    escr_looptype_cnt_k,               {counted loop}
    escr_looptype_dir_k);              {looping over entries in a directory}

  escr_loop_p_t = ^escr_loop_t;
  escr_loop_t = record                 {info about a loop}
    looptype: escr_looptype_k_t;       {type of loop}
    case escr_looptype_k_t of          {unique data for each type of loop}
escr_looptype_unc_k: (                 {unconditional loop}
      );
escr_looptype_sym_k: (                 {loop over all script processor symbols}
      sym_const_p: escr_sym_p_t;       {points to loop iteration value constant}
      sym_list_p: string_list_p_t;     {points to list of symbol name strings}
      );
escr_looptype_cnt_k: (                 {loop over integer values with fixed increment}
      cnt_const_p: escr_sym_p_t;       {points to loop iteration value constant}
      cnt_start: sys_int_max_t;        {starting value}
      cnt_curr: sys_int_max_t;         {current value}
      cnt_end: sys_int_max_t;          {ending value}
      cnt_inc: sys_int_max_t;          {increment per iteration}
      cnt_inf: boolean;                {infinite loop}
      );
escr_looptype_dir_k: (                 {loop over directory entries}
      dir_conn: file_conn_t;           {connection to the directory}
      dir_ftype: file_type_t;          {file types to not ignore}
      dir_fnam: string_leafname_t;     {current directory entry name}
      dir_finfo: file_info_t;          {info about the current dir entry}
      dir_open: boolean;               {connection to directory is open}
      );
    end;

  escr_instr_t = record                {input string to be parsed}
    p: string_index_t;                 {1-N string string parse index}
    s: string_var8192_t;               {the string}
    end;

  escr_parse_p_t = ^escr_parse_t;
  escr_parse_t = record                {complete copy of input parsing state}
    prev_p: escr_parse_p_t;            {points to last pushed parse state, NIL at top}
    ibuf: string_var8192_t;            {curr input line after function expansions}
    ip: string_index_t;                {IBUF parse index}
    lparm: string_var8192_t;           {last parameter parsed from input line}
    cmd: string_var32_t;               {name of current command}
    funarg: escr_instr_t;              {function and arguments with parse state}
    funame: string_var80_t;            {name of function currently being executed}
    funret: string_var8192_t;          {function return string}
    end;

  escr_exblock_k_t = (                 {types of execution block}
    escr_exblock_top_k,                {top level block, not defined by code}
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
    sym_curr_p: escr_sym_p_t;          {symbol to restore to curr version on exit, if any}
    mem_p: util_mem_context_p_t;       {mem context for block, deleted when block closed}
    arg_p: escr_arg_p_t;               {points to list of arguments for this block}
    arg_last_p: escr_arg_p_t;          {points to last argument in list}
    nargs: sys_int_machine_t;          {number of arguments in arguments list}
    locsym_p: escr_sylist_p_t;         {points to list of symbols local to this block}
    inpos_p: escr_inpos_p_t;           {points to current nested input file position}
    previnh_p: escr_inh_p_t;           {points to previous inhibit before this block}
    loop_p: escr_loop_p_t;             {points to loop definition, NIL = none}
    parse_p: escr_parse_p_t;           {points to saved parse state, NIL = none}
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
    parse: escr_parse_t;               {root input parsing state}
    parse_p: escr_parse_p_t;           {points to current input parsing state}
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
    exstat: sys_int_machine_t;         {script exit status}
    end;
{
****************************************
*
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
  out     stat: sys_err_t);            {completion status, no match or hard error}
  val_param; extern;

procedure escr_get_pos_restore (       {restore to previously saved parsing position}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      pos: escr_cmdpos_t);         {saved parsing position to go back to}
  val_param; extern;

procedure escr_get_pos_save (          {save current command line parsing position}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     pos: escr_cmdpos_t);         {saved parsing position}
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
****************************************
*
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

function escr_ifn_get_name (           {get next parameter as symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  name: univ string_var_arg_t; {returned name}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {returning with name}
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

function escr_ifn_get_var_int (        {get next funct parameter as integer variable}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     sym_p: escr_sym_p_t;         {pointer to integer variable symbol descriptor}
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
****************************************
*
*   Other entry points.
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
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_exblock_inline_push (   {push new source line location for exec block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to next input line to use}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_exblock_inline_set (    {go to new input source position in curr block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to next input line to use}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_exblock_locals_off (    {current version of symbols parent, not local}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_locals_on (     {make local versions of symbols current}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_parse_save (    {save parsing state, will be restored on block end}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_exblock_refsym (        {set referencing symbol of current block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym: escr_sym_t);            {version of symbol referencing this block}
  val_param; extern;

procedure escr_exblock_ulab_init (     {create unique labels list in this block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_new (           {create and install new execution block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_exblock_quit (          {stop executing in the current block}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_exblock_repeat (        {loop back to start of block}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
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

procedure escr_exitstatus (            {set EXITSTATUS, create if needed}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      exstat: sys_int_machine_t);  {value to set EXITSTATUS to}
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
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_inh_end (               {end the current execution inhibit}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
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

procedure escr_loop_close (            {close system state of loop of curr block}
  in out  loop: escr_loop_t;           {the loop to close out}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_loop_iter (              {advance to next loop iteration}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t)             {completion status}
  :boolean;                            {looped continues, not terminated}
  val_param; extern;

procedure escr_open (                  {start a new use of the ESCR system}
  in out  mem: util_mem_context_t;     {parent memory context, will make sub context}
  out     e_p: escr_p_t;               {will point to new initialized ESCR use state}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_out_close (             {close the current output file, pop previous}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      del: boolean);               {delete the file}
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

procedure escr_parse_init (            {init input parsing state descriptor}
  out     par: escr_parse_t);          {parsing state to init}
  val_param; extern;

procedure escr_run (                   {run, state all set up, returns on block end}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_atline (            {run starting at specific input files line}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      line_p: escr_inline_p_t;     {pointer to first input files line to execute}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_clean (             {clean out any existing execution state}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_conn (              {run at current line of open file}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  var     conn: file_conn_t;           {pointer to I/O connection state}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_run_file (              {run starting at first line of file}
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

procedure escr_show_obuf (             {write line to standard output from OBUF}
  in out  e: escr_t);                  {state for this use of the ESCR system}
  val_param; extern;

procedure escr_stat_sym_nfound (       {symbol not found}
  in      name: univ string_var_arg_t; {symbol name}
  in out  stat: sys_err_t);            {set, not altered if already err}
  val_param; extern;

procedure escr_str_quote (             {quote and append string, ESCR syntax}
  in      stri: univ string_var_arg_t; {input string}
  in out  stro: univ string_var_arg_t); {string to append to}
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

procedure escr_sym_curr_prev (         {make previous version of symbol current}
  in out  sym: escr_sym_t);            {make previous of this version current}
  val_param; extern;

procedure escr_sym_curr_sym (          {set current version of symbol}
  in out  sym: escr_sym_t);            {the version to make current}
  val_param; extern;

procedure escr_sym_del (               {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t;         {pointer to version to delete, returned NIL}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_del_pos (           {delete specific symbol version}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  sym_p: escr_sym_p_t;         {pointer to version to delete, returned NIL}
  in out  hpos: string_hash_pos_t;     {table pos for this symbol, invalid if deleted}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_del_name (          {delete symbol version by name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_del_name_type (     {delete symbol version by name, specific type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  in      sytype: escr_sytype_k_t;     {symbol must be this type}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_find (              {find version from qualified symbol name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {qualified symbol name}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param; extern;

procedure escr_sym_find_curr (         {find current version of a symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param; extern;

procedure escr_sym_find_type (         {find symbol of specific type}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be qualified}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  out     sym_p: escr_sym_p_t;         {returned pointer to version, NIL if not found}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_lookup_qual (       {get symbol version from qualified name}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name, may be fully qualified}
  out     hpos: string_hash_pos_t;     {returned position of name in symbol table}
  out     sym_p: escr_sym_p_t;         {returned pointer to version, NIL if not found}
  out     stat: sys_err_t);            {completion status, SYM_P = NIL on error}
  val_param; extern;

procedure escr_sym_lookup_sym (        {look up symbol table position of symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      sym: escr_sym_t;             {symbol to look up name of}
  out     hpos: string_hash_pos_t);    {returned position of name in symbol table}
  val_param; extern;

procedure escr_sym_lookup_ver (        {get specific version of a symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sytype: escr_sytype_k_t;     {user-visible symbol type}
  in      syver: escr_syver_t;         {version specifier}
  out     hpos: string_hash_pos_t;     {returned position of name in symbol table}
  out     sym_p: escr_sym_p_t);        {returned pointer to version, NIL if not found}
  val_param; extern;

procedure escr_sym_name (              {make fully qualified name of a symbol}
  in      sym: escr_sym_t;             {symbol version to make full name of}
  in out  name: univ string_var_arg_t); {returned fully qualified name}
  val_param; extern;

function escr_sym_name_bare (          {check for valid symbol bare name}
  in      name: univ string_var_arg_t) {the name to check}
  :boolean;                            {TRUE if valid symbol name, FALSE otherwise}
  val_param; extern;

procedure escr_sym_name_bare_check (   {check for valid bare symbol name}
  in      name: univ string_var_arg_t; {the name to check}
  out     stat: sys_err_t);            {error iff NAME is not valid bare symbol name}
  val_param; extern;

procedure escr_sym_name_parse (        {parse qualified symbol name}
  in      name: univ string_var_arg_t; {input qualified symbol name}
  in out  rawname: univ string_var_arg_t; {returned bare symbol name}
  out     sytype: escr_sytype_k_t;     {user-visible symbol type ID}
  out     syver: escr_syver_t;         {symbol version information}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

function escr_sym_name_sytype (        {interpret symbol type keyword}
  in      name: univ string_var_arg_t) {the keyword, must be upper case}
  :escr_sytype_k_t;                    {symbol type ID or ESCR_SYTYPE_UNSP_K}
  val_param; extern;

procedure escr_sym_new (               {create new symbol}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {bare symbol name}
  in      sz: sys_int_adr_t;           {size of the whole symbol descriptor}
  in      global: boolean;             {create global, not local symbol}
  in out  sytable: escr_sytable_t;     {symbol table to add symbol to}
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

procedure escr_sym_new_var (           {create new symbol for a variable}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      name: univ string_var_arg_t; {symbol name}
  in      dtype: escr_dtype_k_t;       {data type of the variable}
  in      len: sys_int_machine_t;      {extra length parameter used for some data types}
  in      global: boolean;             {create global, not local symbol}
  out     sym_p: escr_sym_p_t;         {returned pointer to the new symbol}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sym_sytype_name (       {get name of user-visible symbol type}
  in      sytype: escr_sytype_k_t;     {user-visible type to get name of}
  in out  name: univ string_var_arg_t); {returned type name}
  val_param; extern;

procedure escr_sytable_init (          {initialize a symbol table}
  in out  mem: util_mem_context_t;     {parent memory context}
  out     sytable: escr_sytable_t;     {the symbol table to initialize}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;

procedure escr_sytable_scan_start (    {start scanning all entries in a symbol table}
  in      table: escr_sytable_t;       {the symbol table to scan}
  out     scan: escr_sytable_scan_t);  {returned initialized scanning state}
  val_param; extern;

procedure escr_sytable_scan (          {get next symbol table entry}
  in out  scan: escr_sytable_scan_t;   {symbol table scanning state}
  out     name_p: string_var_p_t;      {returned pointer to symbol name in table}
  out     ent_p: escr_sytable_data_p_t); {returned pointer to next entry, NIL at end}
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

function escr_term_parse (             {parse next term from input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
  in out  term: univ string_var_arg_t; {returned raw term characters, unquoted}
  out     quoted: boolean;             {the characters were quoted}
  out     stat: sys_err_t)             {completion status, no error on func TRUE}
  :boolean;                            {TRUE if term was available and no error}
  val_param; extern;

function escr_term_raw (               {get chars of next token from input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
  in out  term: univ string_var_arg_t; {returned raw term characters, unquoted}
  out     stat: sys_err_t)             {completion status, no error on func TRUE}
  :boolean;                            {TRUE if term was available and no error}
  val_param; extern;

function escr_term_val (               {get value of next term in input string}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      fstr: univ string_var_arg_t; {source string, term will be next token}
  in out  p: string_index_t;           {source string parse index, updated}
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

procedure escr_write_obuf (            {write line to output file from OBUF}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     stat: sys_err_t);            {completion status}
  val_param; extern;
