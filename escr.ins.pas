{   Application interface to the Embed Inc scripting engine, ESCR.
}
const
  escr_max_symvers_k = 32;             {max allowed symbol versions of same name}
  escr_max_inclev_k = 32;              {max include file nesting level per block}
  escr_max_blklev_k = 128;             {max execution block nesting level}
  escr_max_namelen_k = 80;             {max characters in symbol name}
  escr_sym_log2buck_k = 7;             {Log2 buckets in symbol tables}
  escr_lab_log2buck_k = 5;             {Log2 buckets in local labels tables}
  escr_lab_maxlen_k = 32;              {maximum length of undecorated local label names}
  escr_string_var_len_k = 1024;        {number of characters a string variable can hold}

  escr_sym_nbuck_k =                   {number of buckets in symbol table}
    lshft(1, escr_sym_log2buck_k);
  escr_lab_nbuck_k =                   {number of buckets in local labels tables}
    lshft(1, escr_lab_log2buck_k);

type
  escr_inline_p_t = ^escr_inline_t;    {pointer to line within input file}
  escr_inline_pp_t = ^escr_inline_p_t;
  escr_infile_p_t = ^escr_infile_t;    {pointer to one input file stored in memory}
  escr_exblock_p_t = ^escr_exblock_t;  {pointer to info about one nested executable block}
  escr_inh_p_t = ^escr_inh_t;          {pointer to state for one execution inhibit layer}

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

  escr_sym_k_t = (                     {symbol type}
    escr_sym_var_k,                    {variable}
    escr_sym_const_k,                  {constant}
    escr_sym_subr_k,                   {subroutine}
    escr_sym_macro_k);                 {macro}

  escr_sym_p_t = ^escr_sym_t;
  escr_sym_pp_t = ^escr_sym_p_t;
  escr_sym_t = record                  {all the info about one symbol}
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
escr_sym_subr_k: (                     {subroutine}
      subr_line_p: escr_inline_p_t;    {points to first line of subroutine definition}
      );
escr_sym_macro_k: (                    {macro}
      macro_line_p: escr_inline_p_t;   {points to first line of macro definition}
      );
    end;

  escr_infile_t = record               {information about one input file}
    next_p: escr_infile_p_t;           {points to next input file in the list}
    tnam: string_treename_t;           {full treename of the input file}
    lines_p: escr_inline_p_t;          {pointer to first line of file}
    end;

  escr_inline_t = record               {info about one input file line}
    next_p: escr_inline_p_t;           {pointer to next input line this file, NIL = last}
    file_p: escr_infile_p_t;           {pointer to file this line is from}
    lnum: sys_int_machine_t;           {1-N line number of this line}
    str_p: string_var_p_t;             {pointer to string for this line}
    end;

  escr_sylist_p_t = ^escr_sylist_t;
  escr_sylist_pp_t = ^escr_sylist_p_t;
  escr_sylist_t = record               {one entry in list of local symbols of a block}
    next_p: escr_sylist_p_t;           {points to next list entry, NIL = last}
    sym_p: escr_sym_p_t;               {points to symbol local to this block}
    end;

  escr_inpos_p_t = ^escr_inpos_t;
  escr_inpos_t = record                {one level in current input files position}
    prev_p: escr_inpos_p_t;            {points to previous level, back there on EOF}
    level: sys_int_machine_t;          {nesting depth, top = 0}
    line_p: escr_inline_p_t;           {points to next input line}
    last_p: escr_inline_p_t;           {points to last input line read}
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
    escr_exblock_sub_k,                {subroutine}
    escr_exblock_mac_k,                {macro}
    escr_exblock_blk_k,                {BLOCK ... ENDBLOCK}
    escr_exblock_loop_k);              {LOOP ... ENDLOOP}

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
    inh_p: escr_inh_p_t;               {points to original execution inhibit for this block}
    loop_p: escr_loop_p_t;             {points to loop definition, NIL = none}
    bltype: escr_exblock_k_t;          {type of execution block}
    loclab: string_hash_handle_t;      {table of local labels, NIL for none, use parent}
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

  escr_p_t = ^escr_t;
  escr_t = record                      {state for one use of the ESCR system}
    out_p: escr_outfile_p_t;           {points to current output file info, NIL = none}
    sym: string_hash_handle_t;         {symbol table}
    mem_sytable_p: util_mem_context_p_t; {pointer to mem context for global symbol table}
    mem_sym_p: util_mem_context_p_t;   {pointer to mem context for global symbol data}
    mem_top_p: util_mem_context_p_t;   {points to general mem context for top level data}
    files_p: escr_infile_p_t;          {points to list of input files}
    exblock_p: escr_exblock_p_t;       {points to info about current execution block}
    inhibit_p: escr_inh_p_t;           {points to current execution inhibit info}
    labeln: sys_int_conv32_t;          {sequential number for next unique label}
    cmd: string_var32_t;               {name of current command, upper case}
    ibuf: string_var8192_t;            {current input line after function expansions}
    ip: string_index_t;                {IBUF parse index}
    lparm: string_var8192_t;           {last parameter parsed from input line}
    obuf: string_var8192_t;            {one line output buffer}
    end;
