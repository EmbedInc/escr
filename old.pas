*   fline_coll_t
*
  escr_infile_p_t = ^escr_infile_t;
  escr_infile_t = record               {information about one input file or snippet thereof}
    next_p: escr_infile_p_t;           {points to next input file in the list}
    tnam: string_treename_t;           {full treename of the input file}
    lfirst_p: escr_inline_p_t;         {pointer to first line of snippet}
    llast_p: escr_inline_p_t;          {pointer to last line of snippet}
    end;

*   fline_line_t
*
  escr_inline_t = record               {info about one input file line}
    next_p: escr_inline_p_t;           {pointer to next input line this file, NIL = last}
    file_p: escr_infile_p_t;           {pointer to file this line is from}
    lnum: sys_int_machine_t;           {1-N line number of this line}
    str_p: string_var_p_t;             {pointer to string for this line}
    end;

*   fline_hier_t
*
  escr_inpos_p_t = ^escr_inpos_t;
  escr_inpos_t = record                {one level in current input files position}
    prev_p: escr_inpos_p_t;            {points to previous level, back there on EOF}
    level: sys_int_machine_t;          {nesting level within block, top = 0}
    line_p: escr_inline_p_t;           {points to next input line}
    last_p: escr_inline_p_t;           {points to last input line read}
    end;


