/*   Messages for the ESCR library and associated applications.
/*
msg err1
  lan english -def
    Unable to create new memory context.
msg err2
  lan english -def
    Unable to allocate dynamic memory of %p1i bytes.
msg err3
  lan english -def
    The maximum versions of symbol "%p2s" already exist on attempt to
    create a new version.
msg err4
  lan english -def
    Symbol "%p1s" not found.
msg err5
  lan english -def
    Unexpected end of input encountered.
msg err6
  lan english -def
    Command "%p1s" is unrecognized.
msg err7
  lan english -def
    Symbol "%p1s" is not a command as expected.
msg err8
  lan english -def
    Label "%p1s" was not found.
msg err9
  lan english -def
    Symbol "%p1s" is not a label.
msg err10
  lan english -def
    In nested input file at end of block.
msg err11
  lan english -def
    No output file exists on attempt to write to it.
msg err12
  lan english -def
    No output file exists on attempt to close it.
msg err13
  lan english -def
    Attempt to close the preprocessor output file.
msg err14
  lan english -def
    Bad input line.
msg err15
  lan english -def
    Starting string for syntax range is empty.
msg err16
  lan english -def
    No end (%p2s) for syntax exclusion start (%p1s) in column %p3i.
msg err17
  lan english -def
    Missing end for script comment started in column %p1s.
msg err18
  lan english -def
    End of function "%p1s" not found.
msg err19
  lan english -def
    No such function "%p1s".
msg err20
  lan english -def
    Symbol "%p1s" is not a function.
msg err21
  lan english -def
    Symbol "%p1s" does not have a value (not a variable or constant).
msg err22
  lan english -def
    String "%p1s" can not be interpreted as a value.
msg err23
  lan english -def
    A required parameter to command %p1s is missing.
msg err24
  lan english -def
    The term "%p1s" can not be converted to BOOL type.
msg err25
  lan english -def
    The term "%p1s" can not be converted to INTEGER type.
msg err26
  lan english -def
    The term "%p1s" can not be converted to TIME type.
msg err27
  lan english -def
    The term  "%p1s"can not be converted to REAL type.
msg err28
  lan english -def
    The term "%p1s" can not be converted to INTEGER or REAL type.
msg err29
  lan english -def
    Parameter "%p1s" to command "%p2s" is unrecognized.
msg err30
  lan english -def
    "%p1s" is not the name of data type.
msg err31
  lan english -def
    Too many command parameters found.  The first unexpected parameter was "%p1s".
msg err32
  lan english -def
    The string "%p1s" is not one character long.  Only a one-character
    string is allowed here.
msg err33
  lan english -def
    A required parameter to function "%p1s" is missing.
msg err34
  lan english -def
    Too many parameters to function "%p2s".  The first unexpected parameter was "%p1s".
msg err35
  lan english -def
    The data type of term "%p1s" is incompatible with its usage here.
msg err36
  lan english -def
    Attempt to divide by zero.
msg err37
  lan english -def
    Attempt to take the square root of a negative value.
msg err38
  lan english -def
    Non-integer exponent to negative value.
msg err39
  lan english -def
    Attempt to get logarithm of a non-positive value.
msg err40
  lan english -def
    Date-modifying parameter %p1s to DATE function after date computed.
msg err41
  lan english -def
    Parameter "%p1s" to function "%p2s" is unrecognized.
msg err42
  lan english -def
    Symbol "%p1s" is not a variable.  Only a variable is allowed here.
msg err43
  lan english -def
    Bad command %p1s.  In root execution block.
msg err44
  lan english -def
    Bad command %p1s.  In wrong type of block.
msg err45
  lan english -def
    Bad command %p1s.  Attempt to end block in include file.
msg err46
  lan english -def
    Not in IF block.  THEN, ELSE, and ENDIF commands are not allowed here.
msg err47
  lan english -def
    THEN command is not allowed due to previous THEN or ELSE.
msg err48
  lan english -def
    ELSE command is not allowed due to previous ELSE.
msg err49
  lan english -def
    No THEN or ELSE found in IF block.
msg err50
  lan english -def
    Loop iteration increment is 0.
msg err51
  lan english -def
    Not in a LOOP block.
msg err52
  lan english -def
    Not in a MACRO block.
msg err53
  lan english -def
    Not in a SUBROUTINE definition block.
msg err54
  lan english -def
    Symbol %p1s is not a subroutine.
msg err55
  lan english -def
    Invalid RETURN command encountered.  Not in a user-defined subroutine,
    command, or function.
msg err56
  lan english -def
    Attempt to exceed maximum execution block nesting level of %p1i.
msg err57
  lan english -def
    Attempt to exceed the maximimum input file nesting level of %p1i.  This may be due
    circular references in include files.
msg err58
  lan english -def
    The integer format specifier "%p1s" is unrecognized.
msg err59
  lan english -def
    The floating point format specifier "%p1s" is unrecognized.
msg err60
  lan english -def
    Unexpected execution block and execution inhibit nesting error.  This is possibly a
    internal error.  Attempt to delete the execution inhibit to restore to after the end
    of the current execution block.  The execution inhibit type ID is %p1i.
msg err61
  lan english -def
    IF construct not ended before end of execution block.
msg err62
  lan english -def
    "%p1s" is not a valid symbol name.
msg err63
  lan english -def
    They LOOP command keyword %p1s is incompatible with a previous keyword.
msg err64
  lan english -def
    FROM, TO, and N options used in same LOOP command, at most two of the
    three are allowed.
msg err65
  lan english -def
    Invalid symbol type of %p2s specified, type of %p1s required.
msg err66
  lan english -def
    Attempt to delete symbol "%p1s", which is the name of a current
    execution block.
msg err67
  lan english -def
    Attempt to run new code in existing nested execution context.
msg err68
  lan english -def
    The variable "%p1s" is not integer.
msg err69
  lan english -def
    The variable "%p1s" does not exist.
msg err70
  lan english -def
    The variable "%p1s" does not have data type STRING.
msg err71
  lan english -def
    Not in a COMMAND definition block.
msg err72
  lan english -def
    Not in a FUNCTION definition block.
msg err73
  lan english -def
    Not in any execution block.  A current execution block is required here.
msg err74
  lan english -def
    Parsing state has already been saved in the current execution block.
msg err75
  lan english -def
    Not in a function.
msg err76
  lan english -def
    INTERNAL ERROR: The input parsing state to add the FUNCVAL value to
    does not exist.
msg err77
  lan english -def
    Not in a directory entries loop.
/*
/*   Messages without error status codes.  These messages are used directly by
/*   ESCR system code.
/*
msg err_dtype_unimp
  lan english -def
    INTERNAL ERROR: Data type with ID %p1i is not implemented in routine %p2s.
msg err_fnam_lnum
  lan english -def
    Error on line %p2i of file "%p1s".
msg err_loclab_exist
  lan english -def
    INTERNAL ERROR: Subroutine ESCR_EXBLOCK_ULAB_INIT called with unique
    labels list already existing.
msg err_lookupver_nover
  lan english -def
    INTERNAL ERROR: SYM_P nil in ESCR_SYM_LOOKUP_VER on attempt to find
    version %p1i of symbol %p2s.
msg in_block
  lan english -def
    In block starting on line %p1i of "%p2s".
msg included_from
  lan english -def
    Included from "%p1s" at line %p2i.
msg open
  lan english -def
    Error occurred on attempt to open a new ESCR script system use state.
msg parm_bad_cmd
  lan english -def
    Parameter "%p1s" to command "%p2s" is unrecognized.
msg term_val_int
  lan english -def
    The term has an integer value of %p1i.
msg term_val_fp
  lan english -def
    The term has a floating point value of %p1f15.3.
msg term_val_bool
  lan english -def
    The term has a boolean value of %p1s.
msg term_val_str
  lan english -def
    The term has a string value of "%p1s".
msg term_val_unk
  lan english -def
    The data type of the term is unrecognized.  The data type ID is %p1i.
/*
/*   Messages for possible use by applications using the ESCR library.  These
/*   messages are not used by ESCR code directly.
/*
msg var_new
  lan english -def
    Error occurred on attempt to create the new variable "%p1s".
