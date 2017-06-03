//   One-off script for doing processing per intrinsic function.  The subroutine
//   DOFUNC, below, is intended to be re-written as needed.  The rest of the
//   code in this file calls DOFUNC once for each intrinsic function.
//

////////////////////////////////////////////////////////////////////////////////
//
//   Subroutine DOFUNC name desc class ... class
//
//   This routine is called once for each intrinsic function.  NAME is the name
//   of the function as it would be called in the source code.  DESC is the same
//   as NAME unless NAME contains special characters.  DESC is always simple
//   alpha-numeric text.  The remaining CLASS arguments are keywords that
//   identify classes or catagories the function belongs in.
//
//   For example, the addition function would be called:
//
//     call dofunc "+" plus arithmetic
//
subroutine dofunc
  var local name string = [arg 1] //function name
  var local desc string = [qstr [arg 2]] //text-only function name
  var local ii integer
  var local s string
  var local b bool

  var local nclass integer = 0 //init number of classes this function is in
  loop                      //once for each class
    if [not [exist [+ nclass 3] arg]] then //no more classes ?
      quit
      endif
    set nclass [+ nclass 1]  //make 1-N number of this class
    var new class[chars nclass] string = [qstr [arg [+ nclass 2]]]
    endloop

//************************  Start custom code here *****************************
//


//   HTML templates for function descriptions.
//
//set s ""
//set s [str s "<p><dt><tt id=""" desc """>"]
//set s [str s "<b>" name "</b> <i>arg</i></tt></dt><dd>"]
//write s
//write
//write "  <p>" desc
//write


//   HTML table of contents entries.
//
set b false
loop for ii from 1 to nclass
  set b [or b [= class[chars ii] "miscellaneous"]]
  endloop
if b then
  write "    <a href=""#" desc """>" name "</a> &nbsp; - &nbsp; " desc "<br>"
  endif


//   List of all classes.
//
//loop for ii from 1 to nclass
//  write class[chars ii]
//  endloop
//


//
//*************************  End of custom code ********************************

  loop for ii from 1 to nclass
    del class[chars ii]
    endloop
  endsub

////////////////////////////////////////////////////////////////////////////////
//
//   Start of main routine.
//
writeto "/temp/x.txt"

call dofunc "+" plus arithmetic
call dofunc "-" minus arithmetic
call dofunc "*" times arithmetic
call dofunc "/" divide arithmetic
call dofunc "~" inv bitwise
call dofunc "<" lt logical
call dofunc "<=" le logical
call dofunc "=" eq logical
call dofunc "<>" ne logical
call dofunc ">=" ge logical
call dofunc ">" gt logical
call dofunc "ABS" abs arithmetic
call dofunc "AND" and bitwise logical
call dofunc "ARG" arg script
call dofunc "CCODE" ccode string
call dofunc "CHAR" char string
call dofunc "CHARS" chars string
call dofunc "COS" cos arithmetic
call dofunc "DATE" date miscellaneous
call dofunc "DEGR" degr arithmetic
call dofunc "DIV" div arithmetic
call dofunc "E" e arithmetic
call dofunc "ENG" eng string
call dofunc "EVAR" evar system
call dofunc "EXIST" exist script system
call dofunc "EXP" exp arithmetic
call dofunc "FP" fp string
call dofunc "IF" if script
call dofunc "INT" int string
call dofunc "LAB" lab string
call dofunc "LCASE" lcase string
call dofunc "LNAM" lnam system
call dofunc "LOG" log arithmetic
call dofunc "LOG2" log2 arithmetic
call dofunc "MAX" max arithmetic
call dofunc "MIN" min arithmetic
call dofunc "NOT" not logical
call dofunc "NOW" now miscellaneous
call dofunc "OR" or logical bitwise
call dofunc "PI" pi arithmetic
call dofunc "QSTR" qstr string
call dofunc "RDEG" rdeg arithmetic
call dofunc "RND" rnd arithmetic
call dofunc "SEQ" seq miscellaneous
call dofunc "SHIFTL" shiftl bitwise
call dofunc "SHIFTR" shiftr bitwise
call dofunc "SIN" sin arithmetic
call dofunc "SINDX" sindx string
call dofunc "SLEN" slen string
call dofunc "SQRT" sqrt arithmetic
call dofunc "STR" str string
call dofunc "SUBSTR" substr string
call dofunc "SYM" sym script
call dofunc "TAN" tan arithmetic
call dofunc "TNAM" tnam system
call dofunc "TRUNC" trunc arithmetic
call dofunc "UCASE" ucase string
call dofunc "V" v script
call dofunc "XOR" xor logical bitwise
