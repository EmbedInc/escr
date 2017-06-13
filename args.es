//   Show all arguments.
//
var new arg integer = 1      //current argument number

loop with ii from 0 by -1
  if [exist ii arg] then
    set arg ii               //update number of lowest existing argument
    repeat
    endif
  quit
  endloop
//
//   ARG is set to the number of the first available argument, if any.
//
loop                         //loop over all arguments
  if [not [exist arg arg]] then //done all the available arguments ?
    quit
    endif
  show [int arg "fw 3"] ": " [qstr [arg arg]]
  set arg [+ arg 1]          //advance to next argument
  endloop
