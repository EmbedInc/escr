//  ESCR script test that generates Fibbonacci series.
//
const llen integer = 70      //max line length
var new ii integer
var new jj integer
var new kk integer
var new str string

subroutine shownum
  var local s string
  set s [str [arg 1]]
  if [> [+ [slen str] [slen s] 1] llen] then
    show str
    set str ""
    endif
  if [<> [slen str] 0] then
    set str [str str " "]
    endif
  set str [str str s]
  endsub

set ii 0
set jj 1
call shownum ii
call shownum jj

loop from 1 to 30
  set kk [+ ii jj]
  call shownum kk
  set ii jj
  set jj kk
  endloop

if [> [slen str] 0] then
  show str
  endif

