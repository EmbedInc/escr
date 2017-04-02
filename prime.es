//   Demonstation of ESCR's prgramming and computational abilities.
//
//   This program writes out all the prime numbers within a range.  The range is
//   defined by the constants START_K and END_K, below, or can be set by
//   defining the integer variables START and/or END with the -I command line
//   option.  For example, the following command line will show all prime
//   numbers from 1000 to 2000:
//
//     escr prime -i start 1000 -i end 2000
//
const   start_k integer = 0  //default start of prime number search range
const   end_k integer = 1000 //default end of prime number search range
const   llen integer = 80    //maximum allowed output line length

var new t1 time = [now]      //start time
var new r real               //scratch floating point
var new ii integer           //scratch integer
var new line string          //output line
var new n integer            //number of primes found
var new range integer        //number of numbers to check for prime

if [not [exist "start"]] then //use default start of range ?
  var new start integer = start_k
  endif
set start [max start 2]
if [not [exist "end"]] then  //use default end of range ?
  var new end integer = end_k
  endif
set end [max end start]
set range [+ [- end start] 1] //size of range to check

if [and [<= start 2] [>= end 2]] then //handle the special case of 2 in the range
  set line "2"
  endif

set ii [max start 3]         //make first candidate to check
set ii [or [and ii [~ 1]] 1] //bump up to next odd number if even
loop with p from ii to end by 2 //once for each odd number in the range
  var local tk string        //scratch string

  loop with f from 3 to p by 2 //scan over possible factors
    if [> [* f f] p] then    //exhausted all possible factors, P is prime ?
      //
      //   P is prime.  Add it to the end of LINE.
      //
      set n [+ n 1]          //count one more prime number found
      set tk [str p]         //make string representation of prime
      if [> [+ [slen line] 1 [slen tk]] llen] then //would overflow this line ?
        show line            //write out the filled line
        set line ""          //reset the output line being built to empty
        endif
      if [> [slen line] 0] then //add blank before new number if not first
        set line [str line " "]
        endif
      set line [str line p]
      quit                   //stop checking factors of this prime
      endif
    set ii [div p f]         //divide the candidate prime by this factor
    if [= [* f ii] p] then   //divided evenly, P is not prime ?
      quit                   //done checking this P
      endif
    endloop                  //back to check P against next possible factor
  endloop                    //back for next candidate prime

if [> [slen line] 0] then    //there is a partial unwritten output line ?
  show line                  //write it
  endif

show
set line ""
set line [str line n " prime numbers"]
set r [* [/ n range] 100]
set line [str line " (" [fp r "sig 2 mxr 4 nptn nexp"] "% of range)"]
set r [- [now] t1]
set line [str line " found in " [eng r] "s."]
show line
