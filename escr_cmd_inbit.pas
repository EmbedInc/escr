{   Subroutine ESCR_CMD_INBIT (E, STAT)
*
*   /INBIT name port bit [PUP]
*
*   Declares a particular I/O bit and indicates that the I/O bit will be
*   used as an input.  PORT is the name of the port containing the bit,
*   and BIT is the bit number within the register.  The following
*   assembler constants will be declared:
*
*     <name>_reg   -  Address of port register containing I/O bit.
*     <name>_tris  -  Address of TRIS register controlling in/out direction.
*     <name>_bit   -  Number of bit within port and tris regs for this I/O bit.
*     <name>_lat   -  Address of LAT register for this port, if any.
*
*   The following assembler variables will be updated:
*
*     VAL_TRISx  -  Initial value for TRIS register
*     VAL_PULLUPx  -  Indicates which pins need pullups enabled
*
*   String substitution macros will be defined:
*
*     #define <name>_pin <port>,<bit>
*     #define <name>_pinlat <lat>,<bit>
}
module escr_cmd_inbit;
define escr_cmd_inbit;
%include 'escr2.ins.pas';

procedure escr_cmd_inbit (
  in out  e: escr_t;
  out     stat: sys_err_t);
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  pick: sys_int_machine_t;             {number of token picked from list}
  portl: char;                         {A-Z I/O port name, lower case}
  portu: char;                         {A-Z I/O port name, upper case}
  pup: boolean;                        {enable passive pullup}
  name: string_var80_t;                {I/O bit name}
  namel: string_var80_t;               {lower case I/O bit name}
  im: sys_int_max_t;                   {max size integer}
  bit: sys_int_machine_t;              {number of I/O bit within port register}
  strbit: string_var16_t;              {decimal integer I/O bit number string}
  tk: string_var80_t;                  {scratch token}
  syname: string_var32_t;              {scratch symbol name}
  sym_p: escr_sym_p_t;                 {pointer to newly created symbol}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if e.inhibit_p^.inh then return;     {execution is inhibited ?}
  name.max := size_char(name.str);     {init local var strings}
  namel.max := size_char(namel.str);
  strbit.max := size_char(strbit.str);
  tk.max := size_char(tk.str);
  syname.max := size_char(syname.str);

  if not escr_get_token (e, name)      {get NAME parameter}
    then escr_err_parm_missing (e, '', '', nil, 0);
  string_copy (name, namel);           {make lower case version of name}
  string_downcase (namel);
{
*   Get PORTx parameter.
}
  if not escr_get_token (e, tk)        {get I/O port name into TK}
    then escr_err_parm_missing (e, '', '', nil, 0);
  string_downcase (tk);                {make lower case port register name}
  portl := tk.str[5];                  {extract lower case a-z port letter}
  if                                   {invalid port register name ?}
      (tk.len <> 5) or                 {not right length for "PORTx"}
      (tk.str[1] <> 'p') or            {does not start with "port"}
      (tk.str[2] <> 'o') or
      (tk.str[3] <> 'r') or
      (tk.str[4] <> 't') or
      (portl < 'a') or (portl > 'z')   {not a valid port letter ?}
      then begin
    escr_err_parm_last_bad (e);
    end;
  portu := string_upcase_char (portl); {make upper case port letter}
{
*   Get bit number parameter.
}
  if not escr_get_int (e, im) then begin {get the bit number}
    escr_err_parm_missing (e, '', '', nil, 0);
    end;
  bit := im;                           {get the bit number into BIT}
  if bit < 0 then escr_err_parm_last_bad (e); {negative bit numbers not allowed}
  case e.lang of
escr_lang_aspic_k: begin               {MPASM}
      if bit > 7 then escr_err_parm_last_bad (e); {bit value too large ?}
      end;
escr_lang_dspic_k: begin               {ASM30}
      if bit > 15 then escr_err_parm_last_bad (e); {bit value too large ?}
      end;
otherwise
    escr_err_lang (e, e.lang, 'ESCR_CMD_INBIT', 1);
    end;
  string_f_int (strbit, bit);          {make bit number string}
{
*   Get optional PUP parameter.
}
  escr_get_keyword (e, 'PUP', pick);   {pick the keyword from the list}
  case pick of
1:  pup := true;
otherwise
    pup := false;
    end;

  escr_get_end (e);                    {no more parameters allowed}
{
*   Create the preprocessor constant Portdata_<port><bit> where PORT is the
*   single letter port name and BIT is the 0-N number of this bit within the
*   port.
}
  syname.len := 0;                     {build the constant name}
  string_appends (syname, 'Portdata_'(0));
  string_append1 (syname, portl);
  string_append (syname, strbit);

  escr_sym_find (e, syname, sym_p);    {find constant if it already exists}
  if sym_p <> nil then begin           {already exists ?}
    escr_sym_del (e, sym_p);           {delete it}
    end;

  tk.len := 0;                         {build the constant string value}
  string_append_token (tk, name);      {add name of this I/O pin}
  string_append_token (tk, string_v('IN'(0))); {this is a input bit}
  string_append_token (tk, string_v('POS'(0))); {all INBIT positive logic for now}

  escr_sym_new_const (e,               {create the constant}
    syname,                            {name of the constant}
    escr_dtype_str_k,                  {value will be a string}
    tk.len,                            {string length}
    true,                              {make this new symbol global}
    sym_p);                            {returned pointer to the new symbol}
  string_copy (tk, sym_p^.const_val.str); {set the constant's value}
{
*   Create the preprocessor constant
*
*     Inbit_<name>_port
*
*   This is a string constant that contains the upper case name of the port.
*   For example, if the bit is within Port B, then the value of this constant
*   would be "B".
}
  syname.len := 0;                     {build the constant name}
  string_appends (syname, 'Inbit_'(0));
  string_append (syname, namel);
  string_appends (syname, '_port'(0));

  escr_sym_find (e, syname, sym_p);    {find constant if it already exists}
  if sym_p <> nil then begin           {already exists ?}
    sys_msg_parm_vstr (msg_parm[1], name);
     escr_err_atline (e, 'pic', 'err_inbit_dup', msg_parm, 1); {bomb with error message}
    end;

  tk.len := 0;                         {build the string value}
  string_append1 (tk, portu);

  escr_sym_new_const (e,               {create the constant}
    syname,                            {name of the constant}
    escr_dtype_str_k,                  {value will be a string}
    tk.len,                            {string length}
    true,                              {make this new symbol global}
    sym_p);                            {returned pointer to the new symbol}
  string_copy (tk, sym_p^.const_val.str); {set the constant's value}
{
*   Create the preprocessor constant
*
*     Inbit_<name>_bit
*
*   This is a integer constant set to the bit number of this inbit within its
*   port register.
}
  syname.len := 0;                     {build the constant name}
  string_appends (syname, 'Inbit_'(0));
  string_append (syname, namel);
  string_appends (syname, '_bit'(0));

  escr_sym_find (e, syname, sym_p);    {find constant if it already exists}
  if sym_p <> nil then begin           {already exists ?}
    sys_msg_parm_vstr (msg_parm[1], name);
     escr_err_atline (e, 'pic', 'err_inbit_dup', msg_parm, 1); {bomb with error message}
    end;

   escr_sym_new_const (e,              {create the constant}
    syname,                            {name of the constant}
    escr_dtype_int_k,                  {value will be integer}
    0,                                 {unused for integer data type}
    true,                              {make this new symbol global}
    sym_p);                            {returned pointer to the new symbol}

  sym_p^.const_val.int := bit;         {set the value of the new constant}

  case e.lang of                       {what is the input source language ?}
{
********************
*
*   Input source language is MPASM.
}
escr_lang_aspic_k: begin
{
*   <name>_reg equ portx
}
  string_append (e.obuf, name);
  string_appends (e.obuf, '_reg equ port'(0));
  string_append1 (e.obuf, portl);
  escr_write_obuf (e);
{
*     ifdef trisx
*   <name>_tris equ trisx
*       endif
}
  string_appends (e.obuf, '  ifdef tris'(0));
  string_append1 (e.obuf, portl);
  escr_write_obuf (e);

  string_append (e.obuf, name);
  string_appends (e.obuf, '_tris equ tris'(0));
  string_append1 (e.obuf, portl);
  escr_write_obuf (e);

  string_appends (e.obuf, '    endif'(0));
  escr_write_obuf (e);
{
*   <name>_bit equ <bit>
}
  string_append (e.obuf, name);
  string_appends (e.obuf, '_bit equ '(0));
  string_append (e.obuf, strbit);
  escr_write_obuf (e);
{
*   val_trisx set val_trisx | (1 << <bit>)
}
  string_appends (e.obuf, 'val_tris'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ' set val_tris'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ' | (1 << '(0));
  string_append (e.obuf, strbit);
  string_appends (e.obuf, ')'(0));
  escr_write_obuf (e);
{
*   Update VAL_PULLUPx according to the PUP parameter.
}
  if pup
    then begin                         {pullup enabled}
      {
      *   val_pullupx set val_pullupx | (1 << <bit>)
      }
      string_appends (e.obuf, 'val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, ' set val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, '  | (1 << '(0));
      string_append (e.obuf, strbit);
      string_appends (e.obuf, ')'(0));
      end
    else begin                         {pullup disabled}
      {
      *   val_pullupx set val_pullupx & ~(1 << <bit>)
      }
      string_appends (e.obuf, 'val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, ' set val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, '  & ~(1 << '(0));
      string_append (e.obuf, strbit);
      string_appends (e.obuf, ')'(0));
      end
    ;
  escr_write_obuf (e);
{
*   #define <name>_pin portx,<bit>
}
  string_appends (e.obuf, '#define '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_pin port'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ',');
  string_append (e.obuf, strbit);
  escr_write_obuf (e);
{
*     ifdef latx
*   <name>_lat equ latx
*   #define <name>_pinlat latx,<bit>
*       endif
}
  string_appends (e.obuf, '  ifdef lat'(0));
  string_append1 (e.obuf, portl);
  escr_write_obuf (e);

  string_append (e.obuf, name);
  string_appends (e.obuf, '_lat equ lat'(0));
  string_append1 (e.obuf, portl);
  escr_write_obuf (e);

  string_appends (e.obuf, '#define '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_pinlat lat'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ','(0));
  string_append (e.obuf, strbit);
  escr_write_obuf (e);

  string_appends (e.obuf, '    endif'(0));
  escr_write_obuf (e);
  end;                                 {end of MPASM language case}
{
********************
*
*   Input source language is ASM30.
}
escr_lang_dspic_k: begin
{
*   .equ <name>_reg, _PORTx
}
  string_appends (e.obuf, '.equ '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_reg, _PORT'(0));
  string_append1 (e.obuf, portu);
  escr_write_obuf (e);
{
*   .equ <name>_tris, _TRISx
}
  string_appends (e.obuf, '.equ '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_tris, _TRIS'(0));
  string_append1 (e.obuf, portu);
  escr_write_obuf (e);
{
*   .equ <name>_bit, <bit>
}
  string_appends (e.obuf, '.equ '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_bit, '(0));
  string_append (e.obuf, strbit);
  escr_write_obuf (e);
{
*   .equ <name>_lat, _LATx
}
  string_appends (e.obuf, '.equ '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_lat, _LAT'(0));
  string_append1 (e.obuf, portu);
  escr_write_obuf (e);
{
*   .set val_trisx, val_trisx | (1 << <bit>)
}
  string_appends (e.obuf, '.set val_tris'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ', val_tris'(0));
  string_append1 (e.obuf, portl);
  string_appends (e.obuf, ' | (1 << '(0));
  string_append (e.obuf, strbit);
  string_appends (e.obuf, ')'(0));
  escr_write_obuf (e);
{
*   Update VAL_PULLUPx according to the PUP parameter.
}
  if pup
    then begin                         {pullup enabled}
      {
      *   .set val_pullupx, val_pullupx | (1 << <bit>)
      }
      string_appends (e.obuf, '.set val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, ', val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, '  | (1 << '(0));
      string_append (e.obuf, strbit);
      string_appends (e.obuf, ')'(0));
      end
    else begin                         {pullup disabled}
      {
      *   .set val_pullupx, val_pullupx & ~(1 << <bit>)
      }
      string_appends (e.obuf, '.set val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, ', val_pullup'(0));
      string_append1 (e.obuf, portl);
      string_appends (e.obuf, '  & ~(1 << '(0));
      string_append (e.obuf, strbit);
      string_appends (e.obuf, ')'(0));
      end
    ;
  escr_write_obuf (e);

(*
{
*   #define <name>_pin _PORTx,<bit>
}
  string_appends (e.obuf, '#define '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_pin _PORT'(0));
  string_append1 (e.obuf, portu);
  string_appends (e.obuf, ',');
  string_append (e.obuf, strbit);
  escr_write_obuf (e);
{
*   #define <name>_pinlat _LATx,<bit>
}
  string_appends (e.obuf, '#define '(0));
  string_append (e.obuf, name);
  string_appends (e.obuf, '_pinlat _LAT'(0));
  string_append1 (e.obuf, portu);
  string_appends (e.obuf, ',');
  string_append (e.obuf, strbit);
  escr_write_obuf (e);
*)

  end;                                 {end of ASM30 language case}
{
********************
*
*   Unexpected input source file language.
}
otherwise
    escr_err_lang (e, e.lang, 'ESCR_CMD_INBIT', 2);
    end;
  end;
