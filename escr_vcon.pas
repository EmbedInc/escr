{   Routines that manipulate the data struction ESCR_VCON_T, which defines the
*   value of a variable or constant.
}
module escr_vcon;
define escr_vcon_init;
define escr_vcon_set;
define escr_vcon_val;
%include 'escr2.ins.pas';
{
********************************************************************************
*
*   Subroutine ESCR_VCON_INIT (E, VCON, DTYPE)
*
*   Initialize the variable/constant value VCON to the data type DTYPE.  VCON is
*   set to the default value for its data type.
}
procedure esrc_vcon_init (             {init variable/constant data descriptor}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  out     vcon: escr_vcon_t;           {set to default value for the data type}
  in      dtype: escr_dtype_k_t);      {data type of the variable or constant}
  val_param;

begin
  vcon.dtype := dtype;                 {set the data type}
  case dtype of                        {what data type is it ?}

escr_dtype_bool_k: begin               {BOOL}
      vcon.bool := false;
      end;

escr_dtype_int_k: begin                {INTEGER}
      vcon.int := 0;
      end;

escr_dtype_fp_k: begin                 {REAL}
      vcon.fp := 0.0;
      end;

escr_dtype_str_k: begin                {STRING}
      strflex_str_create (             {create flex string}
        e.sfmem,                       {flex string memory state}
        vcon.stf);                     {newly initialized flex string}
      end;

escr_dtype_time_k: begin               {TIME}
      vcon.time := sys_clock_from_fp_rel (0.0);
      end;

otherwise
    escr_err_dtype_unimp (e, dtype, 'ESCR_VCON_INIT');
    end;                               {end of output data type cases}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_VCON_SET (E, VCON, VAL, STAT)
*
*   Set the value of VCON from VAL.  VCON must be previously initialized.  The
*   data type of VAL must be convertable to the data type of VCON.
}
procedure escr_vcon_set (              {set VCON to a value}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in out  vcon: escr_vcon_t;           {set to new value, must be initialized}
  in      val: escr_val_t;             {input val, must be convertable to VCON dtype}
  out     stat: sys_err_t);            {completion status, error on dtype mismatch}
  val_param;

var
  val2: escr_val_t;                    {VAL with same data type as target}
  val_p: escr_val_p_t;                 {pointer to source value descriptor}

begin
  sys_error_none (stat);               {init to no error}
  val_p := addr(val);                  {init to use VAL directly}

  if val.dtype <> vcon.dtype then begin {need to convert to target data type ?}
    escr_val_copy (e, val, val2, stat); {convert to target data type in VAL2}
    if sys_error(stat) then return;
    val_p := addr(val2);               {copy from VAL2, not VAL}
    end;

  case vcon.dtype of                   {which data type is it ?}

escr_dtype_bool_k: begin               {BOOL}
      vcon.bool := val_p^.bool;
      end;

escr_dtype_int_k: begin                {INTEGER}
      vcon.int := val_p^.int;
      end;

escr_dtype_fp_k: begin                 {REAL}
      vcon.fp := val_p^.fp;
      end;

escr_dtype_str_k: begin                {STRING}
      strflex_copy_f_vstr (val_p^.str, vcon.stf); {copy the string data}
      end;

escr_dtype_time_k: begin               {TIME}
      vcon.time := val_p^.time;
      end;

otherwise
    escr_err_dtype_unimp (e, vcon.dtype, 'ESCR_VCON_SET');
    end;                               {end of output data type cases}
  end;
{
********************************************************************************
*
*   Subroutine ESCR_VCON_VAL (E, VCON, VAL)
*
*   Set VAL to the data type and value of VCON.
}
procedure escr_vcon_val (              {convert VCON to VAL}
  in out  e: escr_t;                   {state for this use of the ESCR system}
  in      vcon: escr_vcon_t;           {input value}
  out     val: escr_val_t);            {returned value}
  val_param;

begin
  val.dtype := vcon.dtype;             {copy the data type}
  case vcon.dtype of                   {which data type is it ?}

escr_dtype_bool_k: begin               {BOOL}
    val.bool := vcon.bool;
    end;

escr_dtype_int_k: begin                {INTEGER}
    val.int := vcon.int;
    end;

escr_dtype_fp_k: begin                 {REAL}
    val.fp := vcon.fp;
    end;

escr_dtype_str_k: begin                {STRING}
    val.str.max := size_char(val.str.str); {init var string}
    strflex_copy_t_vstr (vcon.stf, val.str); {copy the string data}
    end;

escr_dtype_time_k: begin               {TIME}
    val.time := vcon.time;
    end;

otherwise
    escr_err_dtype_unimp (e, vcon.dtype, 'ESCR_VCON_VAL');
    end;                               {end of output data type cases}
  end;
