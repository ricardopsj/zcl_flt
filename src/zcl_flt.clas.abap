class zcl_flt definition
  public
  final
  create public .

  public section.

    types:
      tyt_where type table of string .

    methods add_range
      importing
        !field type clike
        !range type any .
    methods add_ref_range
      importing
        !field     type clike
        !ref_range type ref to data .
    methods add_ref_value
      importing
        !field  type clike
        !sign   type ddsign default 'I'
        !option type ddoption default 'EQ'
        !low    type ref to data optional
        !high   type ref to data optional .
    methods add_value
      importing
        !field  type clike
        !sign   type ddsign default 'I'
        !option type ddoption default 'EQ'
        !low    type any optional
        !high   type any optional .
    methods get_where_string
      returning
        value(rv_where) type string .
    methods get_where_string_abap
      returning
        value(rv_where) type string .
    methods get_where_string_sql
      returning
        value(rv_where) type string .
    methods pop_cond .
    methods push_cond .
    methods refresh .
    methods remove
      importing
        !field type fieldname .
    class-methods st_range_from_value
      importing
        !sign   type ddsign default 'I'
        !option type ddoption default 'EQ'
        !low    type any
        !high   type any
      changing
        !range  type standard table .
  protected section.

*"* protected components of class ZCL_flt
*"* do not include other source files here!!!
    data t_cond type if_shdb_def=>tt_named_dref .
    data:
      t_cond_stack like table of t_cond .
    data v_cond_stack_lines like sy-tabix .
  private section.
*"* private components of class ZCL_flt
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_FLT IMPLEMENTATION.


  method add_range.
    add_ref_range( field = field ref_range = ref #( range ) ).
  endmethod.


  method add_ref_range.
    append value #( name = field dref = ref_range ) to t_cond.
  endmethod.


  method add_ref_value.
    field-symbols: <low>  type any
                   , <high> type any.
    if low is supplied.
      assign low->* to <low>.
    endif.
    if high is supplied.
      assign high->* to <high>.
    endif.
    if <low> is assigned and <high> is assigned.
      add_value( field = field sign = sign option = option low = <low> high = <high> ).
    elseif <low> is assigned.
      add_value( field = field sign = sign option = option low = <low> ).
    else.
      add_value( field = field sign = sign option = option high = <high> ).
    endif.
  endmethod.


  method add_value.
    data: ref_ty_low type ref to cl_abap_typedescr.
    if low is supplied.
      ref_ty_low ?= cl_abap_elemdescr=>describe_by_data( low ).
    elseif high is supplied.
      ref_ty_low ?= cl_abap_elemdescr=>describe_by_data( high ).
    endif.

    data: ls_components_table type line of cl_abap_structdescr=>component_table
          , lt_components_table type cl_abap_structdescr=>component_table.

    ls_components_table-name = 'SIGN'.
    ls_components_table-type ?= cl_abap_typedescr=>describe_by_data( sign ).
    append ls_components_table to lt_components_table.

    ls_components_table-name = 'OPTION'.
    ls_components_table-type ?= cl_abap_typedescr=>describe_by_data( option ).
    append ls_components_table to lt_components_table.

    ls_components_table-name = 'LOW'.
    ls_components_table-type ?= ref_ty_low.
    append ls_components_table to lt_components_table.

    ls_components_table-name = 'HIGH'.
    ls_components_table-type ?= ref_ty_low.
    append ls_components_table to lt_components_table.

    data: ref_ty_rs_range type ref to cl_abap_structdescr
          , ref_ty_rt_range type ref to cl_abap_tabledescr.

    ref_ty_rs_range = cl_abap_structdescr=>get( lt_components_table ).
    ref_ty_rt_range = cl_abap_tabledescr=>get( p_line_type = ref_ty_rs_range ).
    data: ref_rs_range type ref to data
          , ref_rt_range type ref to data.
    create data ref_rs_range type handle ref_ty_rs_range.
    create data ref_rt_range type handle ref_ty_rt_range.

    field-symbols: <rs> type any
                 , <rt> type standard table
                 , <sign> type any
                 , <option> type any
                 , <low> type any
                 , <high> type any.
    assign ref_rs_range->* to <rs>.
    assign component 'SIGN' of structure <rs> to <sign>.
    assign component 'OPTION' of structure <rs> to <option>.
    assign component 'LOW' of structure <rs> to <low>.
    assign component 'HIGH' of structure <rs> to <high>.
    <sign> = sign.
    <option> = option.
    case <option>.
      when '='.
        <option> = 'EQ'.
      when '<>'.
        <option> = 'NE'.
      when '>='.
        <option> = 'GE'.
      when '>'.
        <option> = 'GT'.
      when '<='.
        <option> = 'LE'.
      when '<'.
        <option> = 'LT'.
    endcase.
    if low is supplied.
      <low> = low.
    endif.
    if high is supplied.
      <high> = high.
    endif.

    assign ref_rt_range->* to <rt>.

    append <rs> to <rt>.

    add_range( field = field range = <rt> ).
  endmethod.


  method get_where_string.
    try.
        rv_where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = t_cond ).
      catch: cx_shdb_exception into data(ref_cx_shdb_exception).
    endtry.
    replace all occurrences of regex '\.0000000\)' in rv_where with ')'. "Timestamp
    replace all occurrences of 'AND(' in rv_where with 'AND ('. "Error de programación de cl_shdb_seltab=>combine_seltabs
    replace all occurrences of regex '([^ ])\)' in rv_where with '$1 )'.
    replace all occurrences of regex '\(([^ ])' in rv_where with '( $1'.
    replace all occurrences of '))' in rv_where with ') )'.
    replace all occurrences of ' != ' in rv_where with ' <> '.
  endmethod.


  method get_where_string_abap.
    rv_where = get_where_string( ).
    replace all occurrences of regex 'NOT LIKE( *''.*)%''' in rv_where with 'NP$1*'''.
    replace all occurrences of regex 'LIKE( *''.*)%''' in rv_where with 'CP$1*'''.
  endmethod.


  method get_where_string_sql.
    rv_where = get_where_string( ).
    replace all occurrences of ' EQ ' in rv_where with ' = '.
    replace all occurrences of ' NE ' in rv_where with ' <> '.
    replace all occurrences of ' BT ' in rv_where with ' between '.
    replace all occurrences of ' NB ' in rv_where with ' not between '.
    replace all occurrences of ' GE ' in rv_where with ' >= '.
    replace all occurrences of ' GT ' in rv_where with ' > '.
    replace all occurrences of ' LE ' in rv_where with ' <= '.
    replace all occurrences of ' LT ' in rv_where with ' < '.
  endmethod.


  method pop_cond.
    refresh t_cond.
    if v_cond_stack_lines ge 1.
      read table t_cond_stack index v_cond_stack_lines into t_cond.
      delete t_cond_stack index v_cond_stack_lines.
      subtract 1 from v_cond_stack_lines.
    endif.
  endmethod.


  method push_cond.
    append t_cond to t_cond_stack.
    v_cond_stack_lines = sy-tabix.
  endmethod.


  method refresh.
    refresh me->t_cond.
  endmethod.


  method remove.
    delete t_cond where name eq field.
  endmethod.


  method st_range_from_value.
    data: ref_def_table type ref to cl_abap_tabledescr
        , ref_def_line type ref to cl_abap_structdescr
        , ref_line type ref to data.

    ref_def_table ?= cl_abap_tabledescr=>describe_by_data( range ).
    ref_def_line ?= ref_def_table->get_table_line_type( ).
    create data ref_line type handle ref_def_line.

    field-symbols: <ls> type any
                 , <option> type any
                 , <sign> type any
                 , <low> type any
                 , <high> type any.
    assign ref_line->* to <ls>.
    assign component 'SIGN' of structure <ls> to <sign>.
    assign component 'OPTION' of structure <ls> to <option>.
    assign component 'LOW' of structure <ls> to <low>.
    assign component 'HIGH' of structure <ls> to <high>.
    <sign> = sign.
    <option> = option.
    <low> = low.
    <high> = high.

    append <ls> to range.
  endmethod.
ENDCLASS.
