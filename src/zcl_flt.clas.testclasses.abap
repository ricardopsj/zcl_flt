*"* use this source file for your ABAP unit test classes
class ltcl_test definition deferred.
class zcl_flt definition local friends ltcl_test.

class ltcl_test definition for testing
  duration short
  risk level harmless final.

  private section.
    data: flt type ref to zcl_flt.

    "Lee la primera línea del rango almacenado en t_cond[ iv_index ]
    methods get_stored_range
      importing
        iv_index  type i default 1
      exporting
        ev_sign   type ddsign
        ev_option type ddoption
        ev_low    type string
        ev_high   type string.

    methods: setup
           , teardown
           , add_value_eq        for testing
           , add_value_bt        for testing
           , add_value_high_only for testing
           , add_range_eq        for testing
           , add_range_empty     for testing
           , add_ref_range_direct for testing
           , add_ref_value_low_only for testing
           , add_ref_value_high_only for testing
           , add_ref_value_low_high for testing
           , add_multi_field     for testing
           , remove_field        for testing
           , remove_absent_field for testing
           , remove_duplicated_field for testing
           , refresh_cond        for testing
           , push_pop_cond       for testing
           , push_pop_multi_level for testing
           , pop_cond_empty_stack for testing
           , where_string_sql    for testing
           , where_string_sql_ops for testing
           , where_string_sql_nb for testing
           , where_string_abap   for testing
           , where_string_abap_np for testing
           , where_no_timestamp_decimals for testing
           , where_no_bang_equal for testing
           , where_paren_spacing for testing
           , st_range_from_value for testing
           , st_range_bt         for testing
           , st_range_accumulates for testing
           , st_range_numeric    for testing
           , operator_alias      for testing
           , operator_alias_all  for testing.
endclass.

class ltcl_test implementation.
  method setup.
    flt = new #( ).
  endmethod.

  method teardown.
  endmethod.

  method get_stored_range.
    field-symbols: <rt>   type standard table
                 , <rs>   type any
                 , <comp> type any.
    data(lr_dref) = flt->t_cond[ iv_index ]-dref.
    assign lr_dref->* to <rt>.
    read table <rt> index 1 assigning <rs>.
    cl_abap_unit_assert=>assert_subrc( act = sy-subrc msg = 'El rango almacenado debe tener al menos una línea' ).
    assign component 'SIGN' of structure <rs> to <comp>.
    ev_sign = <comp>.
    assign component 'OPTION' of structure <rs> to <comp>.
    ev_option = <comp>.
    assign component 'LOW' of structure <rs> to <comp>.
    ev_low = <comp>.
    assign component 'HIGH' of structure <rs> to <comp>.
    ev_high = <comp>.
  endmethod.

  method add_value_eq.
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' low = lv_low ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'MATNR' ) msg = 'Debe contener el nombre del campo' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'EQ' )    msg = 'Debe contener el operador EQ' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0001' )  msg = 'Debe contener el valor' ).
  endmethod.

  method add_value_bt.
    data(lv_low)  = '0001'.
    data(lv_high) = '0010'.
    flt->add_value( field = 'MATNR' option = 'BT' low = lv_low high = lv_high ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'BT' )   msg = 'Debe contener BT' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0001' ) msg = 'Debe contener valor low' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0010' ) msg = 'Debe contener valor high' ).
  endmethod.

  method add_value_high_only.
    "Rama de add_value que deriva el tipo desde HIGH cuando LOW no se suministra
    data(lv_high) = '0010'.
    flt->add_value( field = 'MATNR' option = 'LE' high = lv_high ).
    get_stored_range( importing ev_option = data(lv_option)
                                ev_low    = data(lv_low)
                                ev_high   = data(lv_stored_high) ).
    cl_abap_unit_assert=>assert_equals( act = lv_option exp = 'LE' msg = 'OPTION debe ser LE' ).
    cl_abap_unit_assert=>assert_initial( act = lv_low msg = 'LOW debe quedar inicial' ).
    cl_abap_unit_assert=>assert_equals( act = lv_stored_high exp = '0010' msg = 'HIGH debe contener el valor' ).
  endmethod.

  method add_range_eq.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'EQ' low = '0001' ) ).
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'MATNR' ) msg = 'Debe contener el campo' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0001' )  msg = 'Debe contener el valor' ).
  endmethod.

  method add_range_empty.
    "Un rango vacío no debe generar condición (ni provocar dump)
    data: lt_range type range of matnr.
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'MATNR' ) msg = 'Rango vacío no debe generar condición' ).
  endmethod.

  method add_ref_range_direct.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'EQ' low = '0001' ) ).
    flt->add_ref_range( field = 'MATNR' ref_range = ref #( lt_range ) ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'MATNR' ) msg = 'Debe contener el campo' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0001' )  msg = 'Debe contener el valor' ).
  endmethod.

  method add_ref_value_low_only.
    data lv_low type c length 4 value '0001'.
    flt->add_ref_value( field = 'MATNR' low = ref #( lv_low ) ).
    get_stored_range( importing ev_option = data(lv_option)
                                ev_low    = data(lv_stored_low)
                                ev_high   = data(lv_stored_high) ).
    cl_abap_unit_assert=>assert_equals( act = lv_option exp = 'EQ' msg = 'OPTION por defecto debe ser EQ' ).
    cl_abap_unit_assert=>assert_equals( act = lv_stored_low exp = '0001' msg = 'LOW debe contener el valor referenciado' ).
    cl_abap_unit_assert=>assert_initial( act = lv_stored_high msg = 'HIGH debe quedar inicial' ).
  endmethod.

  method add_ref_value_high_only.
    data lv_high type c length 4 value '0010'.
    flt->add_ref_value( field = 'MATNR' option = 'LE' high = ref #( lv_high ) ).
    get_stored_range( importing ev_option = data(lv_option)
                                ev_low    = data(lv_stored_low)
                                ev_high   = data(lv_stored_high) ).
    cl_abap_unit_assert=>assert_equals( act = lv_option exp = 'LE' msg = 'OPTION debe ser LE' ).
    cl_abap_unit_assert=>assert_initial( act = lv_stored_low msg = 'LOW debe quedar inicial' ).
    cl_abap_unit_assert=>assert_equals( act = lv_stored_high exp = '0010' msg = 'HIGH debe contener el valor referenciado' ).
  endmethod.

  method add_ref_value_low_high.
    data: lv_low  type c length 4 value '0001'
        , lv_high type c length 4 value '0010'.
    flt->add_ref_value( field = 'MATNR' option = 'BT' low = ref #( lv_low ) high = ref #( lv_high ) ).
    get_stored_range( importing ev_option = data(lv_option)
                                ev_low    = data(lv_stored_low)
                                ev_high   = data(lv_stored_high) ).
    cl_abap_unit_assert=>assert_equals( act = lv_option exp = 'BT' msg = 'OPTION debe ser BT' ).
    cl_abap_unit_assert=>assert_equals( act = lv_stored_low  exp = '0001' msg = 'LOW debe contener el valor referenciado' ).
    cl_abap_unit_assert=>assert_equals( act = lv_stored_high exp = '0010' msg = 'HIGH debe contener el valor referenciado' ).
  endmethod.

  method add_multi_field.
    data(lv_matnr) = '0001'.
    data(lv_werks) = '1000'.
    flt->add_value( field = 'MATNR' low = lv_matnr ).
    flt->add_value( field = 'WERKS' low = lv_werks ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'MATNR' ) msg = 'Debe contener MATNR' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'WERKS' ) msg = 'Debe contener WERKS' ).
  endmethod.

  method remove_field.
    data(lv_matnr) = '0001'.
    data(lv_werks) = '1000'.
    flt->add_value( field = 'MATNR' low = lv_matnr ).
    flt->add_value( field = 'WERKS' low = lv_werks ).
    flt->remove( 'MATNR' ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'MATNR' ) msg = 'No debe contener campo eliminado' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'WERKS' ) msg = 'Debe conservar campo restante' ).
  endmethod.

  method remove_absent_field.
    data(lv_werks) = '1000'.
    flt->add_value( field = 'WERKS' low = lv_werks ).
    flt->remove( 'MATNR' ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'WERKS' ) msg = 'Eliminar campo inexistente no debe afectar el resto' ).
  endmethod.

  method remove_duplicated_field.
    "remove debe eliminar TODAS las condiciones del campo, no solo la primera
    data(lv_low1) = '0001'.
    data(lv_low2) = '0002'.
    flt->add_value( field = 'MATNR' low = lv_low1 ).
    flt->add_value( field = 'MATNR' low = lv_low2 ).
    flt->remove( 'MATNR' ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'MATNR' ) msg = 'Deben eliminarse todas las condiciones del campo' ).
  endmethod.

  method refresh_cond.
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' low = lv_low ).
    flt->refresh( ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_initial( act = lv_where msg = 'WHERE debe estar vacío tras refresh' ).
  endmethod.

  method push_pop_cond.
    data(lv_matnr) = '0001'.
    data(lv_werks) = '1000'.
    flt->add_value( field = 'MATNR' low = lv_matnr ).
    flt->push_cond( ).
    flt->add_value( field = 'WERKS' low = lv_werks ).
    flt->pop_cond( ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'MATNR' ) msg = 'Tras pop debe contener MATNR' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'WERKS' ) msg = 'Tras pop no debe contener WERKS' ).
  endmethod.

  method push_pop_multi_level.
    data(lv_matnr) = '0001'.
    data(lv_werks) = '1000'.
    data(lv_lgort) = '0002'.
    flt->add_value( field = 'MATNR' low = lv_matnr ).
    flt->push_cond( ).
    flt->add_value( field = 'WERKS' low = lv_werks ).
    flt->push_cond( ).
    flt->add_value( field = 'LGORT' low = lv_lgort ).

    flt->pop_cond( ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'MATNR' ) msg = 'Nivel 2: debe contener MATNR' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'WERKS' ) msg = 'Nivel 2: debe contener WERKS' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'LGORT' ) msg = 'Nivel 2: no debe contener LGORT' ).

    flt->pop_cond( ).
    lv_where = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'MATNR' ) msg = 'Nivel 1: debe contener MATNR' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'WERKS' ) msg = 'Nivel 1: no debe contener WERKS' ).
  endmethod.

  method pop_cond_empty_stack.
    "Documenta el comportamiento actual: pop con pila vacía limpia las condiciones
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' low = lv_low ).
    flt->pop_cond( ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_initial( act = lv_where msg = 'Pop con pila vacía deja las condiciones vacías' ).
  endmethod.

  method where_string_sql.
    data(lv_low)  = '0001'.
    data(lv_high) = '0010'.
    flt->add_value( field = 'MATNR' option = 'EQ' low = lv_low ).
    flt->add_value( field = 'WERKS' option = 'BT' low = lv_low high = lv_high ).
    data(lv_where) = flt->get_where_string_sql( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' EQ ' )     msg = 'SQL: no debe contener EQ' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' = ' )       msg = 'SQL: debe contener =' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' BT ' )      msg = 'SQL: no debe contener BT' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' between ' ) msg = 'SQL: debe contener between' ).
  endmethod.

  method where_string_sql_ops.
    data(lv_low) = '0001'.
    flt->add_value( field = 'FLD_GE' option = 'GE' low = lv_low ).
    flt->add_value( field = 'FLD_GT' option = 'GT' low = lv_low ).
    flt->add_value( field = 'FLD_LE' option = 'LE' low = lv_low ).
    flt->add_value( field = 'FLD_LT' option = 'LT' low = lv_low ).
    flt->add_value( field = 'FLD_NE' option = 'NE' low = lv_low ).
    data(lv_where) = flt->get_where_string_sql( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' GE ' ) msg = 'SQL: no debe contener GE' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' GT ' ) msg = 'SQL: no debe contener GT' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' LE ' ) msg = 'SQL: no debe contener LE' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' LT ' ) msg = 'SQL: no debe contener LT' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' NE ' ) msg = 'SQL: no debe contener NE' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' >= ' ) msg = 'SQL: debe contener >=' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' > ' )  msg = 'SQL: debe contener >' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' <= ' ) msg = 'SQL: debe contener <=' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' < ' )  msg = 'SQL: debe contener <' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs ' <> ' ) msg = 'SQL: debe contener <>' ).
  endmethod.

  method where_string_sql_nb.
    data(lv_low)  = '0001'.
    data(lv_high) = '0010'.
    flt->add_value( field = 'MATNR' option = 'NB' low = lv_low high = lv_high ).
    data(lv_where) = flt->get_where_string_sql( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs ' NB ' )         msg = 'SQL: no debe contener NB' ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_where cs 'not between' )  msg = 'SQL: debe contener not between' ).
  endmethod.

  method where_string_abap.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'CP' low = 'ABC*' ) ).
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_abap) = flt->get_where_string_abap( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_abap cs 'MATNR' ) msg = 'ABAP WHERE debe contener el campo' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_abap cs '%' )     msg = 'ABAP WHERE no debe contener % de SQL' ).
  endmethod.

  method where_string_abap_np.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'NP' low = 'ABC*' ) ).
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_abap) = flt->get_where_string_abap( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_abap cs 'MATNR' ) msg = 'ABAP WHERE debe contener el campo' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_abap cs '%' )     msg = 'ABAP WHERE no debe contener % de SQL' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_abap cs 'LIKE' )  msg = 'ABAP WHERE no debe contener LIKE' ).
  endmethod.

  method where_no_timestamp_decimals.
    "get_where_string debe eliminar los decimales .0000000 de los timestamps
    data lv_ts type timestampl value '20260705120000.0000000'.
    flt->add_value( field = 'CREATED_AT' low = lv_ts ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs '.0000000' ) msg = 'WHERE no debe contener decimales de timestamp' ).
  endmethod.

  method where_no_bang_equal.
    "get_where_string debe convertir != a <>
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' sign = 'E' low = lv_low ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs '!=' ) msg = 'WHERE no debe contener !=' ).
  endmethod.

  method where_paren_spacing.
    "get_where_string debe corregir el espaciado de paréntesis de combine_seltabs
    data(lv_matnr) = '0001'.
    data(lv_werks) = '1000'.
    flt->add_value( field = 'MATNR' low = lv_matnr ).
    flt->add_value( field = 'WERKS' low = lv_werks ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs 'AND(' )   msg = 'WHERE no debe contener AND(' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs '(MATNR' ) msg = 'Debe haber espacio tras (' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_where cs '(WERKS' ) msg = 'Debe haber espacio tras (' ).
  endmethod.

  method st_range_from_value.
    data: lt_range type range of matnr.
    zcl_flt=>st_range_from_value( exporting sign   = 'I'
                                             option = 'EQ'
                                             low    = '0001'
                                             high   = space
                                  changing  range  = lt_range ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_range ) exp = 1     msg = 'Rango debe tener 1 entrada' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-sign   exp = 'I'    msg = 'SIGN debe ser I' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-option exp = 'EQ'   msg = 'OPTION debe ser EQ' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-low    exp = '0001' msg = 'LOW debe ser 0001' ).
  endmethod.

  method st_range_bt.
    data: lt_range type range of matnr.
    zcl_flt=>st_range_from_value( exporting sign   = 'I'
                                             option = 'BT'
                                             low    = '0001'
                                             high   = '0010'
                                  changing  range  = lt_range ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_range ) exp = 1      msg = 'Rango debe tener 1 entrada' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-option exp = 'BT'   msg = 'OPTION debe ser BT' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-low    exp = '0001' msg = 'LOW debe ser 0001' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-high   exp = '0010' msg = 'HIGH debe ser 0010' ).
  endmethod.

  method st_range_accumulates.
    "Llamadas sucesivas deben acumular entradas, no reemplazarlas
    data: lt_range type range of matnr.
    zcl_flt=>st_range_from_value( exporting sign = 'I' option = 'EQ' low = '0001' high = space
                                  changing  range = lt_range ).
    zcl_flt=>st_range_from_value( exporting sign = 'I' option = 'EQ' low = '0002' high = space
                                  changing  range = lt_range ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_range ) exp = 2      msg = 'Rango debe tener 2 entradas' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-low exp = '0001' msg = 'Primera entrada debe conservarse' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 2 ]-low exp = '0002' msg = 'Segunda entrada debe agregarse' ).
  endmethod.

  method st_range_numeric.
    data: lt_range type range of i.
    zcl_flt=>st_range_from_value( exporting sign   = 'I'
                                             option = 'BT'
                                             low    = 5
                                             high   = 10
                                  changing  range  = lt_range ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_range ) exp = 1  msg = 'Rango debe tener 1 entrada' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-low  exp = 5  msg = 'LOW debe ser 5' ).
    cl_abap_unit_assert=>assert_equals( act = lt_range[ 1 ]-high exp = 10 msg = 'HIGH debe ser 10' ).
  endmethod.

  method operator_alias.
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' option = '=' low = lv_low ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'EQ' ) msg = 'Operador = debe normalizarse a EQ' ).
  endmethod.

  method operator_alias_all.
    types: begin of lty_map
             , alias type ddoption
             , norm  type ddoption
           , end of lty_map
           , ltyt_map type standard table of lty_map with empty key.
    data(lt_map) = value ltyt_map( ( alias = '<>' norm = 'NE' )
                                   ( alias = '>=' norm = 'GE' )
                                   ( alias = '>'  norm = 'GT' )
                                   ( alias = '<=' norm = 'LE' )
                                   ( alias = '<'  norm = 'LT' ) ).
    data(lv_low) = '0001'.
    loop at lt_map into data(ls_map).
      flt->refresh( ).
      flt->add_value( field = 'MATNR' option = ls_map-alias low = lv_low ).
      get_stored_range( importing ev_option = data(lv_option) ).
      cl_abap_unit_assert=>assert_equals( act = lv_option
                                          exp = ls_map-norm
                                          msg = |Operador { ls_map-alias } debe normalizarse a { ls_map-norm }| ).
    endloop.
  endmethod.
endclass.
