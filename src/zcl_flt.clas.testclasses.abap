*"* use this source file for your ABAP unit test classes
class ltcl_test definition for testing
  duration short
  risk level harmless final.

  private section.
    data: flt type ref to zcl_flt.

    methods: setup
           , teardown
           , add_value_eq        for testing
           , add_value_bt        for testing
           , add_range_eq        for testing
           , add_multi_field     for testing
           , remove_field        for testing
           , refresh_cond        for testing
           , push_pop_cond       for testing
           , where_string_sql    for testing
           , where_string_abap   for testing
           , st_range_from_value for testing
           , operator_alias      for testing.
endclass.

class ltcl_test implementation.
  method setup.
    flt = new #( ).
  endmethod.

  method teardown.
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

  method add_range_eq.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'EQ' low = '0001' ) ).
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'MATNR' ) msg = 'Debe contener el campo' ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs '0001' )  msg = 'Debe contener el valor' ).
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

  method where_string_abap.
    data: lt_range type range of matnr.
    lt_range = value #( ( sign = 'I' option = 'CP' low = 'ABC*' ) ).
    flt->add_range( field = 'MATNR' range = lt_range ).
    data(lv_abap) = flt->get_where_string_abap( ).
    cl_abap_unit_assert=>assert_true(  act = boolc( lv_abap cs 'MATNR' ) msg = 'ABAP WHERE debe contener el campo' ).
    cl_abap_unit_assert=>assert_false( act = boolc( lv_abap cs '%' )     msg = 'ABAP WHERE no debe contener % de SQL' ).
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

  method operator_alias.
    data(lv_low) = '0001'.
    flt->add_value( field = 'MATNR' option = '=' low = lv_low ).
    data(lv_where) = flt->get_where_string( ).
    cl_abap_unit_assert=>assert_true( act = boolc( lv_where cs 'EQ' ) msg = 'Operador = debe normalizarse a EQ' ).
  endmethod.
endclass.
