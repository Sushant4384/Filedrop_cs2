CLASS zcl_file_to_api_tester DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_file_to_api_tester IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    TYPES: BEGIN OF ty_response,
             file_name   TYPE string,
             file_ext(4) TYPE c,
             file_size   TYPE int4,
             file_id     TYPE string,
           END OF ty_response.

    DATA: ls_comm_str    TYPE zif_file_to_api=>ty_comm_str,
          ls_response    TYPE ty_response,
          lv_data        TYPE REF TO data,
          lt_query_param TYPE zif_file_to_api=>tty_comm_params,
          lt_head_param  TYPE zif_file_to_api=>tty_comm_params.

    SELECT * FROM /dmo/flight
      INTO TABLE @DATA(lt_flight) UP TO 10 ROWS.

    TRY.
        lv_data = REF #( lt_flight[] ).

        ls_comm_str = VALUE #( content_typ = zif_file_to_api=>content_type-appl_json url = 'https://v2.convertapi.com/upload' ).
        lt_query_param = VALUE #( ( name = 'Content-Disposition' value = 'inline' )
                                  ( name = 'filename' value = 'Demo_file_drop.json' ) ).

        zcl_file_to_api_base=>factory(
          EXPORTING
            i_comm_str    = ls_comm_str
            i_query_param = lt_query_param
            i_head_param  = lt_head_param
            i_data_in     = lv_data
          RECEIVING
            r_instance    = DATA(lo_file_drop)
        ).

        lo_file_drop->send(
          IMPORTING
            e_response = ls_response
        ).

        out->write( ls_response ).

      CATCH zcx_file_drop INTO DATA(lo_exception).
        out->write( lo_exception->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
