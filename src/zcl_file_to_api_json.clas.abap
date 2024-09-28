CLASS zcl_file_to_api_json DEFINITION
  PUBLIC
  INHERITING FROM zcl_file_to_api_base
  FINAL
  CREATE PROTECTED GLOBAL FRIENDS zcl_file_to_api_base.

  PUBLIC SECTION.
    METHODS zif_file_to_api~convert_data REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_file_to_api_json IMPLEMENTATION.

  METHOD zif_file_to_api~convert_data.

    FIELD-SYMBOLS: <fs_data_in> TYPE any.

    ASSIGN m_data_in->* TO <fs_data_in>.

    CHECK <fs_data_in> IS ASSIGNED.

    DATA(lv_json_str) = xco_cp_json=>data->from_abap( <fs_data_in> )->apply( VALUE #( ( xco_cp_json=>transformation->underscore_to_camel_case ) ) )->to_string( ).

    r_conv_data = xco_cp=>string( lv_json_str )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.


  ENDMETHOD.

ENDCLASS.
