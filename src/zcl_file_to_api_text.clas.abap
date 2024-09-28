CLASS zcl_file_to_api_text DEFINITION
  PUBLIC
  INHERITING FROM zcl_file_to_api_base
  FINAL
  CREATE PROTECTED GLOBAL FRIENDS zcl_file_to_api_base.

  PUBLIC SECTION.
    METHODS zif_file_to_api~convert_data REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_file_to_api_text IMPLEMENTATION.

  METHOD zif_file_to_api~convert_data.

*    Convert internal table data to string format and then xstring


  ENDMETHOD.

ENDCLASS.
