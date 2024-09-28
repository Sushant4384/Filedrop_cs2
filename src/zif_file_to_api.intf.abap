INTERFACE zif_file_to_api
  PUBLIC .

  TYPES:
    BEGIN OF ty_comm_str,
      url         TYPE string,
      content_typ TYPE string,
    END OF ty_comm_str,

    BEGIN OF ty_comm_params,
      name  TYPE string,
      value TYPE string,
    END OF ty_comm_params,

    tty_comm_params TYPE STANDARD TABLE OF ty_comm_params.

  CONSTANTS:
    BEGIN OF content_type,
      appl_json  TYPE string VALUE 'application/json',
      text_plain TYPE string VALUE 'text/plain',
    END OF content_type.

  METHODS convert_data
    RETURNING VALUE(r_conv_data) TYPE xstring
    RAISING   zcx_file_drop.

  METHODS open_connection
    RETURNING VALUE(r_http_client) TYPE REF TO if_web_http_client
    RAISING   zcx_file_drop.

  METHODS set_header_params
    RAISING zcx_file_drop.

  METHODS send_request_get_response
    RETURNING VALUE(r_response) TYPE string
    RAISING   zcx_file_drop.

  METHODS close_connection
    RAISING zcx_file_drop.


ENDINTERFACE.
