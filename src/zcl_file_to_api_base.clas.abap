CLASS zcl_file_to_api_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_file_to_api .

** Constructor
    METHODS constructor
      IMPORTING
        !i_comm_str    TYPE zif_file_to_api=>ty_comm_str
        !i_query_param TYPE zif_file_to_api=>tty_comm_params
        !i_head_param  TYPE zif_file_to_api=>tty_comm_params
        !i_data_in     TYPE REF TO data.

** Send data to API
    METHODS send
      EXPORTING
                !e_response TYPE any
      RAISING   zcx_file_drop.

** Factory method to get instance
    CLASS-METHODS factory
      IMPORTING
                !i_comm_str       TYPE zif_file_to_api=>ty_comm_str
                !i_query_param    TYPE zif_file_to_api=>tty_comm_params
                !i_head_param     TYPE zif_file_to_api=>tty_comm_params
                !i_data_in        TYPE REF TO data
      RETURNING VALUE(r_instance) TYPE REF TO zcl_file_to_api_base
      RAISING   zcx_file_drop.

  PROTECTED SECTION.
** Alias names for interface methods
    ALIASES close_connection FOR zif_file_to_api~close_connection.
    ALIASES convert_data FOR zif_file_to_api~convert_data.
    ALIASES open_connection FOR zif_file_to_api~open_connection.
    ALIASES send_request_get_response FOR zif_file_to_api~send_request_get_response.
    ALIASES set_header_params FOR zif_file_to_api~set_header_params.

    DATA: ms_comm_str         TYPE zif_file_to_api=>ty_comm_str,
          mt_comm_query_param TYPE zif_file_to_api=>tty_comm_params,
          mt_comm_head_param  TYPE zif_file_to_api=>tty_comm_params,
          mo_http_client      TYPE REF TO if_web_http_client,
          m_data_in           TYPE REF TO data,
          m_data_out          TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: base_class_name TYPE string VALUE 'ZCL_FILE_TO_API_',
               BEGIN OF type_name,
                 json TYPE string VALUE 'JSON',
                 text TYPE string VALUE 'TEXT',
               END OF type_name.
ENDCLASS.



CLASS zcl_file_to_api_base IMPLEMENTATION.

  METHOD factory.

    DATA: lv_class_name TYPE string.

** Decide subclass instance to be called based on content type
    IF i_comm_str-content_typ = zif_file_to_api=>content_type-appl_json.
      lv_class_name = zcl_file_to_api_base=>base_class_name && zcl_file_to_api_base=>type_name-json.
    ELSE.
      lv_class_name = zcl_file_to_api_base=>base_class_name && zcl_file_to_api_base=>type_name-text.
    ENDIF.

** Create instance of the subclass and return
    TRY.
        CREATE OBJECT r_instance TYPE (lv_class_name)
           EXPORTING
             i_comm_str = i_comm_str
             i_head_param = i_head_param
             i_query_param = i_query_param
             i_data_in = i_data_in.
      CATCH cx_sy_create_object_error INTO DATA(lo_object_error).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_object_error )->t100key ).
    ENDTRY.

  ENDMETHOD.

  METHOD constructor.
** Store all data in instance
    ms_comm_str = i_comm_str.
    mt_comm_head_param = i_head_param.
    mt_comm_query_param = i_query_param.
    m_data_in = i_data_in.
  ENDMETHOD.

  METHOD send.

** Get converted data
    TRY.
        convert_data(
          RECEIVING
            r_conv_data = m_data_out
        ).

        CHECK m_data_out IS NOT INITIAL.

** Open connection to http
        open_connection(
          RECEIVING
            r_http_client = mo_http_client
        ).

        CHECK mo_http_client IS BOUND.

** set header parameters
        set_header_params( ).

** Push the request and get the response
        send_request_get_response(
          RECEIVING
            r_response = DATA(lv_response)
        ).

** Convert to exporting structure
        xco_cp_json=>data->from_string( lv_response )->apply( VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) ) )->write_to( REF #( e_response ) ).

** Close connection
        close_connection(  ).

      CATCH zcx_file_drop INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD close_connection.

    CHECK mo_http_client IS BOUND.

    TRY.
        mo_http_client->close( ).
      CATCH cx_web_http_client_error INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_exception )->t100key ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_data.

  ENDMETHOD.


  METHOD open_connection.

** Add query parameters
    LOOP AT mt_comm_query_param INTO DATA(ls_query_pqram).
      IF sy-tabix = 1.
        CONCATENATE ms_comm_str-url '?' ls_query_pqram-name '=' ls_query_pqram-value INTO ms_comm_str-url.
      ELSE.
        CONCATENATE ms_comm_str-url '&' ls_query_pqram-name '=' ls_query_pqram-value INTO ms_comm_str-url.
      ENDIF.
    ENDLOOP.
** Get destination instance
    TRY.
        cl_http_destination_provider=>create_by_url(
          EXPORTING
            i_url              = ms_comm_str-url
          RECEIVING
            r_http_destination = DATA(lo_dest)
        ).
** Get http client manager
        cl_web_http_client_manager=>create_by_http_destination(
          EXPORTING
            i_destination = lo_dest
          RECEIVING
            r_client      = r_http_client
        ).

        r_http_client->get_http_request(  )->set_content_type( content_type = ms_comm_str-content_typ ).

      CATCH cx_web_http_client_error INTO DATA(lo_client_error).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_client_error )->t100key ).
      CATCH cx_http_dest_provider_error INTO DATA(lo_provider_error).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_provider_error )->t100key ).
    ENDTRY.
  ENDMETHOD.


  METHOD send_request_get_response.

    TRY.
** Set the content
        mo_http_client->get_http_request(  )->set_binary( m_data_out ).

** Execute the request
        DATA(lo_resp) = mo_http_client->execute( if_web_http_client=>post ).

** Get the status and response
        lo_resp->get_status(
          RECEIVING
            r_value = DATA(ls_status)
        ).
        IF ls_status-code BETWEEN 200 AND 299.
          lo_resp->get_text(
            RECEIVING
              r_value = r_response
          ).
        ENDIF.
      CATCH cx_web_http_client_error INTO DATA(lo_client_error).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_client_error )->t100key ).
      CATCH cx_web_message_error INTO DATA(lo_msg_Error).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_msg_error )->t100key ).
    ENDTRY..
  ENDMETHOD.


  METHOD set_header_params.

    CHECK mt_comm_head_param IS NOT INITIAL.

    TRY.
        LOOP AT mt_comm_head_param INTO DATA(ls_head_param).
          mo_http_client->get_http_request(  )->set_header_field(
            EXPORTING
              i_name  = ls_head_param-name
              i_value = ls_head_param-value
          ).

        ENDLOOP.
      CATCH cx_web_message_error INTO DATA(lo_exception).
        RAISE EXCEPTION NEW zcx_file_drop( textid = cl_message_helper=>get_latest_t100_exception( lo_exception )->t100key ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
