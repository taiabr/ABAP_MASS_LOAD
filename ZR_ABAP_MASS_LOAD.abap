*&---------------------------------------------------------------------*
*& Report ZR_ABAP_MASS_LOAD
*&---------------------------------------------------------------------*
*& Author: Taiã Pryor
*&---------------------------------------------------------------------*
REPORT zr_abap_mass_load.

**********************************************************************
*** Classes / Interfaces locais
** Classe de excecao
CLASS lcx_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING previous LIKE previous OPTIONAL
                textid   LIKE textid OPTIONAL
                msgtxt   TYPE string OPTIONAL
                symsg    TYPE symsg OPTIONAL .
    METHODS get_symsg
      RETURNING VALUE(symsg) TYPE symsg .
    METHODS get_text REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA symsg TYPE symsg .
    DATA txmsg TYPE string .
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous textid = textid ).
    me->txmsg = msgtxt.
    me->symsg = symsg.
    IF me->symsg-msgid IS NOT INITIAL AND me->symsg-msgty IS INITIAL.
      me->symsg-msgty = 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD get_symsg.
    CLEAR symsg.
    symsg = me->symsg.
  ENDMETHOD.
  METHOD get_text.
    CLEAR result.
    result = me->txmsg.
    CHECK result IS INITIAL.
    MESSAGE ID me->symsg-msgid TYPE me->symsg-msgty NUMBER me->symsg-msgno
      WITH me->symsg-msgv1 me->symsg-msgv2 me->symsg-msgv3 me->symsg-msgv4
      INTO result.
    CHECK result IS INITIAL.
    IF me->previous IS BOUND.
      result = me->previous->get_text( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

** Classe de log
CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_messages,
        posno   TYPE i,
        objtype TYPE trobjtype,
        objname TYPE trobj_name,
        msgtyp  TYPE msgtyp,
        msgtxt  TYPE bapi_msg,
        uname   TYPE sy-uname,
        datum   TYPE sy-datum,
        uzeit   TYPE sy-uzeit,
      END OF ty_messages,
      tt_messages TYPE STANDARD TABLE OF ty_messages WITH EMPTY KEY.

    METHODS constructor.

    METHODS add
      IMPORTING iv_objtype TYPE trobjtype
                iv_objname TYPE trobj_name
                iv_msgtyp  TYPE msgtyp
                iv_msgtxt  TYPE clike.

    METHODS set_context
      IMPORTING iv_objtype TYPE trobjtype
                iv_objname TYPE any.
    METHODS s
      IMPORTING iv_msgtxt TYPE clike.
    METHODS e
      IMPORTING iv_msgtxt TYPE clike.
    METHODS i
      IMPORTING iv_msgtxt TYPE clike.

    METHODS has_error
      RETURNING VALUE(rv_error) TYPE abap_bool.

    METHODS display.

  PRIVATE SECTION.
    DATA:
      BEGIN OF s_context,
        objtype TYPE trobjtype,
        objname TYPE trobj_name,
      END OF s_context,
      t_messages TYPE tt_messages.

    DATA:
      v_posno TYPE i.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD constructor.
    CLEAR: me->t_messages[], me->s_context.
  ENDMETHOD.

  METHOD set_context.
    CLEAR me->s_context.
    me->s_context-objtype = iv_objtype.
    me->s_context-objname = iv_objname.
  ENDMETHOD.

  METHOD add.
    APPEND INITIAL LINE TO me->t_messages[] ASSIGNING FIELD-SYMBOL(<fs_msg>).
    <fs_msg>-posno   = me->v_posno = me->v_posno + 1.
    <fs_msg>-objtype = iv_objtype.
    <fs_msg>-objname = iv_objname.
    <fs_msg>-msgtyp  = iv_msgtyp.
    <fs_msg>-msgtxt  = iv_msgtxt.
    <fs_msg>-uname   = sy-uname.
    <fs_msg>-datum   = sy-datum.
    <fs_msg>-uzeit   = sy-uzeit.
  ENDMETHOD.

  METHOD s.
    me->add( iv_objtype = me->s_context-objtype
             iv_objname = me->s_context-objname
             iv_msgtyp  = 'S'
             iv_msgtxt  = iv_msgtxt ).
  ENDMETHOD.

  METHOD e.
    me->add( iv_objtype = me->s_context-objtype
             iv_objname = me->s_context-objname
             iv_msgtyp  = 'E'
             iv_msgtxt  = iv_msgtxt ).
  ENDMETHOD.

  METHOD i.
    me->add( iv_objtype = me->s_context-objtype
             iv_objname = me->s_context-objname
             iv_msgtyp  = 'I'
             iv_msgtxt  = iv_msgtxt ).
  ENDMETHOD.

  METHOD has_error.

    rv_error = abap_false.
    LOOP AT me->t_messages ASSIGNING FIELD-SYMBOL(<fs_error>) WHERE msgtyp CA 'EAX'.
      rv_error = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lr_salv)
          CHANGING
            t_table      = me->t_messages[]
        ).

        "Seta titulo
        lr_salv->get_display_settings( )->set_list_header( sy-title ).

        "Habilita funcoes
        lr_salv->get_functions( )->set_all( if_salv_c_bool_sap=>true ).

        "Seta todas colunas como otimizada
        lr_salv->get_columns( )->set_optimize( ).

        "Seta ZEBRA
        lr_salv->get_display_settings( )->set_striped_pattern( if_salv_c_bool_sap=>true ).

        "Habilita botao de salvar layout
        DATA(lo_layout) = lr_salv->get_layout( ).
        lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
        lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).

        "Seta linha selecionavel
        lr_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        "Seta exibicao como popup
        lr_salv->set_screen_popup(
          start_column = 25
          end_column   = 150
          start_line   = 1
          end_line     = 15
        ).

        "Exibe ALV
        lr_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_except).
        MESSAGE lx_except->if_message~get_longtext( ) TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

** Classe para leitura do CSV
CLASS lcl_file DEFINITION.

  PUBLIC SECTION.
    TYPES:
      ty_filename TYPE string.   "file_table-filename.

    CONSTANTS:
      c_separator TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab.  "';'.

    DATA:
      v_filename TYPE string READ-ONLY,
      v_hasheadr TYPE abap_bool READ-ONLY.

    CLASS-METHODS f4
      CHANGING cv_filename TYPE ty_filename.

    METHODS constructor
      IMPORTING iv_filename  TYPE ty_filename
                iv_hasheader TYPE abap_bool DEFAULT abap_false
      RAISING   lcx_error.
    METHODS upload
      RAISING lcx_error.
    METHODS parse
      CHANGING ct_data TYPE ANY TABLE
      RAISING  lcx_error.

  PRIVATE SECTION.
    DATA:
      t_strtable TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS conv_date
      IMPORTING in  TYPE any
      EXPORTING out TYPE any
      RAISING   lcx_error.
    METHODS conv_dec
      IMPORTING in  TYPE any
      EXPORTING out TYPE any
      RAISING   lcx_error.
    METHODS conv_char
      IMPORTING in  TYPE any
      EXPORTING out TYPE any
      RAISING   lcx_error.
    METHODS conv_exit
      IMPORTING in  TYPE any
                typ TYPE REF TO cl_abap_typedescr
      EXPORTING out TYPE any
      RAISING   lcx_error.

ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD f4.

    DATA:
      lt_filename TYPE filetable,
      lv_rc       TYPE i.

    CLEAR cv_filename.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*       default_extension       = '*.CSV'
*       file_filter             = '*.CSV'
        multiselection          = abap_false
      CHANGING
        file_table              = lt_filename
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY .
        cv_filename = lt_filename[ 1 ]-filename.
      CATCH cx_sy_itab_line_not_found.
        CLEAR cv_filename.
    ENDTRY.

  ENDMETHOD.

  METHOD constructor.

    CLEAR: me->t_strtable[], me->v_filename.
    me->v_filename = iv_filename.
    me->v_hasheadr = iv_hasheader.

  ENDMETHOD.

  METHOD upload.

    "Importa arquivo
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = me->v_filename
        filetype                = 'ASC'    " File Type (ASCII, Binary)
      CHANGING
        data_tab                = me->t_strtable[]
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    IF me->v_hasheadr = abap_true.
      "remove linha de cabecalho
      DELETE me->t_strtable[] INDEX 1.
    ENDIF.

    "Remove linhas vazias
    DELETE me->t_strtable[] WHERE table_line IS INITIAL.

    IF me->t_strtable[] IS INITIAL.
      "Arquivo vazio
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH 'Arquivo vazio'.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD parse.

    DATA:
      lr_line TYPE REF TO data.

    CLEAR ct_data[].
    CHECK me->t_strtable[] IS NOT INITIAL.

    "Cria linha do tipo de output
    CREATE DATA lr_line LIKE LINE OF ct_data[].
    ASSIGN lr_line->* TO FIELD-SYMBOL(<fs_tbline>).
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH 'Erro ao ler arquivo'.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    LOOP AT me->t_strtable[] ASSIGNING FIELD-SYMBOL(<fs_line>).
      "Limpa estrutura para novo registro
      CLEAR <fs_tbline>.

      "Recupera valor dos campos
      SPLIT <fs_line> AT me->c_separator INTO TABLE DATA(lt_fields).

      "Preenche valores na tabela estruturada
      LOOP AT lt_fields[] ASSIGNING FIELD-SYMBOL(<fs_field>).
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_tbline> TO FIELD-SYMBOL(<fs_value>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        "Converte/Move valor
        DATA(lr_typedescr) = cl_abap_typedescr=>describe_by_data( <fs_value> ).
        CASE lr_typedescr->type_kind.
          WHEN cl_abap_typedescr=>typekind_date.    "'D'.  "data
            me->conv_date( EXPORTING in = <fs_field> IMPORTING out = <fs_value> ).
          WHEN cl_abap_typedescr=>typekind_packed.  "'P'. "decimal
            me->conv_dec( EXPORTING in = <fs_field> IMPORTING out = <fs_value> ).
          WHEN cl_abap_typedescr=>typekind_char.    "'C'. "char
            me->conv_char( EXPORTING in = <fs_field> IMPORTING out = <fs_value> ).
          WHEN OTHERS.
            me->conv_exit( EXPORTING in  = <fs_field>
                                     typ = lr_typedescr
                           IMPORTING out = <fs_value> ).
        ENDCASE.
      ENDLOOP.

      "Insere linha na tab interna
      INSERT <fs_tbline> INTO TABLE ct_data[].
    ENDLOOP.

  ENDMETHOD.

  METHOD conv_date.

    DATA:
      lv_input TYPE string.

    CLEAR out.
    CHECK in IS NOT INITIAL.

    TRY.
        "Recupera input
        lv_input = condense( in ).

        "Trata separadores
        IF lv_input CS '/'.
          TRANSLATE lv_input USING '/.'.
        ENDIF.

        CONDENSE lv_input NO-GAPS.

        "Converte data (DD.MM.AAAA -> AAAAMMDD)
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = lv_input
            accept_initial_date      = abap_true
          IMPORTING
            date_internal            = lv_input
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_error
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

        "Move valor
        out = lv_input.

      CATCH cx_sy_conversion_error INTO DATA(lrx_converr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_converr.
      CATCH cx_root INTO DATA(lrx_rooterr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_rooterr.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_dec.

    DATA:
      lv_input TYPE string.

    CLEAR out.
    CHECK in IS NOT INITIAL.

    TRY.
        "Recupera input
        lv_input = condense( in ).

        "Trata separadores
        IF lv_input CS ','.
          TRANSLATE lv_input USING ',.'.
        ENDIF.

        CONDENSE lv_input NO-GAPS.

        "Move valor
        out = lv_input.

      CATCH cx_sy_conversion_error INTO DATA(lrx_converr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_converr.
      CATCH cx_root INTO DATA(lrx_rooterr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_rooterr.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_char.

    DATA:
      lv_input TYPE string.

    CLEAR out.
    CHECK in IS NOT INITIAL.

    TRY.
        "Recupera input
        lv_input = condense( in ).

        "Move valor
        out = lv_input.

      CATCH cx_sy_conversion_error INTO DATA(lrx_converr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_converr.
      CATCH cx_root INTO DATA(lrx_rooterr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_rooterr.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_exit.

    DATA:
      lo_outtyp TYPE REF TO cl_abap_elemdescr,
      lv_input  TYPE string.

    CLEAR out.
    CHECK in IS NOT INITIAL.

    TRY.
        "Recupera input
        lv_input = condense( in ).

        "Recupera instancia do tipo
        lo_outtyp ?= typ.
        IF lo_outtyp IS INITIAL.
          lo_outtyp ?= cl_abap_typedescr=>describe_by_data( out ).
        ENDIF.

        "Recupera exit de conversao
        lo_outtyp->get_ddic_field(
          RECEIVING
            p_flddescr   = DATA(ls_fldattr)
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2
            OTHERS       = 3
        ).
        IF ls_fldattr-convexit IS NOT INITIAL.
          "Define nome da funcao da rotina de conversao
          DATA(lv_fmname) = |CONVERSION_EXIT_{ condense( ls_fldattr-convexit ) }_INPUT|.

          "Executa rotina de conversao
          CALL FUNCTION lv_fmname
            EXPORTING
              input         = lv_input
            IMPORTING
              output        = lv_input
            EXCEPTIONS
              length_error  = 1
              error_message = 2
              OTHERS        = 3 ##FM_SUBRC_OK.
        ENDIF.

        "Move valor
        out = lv_input.

      CATCH cx_sy_conversion_error INTO DATA(lrx_converr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_converr.
      CATCH cx_root INTO DATA(lrx_rooterr).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_rooterr.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

** Classe pai para criacao de objetos
CLASS lcl_object DEFINITION.

  PUBLIC SECTION.
    CONSTANTS:
      c_local_package TYPE devclass VALUE '$TMP'.

    CLASS-DATA:
      v_language TYPE sy-langu.

    CLASS-METHODS class_constructor.

    CLASS-METHODS set_package
      IMPORTING iv_objtype TYPE clike
                iv_objname TYPE clike
                iv_package TYPE devclass
                iv_request TYPE trkorr
      RAISING   lcx_error.

ENDCLASS.

CLASS lcl_object IMPLEMENTATION.

  METHOD class_constructor.
    lcl_object=>v_language = sy-langu.
  ENDMETHOD.

  METHOD set_package.

    DATA:
      lv_object TYPE string.

    CONCATENATE iv_objtype iv_objname INTO lv_object.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = lv_object
        object_class        = 'DICT'
        mode                = 'I'
        devclass            = iv_package
        korrnum             = iv_request
        master_language     = lcl_object=>v_language
        suppress_dialog     = 'X'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        error_message       = 99        " Error messages
        OTHERS              = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

** Classe filha para criacao de dominios
CLASS lcl_doma DEFINITION
    INHERITING FROM lcl_object.

  PUBLIC SECTION.
    TYPES:
      tt_dd07v TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.

    CONSTANTS:
      c_objtyp TYPE trobjtype VALUE 'DOMA'.

    CLASS-METHODS exists
      IMPORTING iv_domname       TYPE dd01v-domname
      RETURNING VALUE(rv_exists) TYPE abap_bool.

    CLASS-METHODS read
      IMPORTING iv_domname TYPE dd01v-domname
      EXPORTING es_dd01v   TYPE dd01v
                et_dd07v   TYPE tt_dd07v
      RAISING   lcx_error.

    CLASS-METHODS create
      IMPORTING is_dd01v TYPE dd01v
                it_dd07v TYPE tt_dd07v
      RAISING   lcx_error.

    CLASS-METHODS activate
      IMPORTING iv_domname TYPE dd01v-domname
      RAISING   lcx_error.

ENDCLASS.

CLASS lcl_doma IMPLEMENTATION.

  METHOD exists.

    rv_exists = abap_false.

    SELECT COUNT( * )
      FROM dd01l
      WHERE domname = iv_domname.
    CHECK sy-dbcnt > 0.

    rv_exists = abap_true.

  ENDMETHOD.

  METHOD read.

    IF lcl_doma=>exists( iv_domname ) = abap_false.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Dominio { iv_domname } nao existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = iv_domname
*       state         = 'A'
        langu         = lcl_object=>v_language
      IMPORTING
*       gotstate      =
        dd01v_wa      = es_dd01v
      TABLES
        dd07v_tab     = et_dd07v[]
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD create.

    IF lcl_doma=>exists( is_dd01v-domname ) = abap_true.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Dominio { is_dd01v-domname } ja existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = is_dd01v-domname
        dd01v_wa          = is_dd01v
      TABLES
        dd07v_tab         = it_dd07v[]
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        error_message     = 99        " Error messages
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD activate.

    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
      EXPORTING
        name          = iv_domname
*       AUTH_CHK      = 'X'
*       PRID          = -1
*     IMPORTING
*       RC            =
      EXCEPTIONS
        not_found     = 1
        put_failure   = 2
        error_message = 99        " Error messages
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

** Classe filha para criacao de elementos de dados
CLASS lcl_dtel DEFINITION
    INHERITING FROM lcl_object.

  PUBLIC SECTION.
    CONSTANTS:
      c_objtyp TYPE trobjtype VALUE 'DTEL'.

    CLASS-METHODS exists
      IMPORTING iv_rollname      TYPE dd04v-rollname
      RETURNING VALUE(rv_exists) TYPE abap_bool.

    CLASS-METHODS read
      IMPORTING iv_rollname TYPE dd04v-rollname
      EXPORTING es_dd04v    TYPE dd04v
      RAISING   lcx_error.

    CLASS-METHODS create
      IMPORTING is_dd04v TYPE dd04v
      RAISING   lcx_error.

    CLASS-METHODS activate
      IMPORTING iv_rollname TYPE dd04v-rollname
      RAISING   lcx_error.

ENDCLASS.

CLASS lcl_dtel IMPLEMENTATION.

  METHOD exists.

    rv_exists = abap_false.

    SELECT COUNT( * )
      FROM dd04l
      WHERE rollname = iv_rollname.
    CHECK sy-dbcnt > 0.

    rv_exists = abap_true.

  ENDMETHOD.

  METHOD read.

    IF lcl_dtel=>exists( iv_rollname ) = abap_false.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Elemento de dados { iv_rollname } nao existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = iv_rollname
*       state         = 'A'
        langu         = lcl_object=>v_language
      IMPORTING
*       gotstate      =
        dd04v_wa      = es_dd04v
*       tpara_wa      =
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD create.

    IF lcl_dtel=>exists( is_dd04v-rollname ) = abap_true.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Elemento de dados { is_dd04v-rollname } ja existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria elemento de dados
    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = is_dd04v-rollname
        dd04v_wa          = is_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        error_message     = 99        " Error messages
        OTHERS            = 7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD activate.

    "Ativa elemento de dados
    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name          = iv_rollname
*       AUTH_CHK      = 'X'
*       PRID          = -1
*     IMPORTING
*       RC            =
      EXCEPTIONS
        not_found     = 1
        put_failure   = 2
        error_message = 99        " Error messages
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

** Classe filha para criacao de tabelas/estruturas
CLASS lcl_tabl DEFINITION INHERITING FROM lcl_object.

  PUBLIC SECTION.
    TYPES:
      tt_dd03p TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY,
      tt_dd05m TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY,
      tt_dd08v TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
      tt_dd12v TYPE STANDARD TABLE OF dd12v WITH DEFAULT KEY,
      tt_dd17v TYPE STANDARD TABLE OF dd17v WITH DEFAULT KEY,
      tt_dd35v TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
      tt_dd36m TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY.

    CONSTANTS:
      c_objtyp TYPE trobjtype VALUE 'TABL'.

    CLASS-METHODS exists
      IMPORTING iv_tabname       TYPE dd02v-tabname
      RETURNING VALUE(rv_exists) TYPE abap_bool.

    CLASS-METHODS read
      IMPORTING iv_tabname TYPE dd02v-tabname
      EXPORTING es_dd02v   TYPE dd02v
                es_dd09l   TYPE dd09l
                et_dd03p   TYPE tt_dd03p
                et_dd05m   TYPE tt_dd05m
                et_dd08v   TYPE tt_dd08v
                et_dd12v   TYPE tt_dd12v
                et_dd17v   TYPE tt_dd17v
                et_dd35v   TYPE tt_dd35v
                et_dd36m   TYPE tt_dd36m
      RAISING   lcx_error.

    CLASS-METHODS create
      IMPORTING is_dd02v TYPE dd02v
                is_dd09l TYPE dd09l OPTIONAL
                it_dd03p TYPE tt_dd03p
                it_dd05m TYPE tt_dd05m OPTIONAL
                it_dd08v TYPE tt_dd08v OPTIONAL
                it_dd35v TYPE tt_dd35v OPTIONAL
                it_dd36m TYPE tt_dd36m OPTIONAL
      RAISING   lcx_error.

    CLASS-METHODS activate
      IMPORTING iv_tabname TYPE dd02v-tabname
      RAISING   lcx_error.

ENDCLASS.

CLASS lcl_tabl IMPLEMENTATION.

  METHOD exists.

    rv_exists = abap_false.

    SELECT COUNT( * )
      FROM dd02v
      WHERE tabname = iv_tabname.
    CHECK sy-dbcnt > 0.

    rv_exists = abap_true.
*    rv_exists = xsdbool( sy-dbcnt > 0 ).

  ENDMETHOD.

  METHOD read.

    IF lcl_tabl=>exists( iv_tabname ) = abap_false.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Tabela/Estrutura { iv_tabname } nao existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = iv_tabname
*       state         = 'A'
        langu         = lcl_object=>v_language
      IMPORTING
*       gotstate      =
        dd02v_wa      = es_dd02v
        dd09l_wa      = es_dd09l
      TABLES
        dd03p_tab     = et_dd03p[]
        dd05m_tab     = et_dd05m[]
        dd08v_tab     = et_dd08v[]
        dd12v_tab     = et_dd12v[]
        dd17v_tab     = et_dd17v[]
        dd35v_tab     = et_dd35v[]
        dd36m_tab     = et_dd36m[]
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD create.

    IF lcl_tabl=>exists( is_dd02v-tabname ) = abap_true.
      MESSAGE e001(00) INTO DATA(lv_msgtx)
        WITH |Tabela/Estrutura { is_dd02v-tabname } ja existe|.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria tabela / estrutura
    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = is_dd02v-tabname
        dd02v_wa          = is_dd02v      "Atributos
        dd09l_wa          = is_dd09l      "Caracteristicas Tecnicas p/ tab transparente
      TABLES
        dd03p_tab         = it_dd03p[]    "Campos
        dd05m_tab         = it_dd05m[]    "Campos de Chave estrangeira
        dd08v_tab         = it_dd08v[]    "Chave estrangeira
        dd35v_tab         = it_dd35v[]    "Ajudas de pesquisa
        dd36m_tab         = it_dd36m[]    "Conexoes da ajuda de pesquisa
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        error_message     = 99        " Error messages
        OTHERS            = 7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD activate.

    "Ativa estrutura
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name          = iv_tabname
*       AUTH_CHK      = 'X'
*       PRID          = -1
*     IMPORTING
*       RC            =
      EXCEPTIONS
        not_found     = 1
        put_failure   = 2
        error_message = 99        " Error messages
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

** Interface para processamento do relatorio
INTERFACE lif_process.

  METHODS get_type
    RETURNING VALUE(rv_type) TYPE trobjtype.

  METHODS get_filestruc
    EXPORTING er_data TYPE REF TO data
    RAISING   lcx_error.

  METHODS get_data
    EXPORTING et_data TYPE ANY TABLE
    RAISING   lcx_error.

  METHODS set_data
    IMPORTING it_data TYPE ANY TABLE
    RAISING   lcx_error.

  METHODS read
    IMPORTING ir_file TYPE REF TO lcl_file
              ir_last TYPE REF TO lif_process OPTIONAL
              ir_log  TYPE REF TO lcl_log
    RAISING   lcx_error.

  METHODS create
    IMPORTING iv_package TYPE devclass
              iv_request TYPE trkorr
              ir_log     TYPE REF TO lcl_log.

ENDINTERFACE.

** Classe para processamento da criacao de dominios
CLASS lcl_doma_proc DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_filestruc,
        "Atributos
        domname      TYPE dd01v-domname,
        ddtext       TYPE dd01v-ddtext,
        datatype     TYPE dd01v-datatype,
        leng         TYPE dd01v-leng,
        decimals     TYPE dd01v-decimals,
        outputlen    TYPE dd01v-outputlen,
        lowercase    TYPE dd01v-lowercase,
        signflag     TYPE dd01v-signflag,
        "Valores
        v_domvalue_l TYPE dd07v-domvalue_l,
        v_ddtext     TYPE dd07v-ddtext,
      END OF ty_filestruc,
      tt_filestruc TYPE STANDARD TABLE OF ty_filestruc WITH KEY domname v_domvalue_l,
      BEGIN OF ty_data,
        s_dd01v TYPE dd01v,
        t_dd07v TYPE lcl_doma=>tt_dd07v,
      END OF ty_data,
      tt_data TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

    DATA:
      t_data TYPE tt_data READ-ONLY.

    INTERFACES lif_process.

ENDCLASS.

CLASS lcl_doma_proc IMPLEMENTATION.

  METHOD lif_process~get_type.
    rv_type = lcl_doma=>c_objtyp.  "DOMA
  ENDMETHOD.

  METHOD lif_process~get_filestruc.

    TRY.
        "Cria referencia com tipo do arquivo
        CREATE DATA er_data TYPE tt_filestruc.
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~get_data.

    TRY.
        "Retorna informacoes mapeadas
        et_data[] = me->t_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~set_data.

    TRY.
        "Seta informacoes recebidas
        me->t_data[] = it_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~read.

    DATA:
      lt_file TYPE tt_filestruc,
      ls_doma TYPE ty_data.

    "Recupera dados do arquivo
    ir_file->parse( CHANGING ct_data = lt_file[] ).
    DELETE lt_file[] WHERE domname IS INITIAL.

    IF lt_file[] IS INITIAL.
      ir_log->i( |Arquivo { ir_file->v_filename } vazio| ).
      RETURN.
    ENDIF.

    "Mapeia tabelas
    LOOP AT lt_file[] ASSIGNING FIELD-SYMBOL(<fs_key>)
        GROUP BY ( domname    = <fs_key>-domname
                   ddtext     = <fs_key>-ddtext
                   datatype   = <fs_key>-datatype
                   leng       = <fs_key>-leng
                   decimals   = <fs_key>-decimals
                   outputlen  = <fs_key>-outputlen
                   lowercase  = <fs_key>-lowercase
                   signflag   = <fs_key>-signflag )
        ASSIGNING FIELD-SYMBOL(<fs_filedomain>) .
      CLEAR ls_doma.

      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_filedomain>-domname ).

      "Valida se ja existe
      IF lcl_doma=>exists( <fs_filedomain>-domname ) = abap_true.
        ir_log->i( |Dominio { <fs_filedomain>-domname } ja existe| ).
        CONTINUE.
      ENDIF.

      "Insere atributos
      ls_doma-s_dd01v = CORRESPONDING #( <fs_filedomain> ).
      ls_doma-s_dd01v-ddlanguage = lcl_object=>v_language.

      "Criador
      ls_doma-s_dd01v-as4user = sy-uname. "Usuario
      ls_doma-s_dd01v-as4date = sy-datum. "Data
      ls_doma-s_dd01v-as4time = sy-uzeit. "Hora

      DATA(lv_valpos) = VALUE valpos( ).
      LOOP AT GROUP <fs_filedomain> ASSIGNING FIELD-SYMBOL(<fs_filevalue>)
        WHERE v_domvalue_l IS NOT INITIAL.
        "Insere valor
        APPEND INITIAL LINE TO ls_doma-t_dd07v[] ASSIGNING FIELD-SYMBOL(<fs_dd07v>).
        <fs_dd07v>-domname    = ls_doma-s_dd01v-domname.
        <fs_dd07v>-ddlanguage = ls_doma-s_dd01v-ddlanguage.
        <fs_dd07v>-valpos     = lv_valpos = lv_valpos + 1.
        <fs_dd07v>-domvalue_l = <fs_filevalue>-v_domvalue_l.
        <fs_dd07v>-ddtext     = <fs_filevalue>-v_ddtext.
      ENDLOOP.

      "Salva registro
      APPEND ls_doma TO me->t_data[].

      "Insere log de sucesso
      ir_log->s( |Registro mapeado| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_process~create .

    LOOP AT me->t_data[] ASSIGNING FIELD-SYMBOL(<fs_doma>).
      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_doma>-s_dd01v-domname ).

      TRY.
          "Cria dominio
          lcl_doma=>create( is_dd01v = <fs_doma>-s_dd01v
                            it_dd07v = <fs_doma>-t_dd07v[] ).
          ir_log->s( |Registro criado| ).

          "Insere no pacote / request
          lcl_doma=>set_package( iv_objtype = me->lif_process~get_type( )
                                 iv_objname = <fs_doma>-s_dd01v-domname
                                 iv_package = iv_package
                                 iv_request = iv_request ).
          ir_log->s( |Registro inserido no pacote/request| ).

          "Ativa dominio
          lcl_doma=>activate( <fs_doma>-s_dd01v-domname ).
          ir_log->s( |Registro ativado| ).

        CATCH lcx_error INTO DATA(lrx_error).
          ir_log->e( lrx_error->get_text( ) ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

** Classe para processamento da criacao de elemento de dados
CLASS lcl_dtel_proc DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_filestruc,
        "Atributos
        rollname  TYPE dd04v-rollname,
        ddtext    TYPE dd04v-ddtext,
        scrtext_s TYPE dd04v-scrtext_s,
        scrtext_m TYPE dd04v-scrtext_m,
        scrtext_l TYPE dd04v-scrtext_l,
        domname   TYPE dd04v-domname,
        datatype  TYPE dd04v-datatype,
        leng      TYPE dd04v-leng,
        decimals  TYPE dd04v-decimals,
        outputlen TYPE dd04v-outputlen,
      END OF ty_filestruc,
      tt_filestruc TYPE STANDARD TABLE OF ty_filestruc WITH EMPTY KEY,
      BEGIN OF ty_data,
        s_dd04v TYPE dd04v,
      END OF ty_data,
      tt_data TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

    DATA:
      t_data TYPE tt_data READ-ONLY.

    INTERFACES lif_process.

ENDCLASS.

CLASS lcl_dtel_proc IMPLEMENTATION.

  METHOD lif_process~get_type.
    rv_type = lcl_dtel=>c_objtyp.  "DTEL
  ENDMETHOD.

  METHOD lif_process~get_filestruc.

    TRY.
        "Cria referencia com tipo do arquivo
        CREATE DATA er_data TYPE tt_filestruc.
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~get_data.

    TRY.
        "Retorna informacoes mapeadas
        et_data[] = me->t_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~set_data.

    TRY.
        "Seta informacoes recebidas
        me->t_data[] = it_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~read.

    DATA:
      lt_file TYPE tt_filestruc,
      ls_dtel TYPE ty_data.
    DATA:
      lt_doma  TYPE lcl_doma_proc=>tt_data,
      lt_dd07v TYPE lcl_doma=>tt_dd07v,
      ls_dd01v TYPE dd01v.
    DATA:
      lrx_nodoma TYPE REF TO cx_static_check.

    "Recupera dados do arquivo
    ir_file->parse( CHANGING ct_data = lt_file[] ).
    DELETE lt_file[] WHERE rollname IS INITIAL.

    IF lt_file[] IS INITIAL.
      ir_log->i( |Arquivo { ir_file->v_filename } vazio| ).
      RETURN.
    ENDIF.

    IF ir_last IS NOT INITIAL.
      IF ir_last->get_type( ) <> lcl_doma=>c_objtyp.
        ir_log->e( |Arquivo predecessor nao se refere aos dominios| ).
        RETURN.
      ENDIF.
      "Le dominios a serem criados
      ir_last->get_data( IMPORTING et_data = lt_doma[] ).
    ENDIF.

    "Mapeia tabelas
    LOOP AT lt_file[] ASSIGNING FIELD-SYMBOL(<fs_fileelement>).
      CLEAR: ls_dtel, ls_dd01v, lt_dd07v[], lrx_nodoma.

      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_fileelement>-rollname ).

      "Valida se ja existe
      IF lcl_dtel=>exists( <fs_fileelement>-rollname ) = abap_true.
        ir_log->i( |Elemento de dados { <fs_fileelement>-rollname } ja existe| ).
        CONTINUE.
      ENDIF.

      IF <fs_fileelement>-domname IS NOT INITIAL.
        TRY.
            "Recupera atributos do dominio
            lcl_doma=>read( EXPORTING iv_domname = <fs_fileelement>-domname
                            IMPORTING es_dd01v   = ls_dd01v
                                      et_dd07v   = lt_dd07v ).

          CATCH lcx_error INTO lrx_nodoma. "Dominio nao existe
            TRY.
                "Recupera informacoes no arquivo de dominios
                ls_dd01v = lt_doma[ s_dd01v-domname = <fs_fileelement>-domname ]-s_dd01v.
                lt_dd07v = lt_doma[ s_dd01v-domname = <fs_fileelement>-domname ]-t_dd07v.
              CATCH cx_sy_itab_line_not_found.
                "Dominio nao existe
                ir_log->e( lrx_nodoma->get_text( ) ).
                CONTINUE.
            ENDTRY.
        ENDTRY.

        "Insere dominio do elemento de dados
        ls_dtel-s_dd04v = CORRESPONDING #( ls_dd01v ).
        ls_dtel-s_dd04v-valexi = COND #(
            WHEN lt_dd07v[] IS NOT INITIAL THEN abap_true ).    "Valores fixos
        ls_dtel-s_dd04v-refkind = 'D'.  "Dominio                "Tipo de Referencia
        ls_dtel-s_dd04v-domname = <fs_fileelement>-domname.     "Dominio

      ELSE.
        "Insere tipo direto do elemento de dados
        ls_dtel-s_dd04v-refkind   = ''. "Entrada direta         "Tipo de Referencia
        ls_dtel-s_dd04v-datatype  = <fs_fileelement>-datatype.  "Tipo
        ls_dtel-s_dd04v-leng      = <fs_fileelement>-leng.      "Tamanho
        ls_dtel-s_dd04v-decimals  = <fs_fileelement>-decimals.  "Decimais
        ls_dtel-s_dd04v-outputlen = <fs_fileelement>-outputlen. "Tamanho output

      ENDIF.

      "Preenche atributos
      MOVE-CORRESPONDING <fs_fileelement> TO ls_dtel-s_dd04v.
      ls_dtel-s_dd04v-reptext    = ls_dtel-s_dd04v-ddtext.
      ls_dtel-s_dd04v-ddlanguage = lcl_object=>v_language.
      ls_dtel-s_dd04v-headlen    = 55. "Tamanho do titulo
      ls_dtel-s_dd04v-scrlen1    = 10. "Tamanho descricao curta
      ls_dtel-s_dd04v-scrlen2    = 20. "Tamanho descricao media
      ls_dtel-s_dd04v-scrlen3    = 40. "Tamanho descricao longa

      "Criador
      ls_dtel-s_dd04v-as4user = sy-uname. "Usuario criador
      ls_dtel-s_dd04v-as4date = sy-datum. "Data criacao
      ls_dtel-s_dd04v-as4time = sy-uzeit. "Hora criacao

      "Salva registro
      APPEND ls_dtel TO me->t_data[].

      "Insere log de sucesso
      IF lrx_nodoma IS NOT INITIAL.
        ir_log->s( |Registro mapeado de acordo com arquivo predecessor| ).
      ELSE.
        ir_log->s( |Registro mapeado| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_process~create .

    LOOP AT me->t_data[] ASSIGNING FIELD-SYMBOL(<fs_dtel>).
      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_dtel>-s_dd04v-rollname ).

      TRY.
          "Cria elemento de dados
          lcl_dtel=>create( is_dd04v = <fs_dtel>-s_dd04v ).
          ir_log->s( |Registro criado| ).

          "Insere no pacote / request
          lcl_dtel=>set_package( iv_objtype = me->lif_process~get_type( )
                                 iv_objname = <fs_dtel>-s_dd04v-rollname
                                 iv_package = iv_package
                                 iv_request = iv_request ).
          ir_log->s( |Registro inserido no pacote/request| ).

          "Ativa elemento de dados
          lcl_dtel=>activate( <fs_dtel>-s_dd04v-rollname ).
          ir_log->s( |Registro ativado| ).

        CATCH lcx_error INTO DATA(lrx_error).
          ir_log->e( lrx_error->get_text( ) ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

** Classe para processamento da criacao de Tabelas/Estruturas
CLASS lcl_tabl_proc DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_filestruc,
        "Atributos
        tabname   TYPE dd02v-tabname,
        ddtext    TYPE dd02v-ddtext,
        tabclass  TYPE dd02v-tabclass,
        "Campos
        fieldname TYPE dd03p-fieldname,
        rollname  TYPE dd03p-rollname,
        reftable  TYPE dd03p-reftable,
        reffield  TYPE dd03p-reffield,
      END OF ty_filestruc,
      tt_filestruc TYPE STANDARD TABLE OF ty_filestruc WITH KEY tabname,
      BEGIN OF ty_data,
        s_dd02v TYPE dd02v,
        s_dd09l TYPE dd09l,
        t_dd03p TYPE lcl_tabl=>tt_dd03p,
        t_dd05m TYPE lcl_tabl=>tt_dd05m,
        t_dd08v TYPE lcl_tabl=>tt_dd08v,
        t_dd35v TYPE lcl_tabl=>tt_dd35v,
        t_dd36m TYPE lcl_tabl=>tt_dd36m,
      END OF ty_data,
      tt_data TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

    DATA:
      t_data TYPE tt_data READ-ONLY.

    INTERFACES lif_process.

ENDCLASS.

CLASS lcl_tabl_proc IMPLEMENTATION.

  METHOD lif_process~get_type.
    rv_type = lcl_tabl=>c_objtyp.  "TABL
  ENDMETHOD.

  METHOD lif_process~get_filestruc.

    TRY.
        "Cria referencia com tipo do arquivo
        CREATE DATA er_data TYPE tt_filestruc.
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~get_data.

    TRY.
        "Retorna informacoes mapeadas
        et_data[] = me->t_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~set_data.

    TRY.
        "Seta informacoes recebidas
        me->t_data[] = it_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~read.

    DATA:
      lt_file  TYPE tt_filestruc,
      ls_tabl  TYPE ty_data,
      ls_dd03p TYPE dd03p.
    DATA:
      lt_dtel  TYPE lcl_dtel_proc=>tt_data,
      ls_dd04v TYPE dd04v.
    DATA:
      lrx_nodtel    TYPE REF TO cx_static_check,
      lrx_notinfile TYPE REF TO cx_dynamic_check.

    "Recupera dados do arquivo
    ir_file->parse( CHANGING ct_data = lt_file[] ).
    DELETE lt_file[] WHERE tabname IS INITIAL OR fieldname IS INITIAL.

    IF lt_file[] IS INITIAL.
      ir_log->i( |Arquivo { ir_file->v_filename } vazio| ).
      RETURN.
    ENDIF.

    IF ir_last IS NOT INITIAL.
      IF ir_last->get_type( ) <> lcl_dtel=>c_objtyp.
        ir_log->e( |Arquivo predecessor nao se refere aos elem.dados| ).
        RETURN.
      ENDIF.
      "Le elementos de dados a serem criados
      ir_last->get_data( IMPORTING et_data = lt_dtel[] ).
    ENDIF.

    "Mapeia tabelas
    LOOP AT lt_file[] ASSIGNING FIELD-SYMBOL(<fs_key>)
        GROUP BY ( tabname   = <fs_key>-tabname
                   ddtext    = <fs_key>-ddtext
                   tabclass  = <fs_key>-tabclass
                   fldcount  = GROUP SIZE )
        ASSIGNING FIELD-SYMBOL(<fs_filetable>).
      CLEAR: ls_tabl, lrx_nodtel, lrx_notinfile.

      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_filetable>-tabname ).

      IF lcl_tabl=>exists( <fs_filetable>-tabname ) = abap_true.
        ir_log->i( |Tabela/Estrutura ja existe| ).
        CONTINUE.
      ENDIF.

      IF <fs_filetable>-fldcount <= 0.
        ir_log->e( |Tabela/Estrutura nao possui colunas| ).
        CONTINUE.
      ENDIF.

      "Insere atributos da tabela
      ls_tabl-s_dd02v = CORRESPONDING #( <fs_filetable> ).
      ls_tabl-s_dd02v-ddlanguage = lcl_object=>v_language.
      ls_tabl-s_dd02v-exclass    = '4'.   "Ampliavel como pretendido

      "Insere Criador
      ls_tabl-s_dd02v-as4user = sy-uname. "Usuario
      ls_tabl-s_dd02v-as4date = sy-datum. "Data
      ls_tabl-s_dd02v-as4time = sy-uzeit. "Hora

      "Insere campos
      DATA(lv_position) = VALUE tabfdpos( ).
      LOOP AT GROUP <fs_filetable> ASSIGNING FIELD-SYMBOL(<fs_column>).
        CLEAR: ls_dd03p, ls_dd04v.

        TRY.
            "Recupera informacoes do elemento de dados
            lcl_dtel=>read( EXPORTING iv_rollname = <fs_column>-rollname
                            IMPORTING es_dd04v    = ls_dd04v ).

          CATCH lcx_error INTO lrx_nodtel. "Elemento de dados nao existe
            TRY.
                "Recupera informacoes no arquivo de elementos de dados
                ls_dd04v = lt_dtel[ s_dd04v-rollname = <fs_column>-rollname ]-s_dd04v.
              CATCH cx_sy_itab_line_not_found INTO lrx_notinfile.
                "Elemento de dados nao existe
                ir_log->e( lrx_nodtel->get_text( ) ).
                CONTINUE.
            ENDTRY.
        ENDTRY.

        "Insere campo da tabela
        ls_dd03p = CORRESPONDING #( ls_dd04v ).
        ls_dd03p-tabname    = ls_tabl-s_dd02v-tabname.
        ls_dd03p-ddlanguage = ls_tabl-s_dd02v-ddlanguage.
        ls_dd03p-position   = lv_position = lv_position + 1.
        ls_dd03p-fieldname  = <fs_column>-fieldname.
        ls_dd03p-rollname   = <fs_column>-rollname.
        ls_dd03p-reftable   = <fs_column>-reftable.
        ls_dd03p-reffield   = <fs_column>-reffield.
        APPEND ls_dd03p TO ls_tabl-t_dd03p[].
      ENDLOOP.
      CHECK lrx_notinfile IS INITIAL.

      "Salva registro
      APPEND ls_tabl TO me->t_data[].

      "Insere log de sucesso
      IF lrx_nodtel IS NOT INITIAL.
        ir_log->s( |Registro mapeado de acordo com arquivo predecessor| ).
      ELSE.
        ir_log->s( |Registro mapeado| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_process~create .

    LOOP AT me->t_data[] ASSIGNING FIELD-SYMBOL(<fs_tabl>).
      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_tabl>-s_dd02v-tabname ).

      TRY.
          "Cria tabela / estrutura
          lcl_tabl=>create( is_dd02v = <fs_tabl>-s_dd02v
                            is_dd09l = <fs_tabl>-s_dd09l
                            it_dd03p = <fs_tabl>-t_dd03p
                            it_dd05m = <fs_tabl>-t_dd05m
                            it_dd08v = <fs_tabl>-t_dd08v
                            it_dd35v = <fs_tabl>-t_dd35v
                            it_dd36m = <fs_tabl>-t_dd36m ).
          ir_log->s( |Registro criado| ).

          "Insere no pacote / request
          lcl_tabl=>set_package( iv_objtype = me->lif_process~get_type( )
                                 iv_objname = <fs_tabl>-s_dd02v-tabname
                                 iv_package = iv_package
                                 iv_request = iv_request ).
          ir_log->s( |Registro inserido no pacote/request| ).

          "Ativa tabela / estrutura
          lcl_tabl=>activate( <fs_tabl>-s_dd02v-tabname ).
          ir_log->s( |Registro ativado| ).

        CATCH lcx_error INTO DATA(lrx_error).
          ir_log->e( lrx_error->get_text( ) ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_proc DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_filestruc,
        "Atributos
        tabname   TYPE dd02v-tabname,
        ddtext    TYPE dd02v-ddtext,
        "Campo
        fieldname TYPE dd03p-fieldname,
        rollname  TYPE dd04v-rollname,
        ddtext_f  TYPE dd04v-ddtext,
        scrtext_s TYPE dd04v-scrtext_s,
        scrtext_m TYPE dd04v-scrtext_m,
        scrtext_l TYPE dd04v-scrtext_l,
        datatype  TYPE dd01v-datatype,
        leng      TYPE dd01v-leng,
        decimals  TYPE dd01v-decimals,
        outputlen TYPE dd01v-outputlen,
        "Referencia do campo
        reftable  TYPE dd03p-reftable,
        reffield  TYPE dd03p-reffield,
      END OF ty_filestruc,
      tt_filestruc TYPE STANDARD TABLE OF ty_filestruc WITH KEY tabname,
      BEGIN OF ty_data,
        t_tabl TYPE lcl_tabl_proc=>tt_data,
        t_dtel TYPE lcl_dtel_proc=>tt_data,
        t_doma TYPE lcl_doma_proc=>tt_data,
      END OF ty_data,
      tt_data TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

    DATA:
      t_data TYPE tt_data READ-ONLY.

    INTERFACES lif_process.

  PRIVATE SECTION.
    METHODS mapp_dtel
      IMPORTING is_file        TYPE ty_filestruc
      RETURNING VALUE(rs_dtel) TYPE lcl_dtel_proc=>ty_data.

ENDCLASS.

CLASS lcl_alv_proc IMPLEMENTATION.

  METHOD lif_process~get_type.
    rv_type = 'ZALV'.
  ENDMETHOD.

  METHOD lif_process~get_filestruc.

    TRY.
        "Cria referencia com tipo do arquivo
        CREATE DATA er_data TYPE tt_filestruc.
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~get_data.

    TRY.
        "Retorna informacoes mapeadas
        et_data[] = me->t_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~set_data.

    TRY.
        "Seta informacoes recebidas
        me->t_data[] = it_data[].
      CATCH cx_root INTO DATA(lrx_root).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = lrx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_process~read.

    DATA:
      lt_file TYPE tt_filestruc,
      ls_alv  TYPE ty_data.
    DATA:
      ls_tabl   TYPE lcl_tabl_proc=>ty_data,
      ls_dtel   TYPE lcl_dtel_proc=>ty_data,
      ls_column TYPE dd03p.

    "Recupera dados do arquivo
    ir_file->parse( CHANGING ct_data = lt_file[] ).
    DELETE lt_file[] WHERE tabname IS INITIAL OR fieldname IS INITIAL.

    IF lt_file[] IS INITIAL.
      ir_log->i( |Arquivo { ir_file->v_filename } vazio| ).
      RETURN.
    ENDIF.

    "Mapeia tabelas
    LOOP AT lt_file[] ASSIGNING FIELD-SYMBOL(<fs_key>)
        GROUP BY ( tabname   = <fs_key>-tabname
                   ddtext    = <fs_key>-ddtext
                   fldcount  = GROUP SIZE )
        ASSIGNING FIELD-SYMBOL(<fs_filetable>).
      CLEAR: ls_alv, ls_tabl.

      "Seta contexto do log
      ir_log->set_context( iv_objtype = me->lif_process~get_type( )
                           iv_objname = <fs_filetable>-tabname ).

      IF lcl_tabl=>exists( <fs_filetable>-tabname ) = abap_true.
        ir_log->i( |Tabela/Estrutura ja existe| ).
        CONTINUE.
      ENDIF.

      IF <fs_filetable>-fldcount <= 0.
        ir_log->e( |Tabela/Estrutura nao possui colunas| ).
        CONTINUE.
      ENDIF.

      "Insere atributos da estrutura
      ls_tabl-s_dd02v = CORRESPONDING #( <fs_filetable> ).
      ls_tabl-s_dd02v-ddlanguage = lcl_object=>v_language.
      ls_tabl-s_dd02v-tabclass   = 'INTTAB'.    "Estrutura
      ls_tabl-s_dd02v-exclass    = '4'.         "Ampliavel como pretendido

      "Insere Criador
      ls_tabl-s_dd02v-as4user = sy-uname. "Usuario
      ls_tabl-s_dd02v-as4date = sy-datum. "Data
      ls_tabl-s_dd02v-as4time = sy-uzeit. "Hora

      "Insere campos
      DATA(lv_position) = VALUE tabfdpos( ).
      LOOP AT GROUP <fs_filetable> ASSIGNING FIELD-SYMBOL(<fs_column>).
        CLEAR ls_dtel.

        TRY.
            "Recupera informacoes do elemento de dados
            lcl_dtel=>read( EXPORTING iv_rollname = <fs_column>-rollname
                            IMPORTING es_dd04v    = ls_dtel-s_dd04v ).

          CATCH lcx_error. "Elemento de dados nao existe
            "Mapeia elemento de dados com informacoes do arquivo
            ls_dtel = me->mapp_dtel( <fs_column> ).
            IF ls_dtel-s_dd04v-rollname IS NOT INITIAL.
              "Insere elem.dados para criacao
              APPEND ls_dtel TO ls_alv-t_dtel[].

              "Insere log
              ir_log->add( iv_objtype = lcl_dtel=>c_objtyp
                           iv_objname = CONV #( ls_dtel-s_dd04v-rollname )
                           iv_msgtyp  = 'S'
                           iv_msgtxt  = 'Registro mapeado' ).
            ENDIF.

        ENDTRY.

        "Insere campo da tabela
        CLEAR ls_column.
        ls_column = CORRESPONDING #( ls_dtel-s_dd04v ).
        ls_column-tabname    = ls_tabl-s_dd02v-tabname.
        ls_column-ddlanguage = ls_tabl-s_dd02v-ddlanguage.
        ls_column-position   = lv_position = lv_position + 1.
        ls_column-fieldname  = <fs_column>-fieldname.
        ls_column-rollname   = <fs_column>-rollname.
        ls_column-reftable   = <fs_column>-reftable.
        ls_column-reffield   = <fs_column>-reffield.
        APPEND ls_column TO ls_tabl-t_dd03p[].
      ENDLOOP.

      "Insere estrutura da ALV
      APPEND ls_tabl TO ls_alv-t_tabl[].

      "Salva ALV para criacao
      APPEND ls_alv TO me->t_data[].

      "Insere log de sucesso
      ir_log->s( |Registro mapeado| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_process~create .
  ENDMETHOD.

  METHOD mapp_dtel.

    CLEAR rs_dtel.

    "Preenche atributos
    MOVE-CORRESPONDING is_file TO rs_dtel-s_dd04v.
    rs_dtel-s_dd04v-refkind    = ''. "Entrada direta        "Tipo de Referencia
    rs_dtel-s_dd04v-ddtext     = is_file-ddtext_f.          "Descricao do campo
    rs_dtel-s_dd04v-reptext    = rs_dtel-s_dd04v-ddtext.    "Titulo
    rs_dtel-s_dd04v-ddlanguage = lcl_object=>v_language.    "Lingaugem
    rs_dtel-s_dd04v-headlen    = 55.                        "Tamanho do titulo
    rs_dtel-s_dd04v-scrlen1    = 10.                        "Tamanho descricao curta
    rs_dtel-s_dd04v-scrlen2    = 20.                        "Tamanho descricao media
    rs_dtel-s_dd04v-scrlen3    = 40.                        "Tamanho descricao longa

    "Criador
    rs_dtel-s_dd04v-as4user = sy-uname.                     "Usuario
    rs_dtel-s_dd04v-as4date = sy-datum.                     "Data
    rs_dtel-s_dd04v-as4time = sy-uzeit.                     "Hora

  ENDMETHOD.

ENDCLASS.

** Classe para processamento principal
CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_queue,
        priority TYPE i,
        r_proc   TYPE REF TO lif_process,
      END OF ty_queue,
      tt_queue TYPE STANDARD TABLE OF ty_queue WITH EMPTY KEY.

    CLASS-METHODS get_template
      IMPORTING ir_process TYPE REF TO lif_process
      RAISING   lcx_error.

    METHODS constructor.

    METHODS add
      IMPORTING iv_path TYPE lcl_file=>ty_filename
                ir_proc TYPE REF TO lif_process
                ir_last TYPE REF TO lif_process OPTIONAL
      RAISING   lcx_error.

    METHODS add_alv
      IMPORTING iv_path TYPE lcl_file=>ty_filename
      RAISING   lcx_error.

    METHODS process
      IMPORTING iv_request TYPE trkorr
                iv_package TYPE devclass.

    METHODS display.

  PRIVATE SECTION.
    DATA:
      r_log   TYPE REF TO lcl_log,
      t_queue TYPE tt_queue.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD get_template.

    DATA:
      lr_filestruc TYPE REF TO data,
      lt_template  TYPE stringtab,
      lv_filepath  TYPE string,
      lv_filename  TYPE string,
      lv_fullpath  TYPE string,
      lv_usaction  TYPE i.

    "Recupera tipo da tabela do arquivo
    ir_process->get_filestruc( IMPORTING er_data = lr_filestruc ).
    CHECK lr_filestruc IS NOT INITIAL.

    "Recupera definicao da estrutura
    DATA(lr_structdescr) = CAST cl_abap_structdescr(
      CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data_ref( lr_filestruc )
      )->get_table_line_type( )
    ).

    "Insere header
    APPEND INITIAL LINE TO lt_template[] ASSIGNING FIELD-SYMBOL(<fs_header>).
    "Linha vazia
    APPEND INITIAL LINE TO lt_template[].

    "Preenche header
    LOOP AT lr_structdescr->components[] ASSIGNING FIELD-SYMBOL(<fs_field>).
      "Preenche separador
      IF sy-tabix <> 1.
        <fs_header> = |{ <fs_header> }{ lcl_file=>c_separator }|.
      ENDIF.

      "Recupera tipo do campo
      DATA(ls_ddicfld) = CAST cl_abap_elemdescr(
        lr_structdescr->get_component_type( <fs_field>-name )
      )->get_ddic_field( ).
      IF ls_ddicfld IS INITIAL.
        "Preenche com nome ABAP
        <fs_header> = |{ <fs_header> }{ <fs_field>-name  } ({ <fs_field>-type_kind }/{ <fs_field>-length })|.
      ELSE.
        "Preenche com descricao do elemento
        <fs_header> = |{ <fs_header> }{ ls_ddicfld-fieldtext  } ({ <fs_field>-name  }/{ <fs_field>-type_kind }/{ <fs_field>-length })|.
      ENDIF.
    ENDLOOP.

    "Monta nome default
    lv_filename = |Template { ir_process->get_type( ) }.txt|.

    "Solicita diretorio
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_file_name         = lv_filename
        file_filter               = '*.TXT'
      CHANGING
        filename                  = lv_filename
        path                      = lv_filepath
        fullpath                  = lv_fullpath
        user_action               = lv_usaction
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    IF lv_usaction <> cl_gui_frontend_services=>action_ok.
      MESSAGE 'Cancelado pelo usuário' TYPE 'S' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    "Realiza download do arquivo
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                = lv_filename
      CHANGING
        data_tab                = lt_template[]
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Abre arquivo
    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = lv_filename
        synchronous            = ' '
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    "Inicializa log
    me->r_log = NEW lcl_log( ).

  ENDMETHOD.

  METHOD add.

    CHECK iv_path IS NOT INITIAL AND ir_proc IS BOUND.

    "Le arquivo
    DATA(lr_file) = NEW lcl_file( iv_filename  = iv_path
                                  iv_hasheader = abap_true ).
    lr_file->upload( ).

    "Le dados do arquivo
    ir_proc->read( ir_file = lr_file
                   ir_last = ir_last        "processamento anterior
                   ir_log  = me->r_log ).

    "Insere registro na fila
    APPEND INITIAL LINE TO me->t_queue[] ASSIGNING FIELD-SYMBOL(<fs_queue>).
    <fs_queue>-r_proc   = ir_proc.
    <fs_queue>-priority = SWITCH #( ir_proc->get_type( )
      WHEN lcl_doma=>c_objtyp THEN 1  "DOMA
      WHEN lcl_dtel=>c_objtyp THEN 2  "DTEL
      WHEN lcl_tabl=>c_objtyp THEN 3  "TABL
      ELSE 999
    ).

  ENDMETHOD.

  METHOD add_alv.

    DATA:
      lt_alv TYPE lcl_alv_proc=>tt_data.

    CHECK iv_path IS NOT INITIAL.

    "Le arquivo
    DATA(lr_file) = NEW lcl_file( iv_filename  = iv_path
                                  iv_hasheader = abap_true ).
    lr_file->upload( ).

    "Cria instancia da classe de processamento
    DATA(lr_alv) = NEW lcl_alv_proc( ).

    "Le dados do arquivo
    lr_alv->lif_process~read( ir_file = lr_file
                              ir_log  = me->r_log ).

    "Recupera informacoes mapeadas
    lr_alv->lif_process~get_data( IMPORTING et_data = lt_alv[] ).

    "Insere dados mapeados na fila de processamento
    LOOP AT lt_alv[] ASSIGNING FIELD-SYMBOL(<fs_alv>).

      "Insere tabela/estrutura na fila
      APPEND INITIAL LINE TO me->t_queue[] ASSIGNING FIELD-SYMBOL(<fs_queue_tabl>).
      <fs_queue_tabl>-r_proc   = NEW lcl_tabl_proc( ).
      <fs_queue_tabl>-priority = 3.
      "Seta estruturas mapeadas
      <fs_queue_tabl>-r_proc->set_data( <fs_alv>-t_tabl[] ).

      IF <fs_alv>-t_doma[] IS NOT INITIAL.
        "Insere dominios na fila
        APPEND INITIAL LINE TO me->t_queue[] ASSIGNING FIELD-SYMBOL(<fs_queue_doma>).
        <fs_queue_doma>-r_proc   = NEW lcl_doma_proc( ).
        <fs_queue_doma>-priority = 1.
        "Seta dominios mapeados
        <fs_queue_doma>-r_proc->set_data( <fs_alv>-t_doma[] ).
      ENDIF.

      IF <fs_alv>-t_dtel[] IS NOT INITIAL.
        "Insere elementos de dados na fila
        APPEND INITIAL LINE TO me->t_queue[] ASSIGNING FIELD-SYMBOL(<fs_queue_dtel>).
        <fs_queue_dtel>-r_proc   = NEW lcl_dtel_proc( ).
        <fs_queue_dtel>-priority = 2.
        "Seta elementos de dados mapeados
        <fs_queue_dtel>-r_proc->set_data( <fs_alv>-t_dtel[] ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD process.

    "Valida se nao houve erro na selecao
    CHECK me->r_log->has_error( ) = abap_false.

    SORT me->t_queue[] BY priority.

    "Processa fila
    LOOP AT me->t_queue[] ASSIGNING FIELD-SYMBOL(<fs_queue>).
      "Informa progresso
      DATA(lv_progress) = ( ( sy-tabix / lines( me->t_queue[] ) ) - 1 ) * 100.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_progress
          text       = |Criando { <fs_queue>-r_proc->get_type( ) }'s...|.

      "Processa criacao dos objetos
      <fs_queue>-r_proc->create( iv_package = iv_package
                                 iv_request = iv_request
                                 ir_log     = me->r_log ).

      "Aborta execucao
      IF me->r_log->has_error( ) = abap_true.
        me->r_log->i( 'Execucoes seguintes abortadas' ).
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    "Exibe log
    me->r_log->display( ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Variaveis globais
DATA:
  gr_main TYPE REF TO lcl_main.

**********************************************************************
*** Tela de selecao
TABLES: sscrfields.
SELECTION-SCREEN FUNCTION KEY 1.  "download template DOMA
SELECTION-SCREEN FUNCTION KEY 2.  "download template DTEL
SELECTION-SCREEN FUNCTION KEY 3.  "download template TABL
SELECTION-SCREEN FUNCTION KEY 4.  "download template ALV
"Arquivos
SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE TEXT-t02.
  PARAMETERS:
    p_inddir RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND chg,
    p_doma   TYPE lcl_file=>ty_filename LOWER CASE,  "Dominios
    p_dtel   TYPE lcl_file=>ty_filename LOWER CASE,  "Elem.Dados
    p_tabl   TYPE lcl_file=>ty_filename LOWER CASE,  "Tabelas/Estruturas
    p_indalv RADIOBUTTON GROUP g1,
    p_alv    TYPE lcl_file=>ty_filename LOWER CASE.  "ALV
SELECTION-SCREEN END OF BLOCK file.
"Parametros de execucao
SELECTION-SCREEN BEGIN OF BLOCK exec WITH FRAME TITLE TEXT-t01.
  PARAMETERS:
    p_pack TYPE devclass OBLIGATORY DEFAULT lcl_object=>c_local_package,    "Pacote
    p_treq TYPE trkorr.                                                     "Request
  PARAMETERS:
    p_test AS CHECKBOX DEFAULT 'X',
    p_debg AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK exec.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_doma.
  lcl_file=>f4( CHANGING cv_filename = p_doma ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dtel.
  lcl_file=>f4( CHANGING cv_filename = p_dtel ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tabl.
  lcl_file=>f4( CHANGING cv_filename = p_tabl ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alv.
  lcl_file=>f4( CHANGING cv_filename = p_alv ).

AT SELECTION-SCREEN OUTPUT.
  DEFINE hide_field.
    screen-invisible = 1.
    screen-input     = 0.
    MODIFY SCREEN.
  END-OF-DEFINITION.
  DEFINE display_field.
    screen-invisible = 0.
    screen-input     = 1.
    MODIFY SCREEN.
  END-OF-DEFINITION.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_inddir.
        IF screen-name CS 'P_ALV'.
          hide_field.       "Oculta campo
        ELSEIF screen-name CS 'P_DOMA' OR screen-name CS 'P_DTEL' OR screen-name CS 'P_TABL'.
          display_field.    "Exibe campo
        ENDIF.
      WHEN p_indalv.
        IF screen-name CS 'P_ALV'.
          display_field.    "Exibe campo
        ELSEIF screen-name CS 'P_DOMA' OR screen-name CS 'P_DTEL' OR screen-name CS 'P_TABL'.
          hide_field.       "Oculta campo
        ENDIF.
    ENDCASE.
  ENDLOOP.

AT SELECTION-SCREEN.
  TRY.
      CASE sy-ucomm.
        WHEN 'FC01'.
          lcl_main=>get_template( NEW lcl_doma_proc( ) ). "Dominios
        WHEN 'FC02'.
          lcl_main=>get_template( NEW lcl_dtel_proc( ) ). "Elem.Dados
        WHEN 'FC03'.
          lcl_main=>get_template( NEW lcl_tabl_proc( ) ). "Estruturas/Tabelas
        WHEN 'FC04'.
          lcl_main=>get_template( NEW lcl_alv_proc( ) ).  "ALV
        WHEN OTHERS.
          IF p_test = abap_false AND
             ( p_pack <> lcl_object=>c_local_package AND p_treq IS INITIAL ).
            MESSAGE 'Request obrigatoria' TYPE 'E'.
          ENDIF.
      ENDCASE.
    CATCH lcx_error INTO DATA(lrx_error).
      MESSAGE lrx_error->get_text( ) TYPE 'E'.
  ENDTRY.

**********************************************************************
*** Inicializacao
LOAD-OF-PROGRAM.
  "Botoes para download dos templates
  sscrfields-functxt_01 = |@0U@Template DOMA|.
  sscrfields-functxt_02 = |@0U@Template DTEL|.
  sscrfields-functxt_03 = |@0U@Template TABL|.
  sscrfields-functxt_04 = |@0U@Template ALV|.

**********************************************************************
*** Logica principal
START-OF-SELECTION.
  TRY.
      "Inicializa classe principal
      gr_main = NEW lcl_main( ).

      CASE abap_true.
        WHEN p_inddir.  "Entrada direta
          "Insere Dominios para processamento
          DATA(lr_doma_proc) = NEW lcl_doma_proc( ).
          gr_main->add( iv_path = p_doma
                        ir_proc = lr_doma_proc ).

          "Insere Elementos de Dados para processamento
          DATA(lr_dtel_proc) = NEW lcl_dtel_proc( ).
          gr_main->add( iv_path = p_dtel
                        ir_proc = lr_dtel_proc
                        ir_last = lr_doma_proc ).

          "Insere Estruturas/Tabelas para processamento
          DATA(lr_tabl_proc) = NEW lcl_tabl_proc( ).
          gr_main->add( iv_path = p_tabl
                        ir_proc = lr_tabl_proc
                        ir_last = lr_dtel_proc ).

        WHEN p_indalv.  "ALV
          "Inser objetos da ALV para processamento
          gr_main->add_alv( p_alv ).

      ENDCASE.

      IF p_debg = abap_true.
        p_test = abap_true.
        BREAK-POINT.
      ENDIF.  "gr_main->t_queue[]

      IF p_test = abap_false.
        "Cria objetos
        gr_main->process( iv_request = p_treq
                          iv_package = p_pack ).
      ENDIF.

      "Exibe output
      gr_main->display( ).

      MESSAGE 'Processamento finalizado' TYPE 'S'.

    CATCH lcx_error cx_root INTO DATA(lrx_error).
      MESSAGE lrx_error->get_text( ) TYPE 'E'.
  ENDTRY.