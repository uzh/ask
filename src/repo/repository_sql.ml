open Lwt.Syntax
module Database = Sihl.Service.Database
module Utils = Repository_utils

module MariaDB () = struct
  module DbUtils = struct
    let set_fk_check_request =
      Caqti_request.exec Caqti_type.bool "SET FOREIGN_KEY_CHECKS = ?;"
    ;;

    let with_disabled_fk_check f =
      Database.query (fun connection ->
          let module Connection = (val connection : Caqti_lwt.CONNECTION) in
          let* () =
            Connection.exec set_fk_check_request false |> Lwt.map Utils.raise_caqti_error
          in
          Lwt.finalize
            (fun () -> f connection)
            (fun () ->
              Connection.exec set_fk_check_request true |> Lwt.map Utils.raise_caqti_error))
    ;;

    let found_rows_request =
      Caqti_request.find Caqti_type.unit Caqti_type.int "SELECT FOUND_ROWS()"
    ;;

    let is_unique_request table_name sql_filter request_types sql_joins =
      let sql_request =
        Caml.Format.asprintf
          {sql|
            SELECT NOT EXISTS (
              SELECT 1
              FROM %s
                %s
              WHERE %s
            )
          |sql}
          table_name
          sql_joins
          sql_filter
      in
      Caqti_request.find request_types Caqti_type.bool sql_request
    ;;

    let is_unique_with_uuid_request table_name sql_filter request_types sql_joins =
      let sql_request =
        Caml.Format.asprintf
          {sql|
          SELECT NOT EXISTS (
            SELECT 1
            FROM %s
              %s
            WHERE
              %s
              AND %s.uuid != UNHEX(REPLACE(?, '-', ''))
            LIMIT 1
          )
          |sql}
          table_name
          sql_joins
          sql_filter
          table_name
      in
      Caqti_request.find ~oneshot:true request_types Caqti_type.bool sql_request
    ;;

    let is_unique connection table_name ~sql_filter ~values ?sql_joins ?uuid () =
      let sql_joins = sql_joins |> Option.value ~default:"" in
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      match uuid with
      | None ->
        let (Utils.Dynparam.Pack (pt, pv)) = values in
        Connection.find (is_unique_request table_name sql_filter pt sql_joins) pv
        |> Lwt.map Utils.raise_caqti_error
      | Some uuid ->
        let values = Utils.Dynparam.add Caqti_type.string uuid values in
        let (Utils.Dynparam.Pack (pt, pv)) = values in
        Connection.find
          (is_unique_with_uuid_request table_name sql_filter pt sql_joins)
          pv
        |> Lwt.map Utils.raise_caqti_error
    ;;
  end

  module Sql = struct
    module Model = Repository_model

    module QuestionRow = struct
      let find_request =
        Caqti_request.find
          Caqti_type.string
          Model.QuestionRow.t
          {sql|
            SELECT
              LOWER(CONCAT(
                SUBSTR(HEX(uuid), 1, 8), '-',
                SUBSTR(HEX(uuid), 9, 4), '-',
                SUBSTR(HEX(uuid), 13, 4), '-',
                SUBSTR(HEX(uuid), 17, 4), '-',
                SUBSTR(HEX(uuid), 21)
              )),
              label,
              help_text,
              text,
              default_value,
              validation_regex,
              question_type,
              max_file_size_mb,
              mime_types,
              possible_options
            FROM quest_questions
            WHERE uuid = UNHEX(REPLACE(?, '-', ''))
          |sql}
      ;;

      let find connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find_opt find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let find_exn connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let insert_question_request =
        Caqti_request.exec
          Model.QuestionRow.t
          {sql|
            INSERT INTO quest_questions (
              uuid,
              label,
              help_text,
              text,
              default_value,
              validation_regex,
              question_type,
              max_file_size_mb,
              mime_types,
              possible_options
            ) VALUES (
              UNHEX(REPLACE(?, '-', '')),
              ?,
              ?,
              ?,
              ?,
              ?,
              ?,
              ?,
              ?,
              ?
            );
          |sql}
      ;;

      let insert connection question =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_question_request question
        |> Lwt.map Utils.raise_caqti_error
      ;;

      let clean_request =
        Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_questions;"
      ;;

      let clean connection =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec clean_request () |> Lwt.map Utils.raise_caqti_error
      ;;
    end

    module AnswerRow = struct
      let find_request =
        Caqti_request.find
          Caqti_type.string
          Model.AnswerRow.t
          {sql|
            SELECT
              LOWER(CONCAT(
                SUBSTR(HEX(uuid), 1, 8), '-',
                SUBSTR(HEX(uuid), 9, 4), '-',
                SUBSTR(HEX(uuid), 13, 4), '-',
                SUBSTR(HEX(uuid), 17, 4), '-',
                SUBSTR(HEX(uuid), 21)
              )),
              text,
              storage_handle
            FROM quest_answers
            WHERE uuid = UNHEX(REPLACE(?, '-', ''))
          |sql}
      ;;

      let find connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find_opt find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let find_exn connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let insert_type =
        let open Caqti_type in
        tup2
          string
          (tup2 string (tup2 string (tup2 string (tup2 (option string) (option string)))))
      ;;

      let insert_request =
        Caqti_request.exec
          insert_type
          {sql|
          INSERT INTO quest_answers (
            uuid,
            quest_questionnaire,
            quest_template_question_mapping,
            text,
            storage_handle
          ) VALUES (
            UNHEX(REPLACE(?, '-', '')),
            (SELECT id FROM quest_questionnaires WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', ''))),
            (SELECT id FROM quest_template_question_mappings
              WHERE quest_template_question_mappings.quest_template =
                (SELECT quest_template FROM quest_questionnaires
                  WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', '')))
              AND quest_template_question_mappings.quest_question =
                (SELECT id FROM quest_questions WHERE quest_questions.uuid = UNHEX(REPLACE(?, '-', '')))),
            ?,
            UNHEX(REPLACE(?, '-', ''))
          );
        |sql}
      ;;

      let insert connection answer =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_request answer |> Lwt.map Utils.raise_caqti_error
      ;;

      let update_request =
        Caqti_request.exec
          (let open Caqti_type in
          tup3 (option string) (option string) string)
          {sql|
          UPDATE quest_answers
          SET
            text = ?,
            storage_handle = UNHEX(REPLACE(?, '-', ''))
          WHERE
          quest_answers.uuid = UNHEX(REPLACE(?, '-', ''));
        |sql}
      ;;

      let update connection answer =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec update_request answer |> Lwt.map Utils.raise_caqti_error
      ;;

      let delete_request =
        Caqti_request.exec
          Caqti_type.string
          {sql|
          DELETE FROM quest_answers WHERE
          quest_answers.uuid = UNHEX(REPLACE(?, '-', ''));
        |sql}
      ;;

      let delete connection answer =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec delete_request answer |> Lwt.map Utils.raise_caqti_error
      ;;

      let is_unique ~questionnaire_id ~template_question_mapping_id ?uuid () =
        let table_name = "quest_answers" in
        let sql_filter =
          {sql|
            quest_questionnaire = (SELECT id FROM quest_questionnaires WHERE uuid = UNHEX(REPLACE(?, '-', '')) ) AND
            quest_template_question_mapping = (SELECT id FROM quest_template_question_mappings WHERE uuid = UNHEX(REPLACE(?, '-', '')) )
          |sql}
        in
        let values =
          let open Utils.Dynparam in
          empty
          |> add Caqti_type.string questionnaire_id
          |> add Caqti_type.string template_question_mapping_id
        in
        Database.query (fun connection ->
            DbUtils.is_unique connection table_name ~sql_filter ~values ?uuid ())
      ;;

      let clean_request =
        Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_answers;"
      ;;

      let clean connection =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec clean_request () |> Lwt.map Utils.raise_caqti_error
      ;;
    end

    module QuestionnaireRow = struct
      let find_request =
        Caqti_request.find
          Caqti_type.string
          Model.QuestionnaireRow.t
          {sql|
            SELECT
              LOWER(CONCAT(
                SUBSTR(HEX(quest_questionnaires.uuid), 1, 8), '-',
                SUBSTR(HEX(quest_questionnaires.uuid), 9, 4), '-',
                SUBSTR(HEX(quest_questionnaires.uuid), 13, 4), '-',
                SUBSTR(HEX(quest_questionnaires.uuid), 17, 4), '-',
                SUBSTR(HEX(quest_questionnaires.uuid), 21)
              )),
              LOWER(CONCAT(
                SUBSTR(HEX(quest_templates.uuid), 1, 8), '-',
                SUBSTR(HEX(quest_templates.uuid), 9, 4), '-',
                SUBSTR(HEX(quest_templates.uuid), 13, 4), '-',
                SUBSTR(HEX(quest_templates.uuid), 17, 4), '-',
                SUBSTR(HEX(quest_templates.uuid), 21)
              )),
              quest_templates.label,
              quest_templates.description
            FROM quest_questionnaires
              LEFT JOIN quest_templates ON quest_questionnaires.quest_template = quest_templates.id
            WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', ''))
          |sql}
      ;;

      let find connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find_opt find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let find_exn connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find find_request id |> Lwt.map Utils.raise_caqti_error
      ;;

      let find_questions_request =
        Caqti_request.collect
          Caqti_type.string
          Model.QuestionAnswerRow.t
          {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(quest_questions.uuid), 1, 8), '-',
              SUBSTR(HEX(quest_questions.uuid), 9, 4), '-',
              SUBSTR(HEX(quest_questions.uuid), 13, 4), '-',
              SUBSTR(HEX(quest_questions.uuid), 17, 4), '-',
              SUBSTR(HEX(quest_questions.uuid), 21)
            )),
            quest_questions.label,
            quest_questions.help_text,
            quest_questions.text,
            quest_template_question_mappings.required,
            quest_questions.default_value,
            quest_questions.validation_regex,
            quest_questions.question_type,
            quest_questions.max_file_size_mb,
            quest_questions.mime_types,
            quest_questions.possible_options,
            LOWER(CONCAT(
              SUBSTR(HEX(quest_answers.uuid), 1, 8), '-',
              SUBSTR(HEX(quest_answers.uuid), 9, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 13, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 17, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 21)
            )),
            quest_answers.text,
            LOWER(CONCAT(
              SUBSTR(HEX(storage_handles.uuid), 1, 8), '-',
              SUBSTR(HEX(storage_handles.uuid), 9, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 13, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 17, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 21)
            )),
            storage_handles.filename,
            storage_handles.filesize,
            storage_handles.mime
          FROM quest_questionnaires
            LEFT JOIN quest_templates ON quest_questionnaires.quest_template = quest_templates.id
            LEFT JOIN quest_template_question_mappings ON quest_templates.id = quest_template_question_mappings.quest_template
            LEFT JOIN quest_questions ON quest_template_question_mappings.quest_question = quest_questions.id
            LEFT JOIN quest_answers ON quest_questionnaires.id = quest_answers.quest_questionnaire
              AND quest_template_question_mappings.id = quest_answers.quest_template_question_mapping
            LEFT JOIN storage_handles ON quest_answers.storage_handle = storage_handles.uuid
          WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', ''))
            AND quest_questions.uuid IS NOT NULL
            ORDER BY quest_template_question_mappings.question_order ASC
        |sql}
      ;;

      let find_questions connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.collect_list find_questions_request id
        |> Lwt.map Utils.raise_caqti_error
      ;;

      let find_answer_request =
        Caqti_request.find_opt
          (let open Caqti_type in
          tup2 string string)
          Model.AnswerRow.t
          {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(quest_answers.uuid), 1, 8), '-',
              SUBSTR(HEX(quest_answers.uuid), 9, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 13, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 17, 4), '-',
              SUBSTR(HEX(quest_answers.uuid), 21)
            )),
            quest_answers.text,
            LOWER(CONCAT(
              SUBSTR(HEX(storage_handles.uuid), 1, 8), '-',
              SUBSTR(HEX(storage_handles.uuid), 9, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 13, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 17, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 21)
            ))
          FROM quest_answers
            LEFT JOIN storage_handles
              ON quest_answers.storage_handle = storage_handles.uuid
            LEFT JOIN quest_template_question_mappings
              ON quest_answers.quest_template_question_mapping = quest_template_question_mappings.id
            LEFT JOIN quest_questions
              ON quest_template_question_mappings.quest_question = quest_questions.id
            LEFT JOIN quest_questionnaires
              ON quest_answers.quest_questionnaire = quest_questionnaires.id
          WHERE
          quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', ''))
            AND quest_questions.uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
      ;;

      let find_answer connection ids =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find_opt find_answer_request ids |> Lwt.map Utils.raise_caqti_error
      ;;

      let insert_request =
        Caqti_request.exec
          (let open Caqti_type in
          tup2 string string)
          {sql|
            INSERT INTO quest_questionnaires (
              uuid,
              quest_template
            ) VALUES (
              UNHEX(REPLACE(?, '-', '')),
              (SELECT id FROM quest_templates WHERE quest_templates.uuid = UNHEX(REPLACE(?, '-', '')))
            );
          |sql}
      ;;

      let insert connection questionnaire =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_request questionnaire |> Lwt.map Utils.raise_caqti_error
      ;;

      let clean_request =
        Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_questionnaires;"
      ;;

      let clean connection =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec clean_request () |> Lwt.map Utils.raise_caqti_error
      ;;
    end

    module TemplateRow = struct
      let insert_request =
        Caqti_request.exec
          (let open Caqti_type in
          tup3 string string (option string))
          {sql|
            INSERT INTO quest_templates (
              uuid,
              label,
              description
            ) VALUES (
              UNHEX(REPLACE(?, '-', '')),
              ?,
              ?
            );
          |sql}
      ;;

      let insert connection template =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_request template |> Lwt.map Utils.raise_caqti_error
      ;;

      let clean_request =
        Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_templates;"
      ;;

      let clean connection =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec clean_request () |> Lwt.map Utils.raise_caqti_error
      ;;
    end

    module Mapping = struct
      let insert_request =
        Caqti_request.exec
          (let open Caqti_type in
          tup2 string (tup2 string (tup2 string (tup2 int bool))))
          {sql|
            INSERT INTO quest_template_question_mappings (
              uuid,
              quest_template,
              quest_question,
              question_order,
              required
            ) VALUES (
              UNHEX(REPLACE(?, '-', '')),
              (SELECT id FROM quest_templates WHERE quest_templates.uuid = UNHEX(REPLACE(?, '-', ''))),
              (SELECT id FROM quest_questions WHERE quest_questions.uuid = UNHEX(REPLACE(?, '-', ''))),
              ?,
              ?
            );
          |sql}
      ;;

      let insert connection mapping =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_request mapping |> Lwt.map Utils.raise_caqti_error
      ;;

      let clean_request =
        Caqti_request.exec
          Caqti_type.unit
          "TRUNCATE TABLE quest_template_question_mappings;"
      ;;

      let clean connection =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec clean_request () |> Lwt.map Utils.raise_caqti_error
      ;;
    end

    let clean () =
      Database.query (fun connection ->
          let* () = QuestionRow.clean connection in
          let* () = TemplateRow.clean connection in
          let* () = QuestionnaireRow.clean connection in
          let* () = Mapping.clean connection in
          AnswerRow.clean connection)
    ;;
  end
end
