open Lwt.Syntax

module MakeMariaDb (MigrationService : Sihl.Contract.Migration.Sig) : Repository.Sig =
struct
  let lifecycles = [ Sihl.Service.Database.lifecycle; MigrationService.lifecycle ]

  let raise_caqti_error err =
    match err with
    | Error err -> failwith (Caqti_error.show err)
    | Ok result -> result
  ;;

  module Migration = Repository_migration

  let register_migration () = MigrationService.register_migration (Migration.migration ())

  module Sql = struct
    module Model = Repository_model

    module Questionnaire = struct
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
        Connection.find_opt find_request id |> Lwt.map raise_caqti_error
      ;;

      let find_exn connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find find_request id |> Lwt.map raise_caqti_error
      ;;

      let get_questions_request =
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

      let get_questions connection id =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.collect_list get_questions_request id |> Lwt.map raise_caqti_error
      ;;

      let get_answer_request =
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

      let get_answer connection ids =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.find_opt get_answer_request ids |> Lwt.map raise_caqti_error
      ;;
    end

    module Template = struct
      let insert_template_request =
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

      let insert_template connection template =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_template_request template |> Lwt.map raise_caqti_error
      ;;

      let insert_questionnaire_request =
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

      let insert_questionnaire connection questionnaire =
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_questionnaire_request questionnaire
        |> Lwt.map raise_caqti_error
      ;;
    end

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

    let insert_question connection question =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec insert_question_request question |> Lwt.map raise_caqti_error
    ;;

    let insert_mapping_request =
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

    let insert_mapping connection mapping =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec insert_mapping_request mapping |> Lwt.map raise_caqti_error
    ;;

    let update_answer_request =
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

    let update_answer connection answer =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec update_answer_request answer |> Lwt.map raise_caqti_error
    ;;

    let delete_answer_request =
      Caqti_request.exec
        Caqti_type.string
        {sql|
          DELETE FROM quest_answers WHERE
          quest_answers.uuid = UNHEX(REPLACE(?, '-', ''));
        |sql}
    ;;

    let delete_answer connection answer =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec delete_answer_request answer |> Lwt.map raise_caqti_error
    ;;

    let insert_answer_input_type =
      let open Caqti_type in
      tup2
        string
        (tup2 string (tup2 string (tup2 string (tup2 (option string) (option string)))))
    ;;

    let insert_answer_request =
      Caqti_request.exec
        insert_answer_input_type
        {sql|
          INSERT INTO quest_answers (
            uuid,
            quest_questionnaire,
            quest_template_question_mapping,
            text,
            storage_handle
          ) VALUES (
            UNHEX(REPLACE(?, '-', '')),
            (SELECT id FROM quest_questionnaires
            WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', ''))),
            (SELECT id FROM quest_template_question_mappings
            WHERE quest_template_question_mappings.quest_template =
                (SELECT quest_template FROM quest_questionnaires
                WHERE quest_questionnaires.uuid = UNHEX(REPLACE(?, '-', '')))
              AND quest_template_question_mappings.quest_question =
                  (SELECT id FROM quest_questions
                  WHERE quest_questions.uuid = UNHEX(REPLACE(?, '-', '')))),
            ?,
            UNHEX(REPLACE(?, '-', ''))
          );
        |sql}
    ;;

    let insert_answer connection answer =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec insert_answer_request answer |> Lwt.map raise_caqti_error
    ;;

    let clean_questions_request =
      Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_questions;"
    ;;

    let clean_questions connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec clean_questions_request () |> Lwt.map raise_caqti_error
    ;;

    let clean_templates_request =
      Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_templates;"
    ;;

    let clean_templates connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec clean_templates_request () |> Lwt.map raise_caqti_error
    ;;

    let clean_questionnaires_request =
      Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_questionnaires;"
    ;;

    let clean_questionnaires connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec clean_questionnaires_request () |> Lwt.map raise_caqti_error
    ;;

    let clean_mappings_request =
      Caqti_request.exec
        Caqti_type.unit
        "TRUNCATE TABLE quest_template_question_mappings;"
    ;;

    let clean_mappings connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec clean_mappings_request () |> Lwt.map raise_caqti_error
    ;;

    let clean_answers_request =
      Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE quest_answers;"
    ;;

    let clean_answers connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec clean_answers_request () |> Lwt.map raise_caqti_error
    ;;

    let clean () =
      Sihl.Service.Database.query (fun connection ->
          let* () = clean_questions connection in
          let* () = clean_templates connection in
          let* () = clean_questionnaires connection in
          let* () = clean_mappings connection in
          clean_answers connection)
    ;;
  end

  let register_cleaner () = Sihl.Service.Repository.register_cleaner Sql.clean
  (* let get = Sql.get *)
  (* let get_by_label = Sql.get_by_label *)
  (* let insert = Sql.insert *)
  (* let update = Sql.update *)
end
