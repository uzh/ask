open Lwt.Syntax

let name = "quest"
let log_src = Logs.Src.create name

module Logs = (val Logs.src_log log_src : Logs.LOG)

module type Sig = sig
  module Questionnaire : sig
    val handle_event : Model.Event.t -> unit Lwt.t

    val instantiate_questionnaire
      :  template_id:string
      -> questionnaire_id:string
      -> string Lwt.t

    val find : string -> Model.Questionnaire.t option Lwt.t

    val create_template
      :  ?id:string
      -> label:string
      -> ?description:string
      -> unit
      -> string Lwt.t

    val answer
      :  Model.Questionnaire.t
      -> (Model.Question.t * Model.AnswerInput.t option) list
      -> unit Lwt.t

    val delete_asset_answer : questionnaire_id:string -> question_id:string -> unit Lwt.t
    val add_question : template_id:string -> order:int -> Model.Question.t -> string Lwt.t
  end

  val register : unit -> Sihl.Container.Service.t

  include Sihl.Container.Service.Sig
end

module Make (Repo : Repository.Sig) (StorageRepo : Sihl.Service.Storage.Repo.Sig) = struct
  module Storage = Sihl.Service.Storage.Make (StorageRepo)

  module Questionnaire = struct
    exception Exception of string

    let handle_event event =
      match event with
      | Model.Event.TextAnswerCreated (questionnaire_id, question_id, text) ->
        Repo.Questionnaire.create_text_answer
          ~answer_id:(Uuidm.create `V4 |> Uuidm.to_string)
          ~questionnaire_id
          ~question_id
          ~text
      | Model.Event.AssetAnswerCreated
          (questionnaire_id, question_id, (_, filename, size, mime, data)) ->
        let asset_id = Uuidm.create `V4 |> Uuidm.to_string in
        let file = Sihl.Storage.File.make ~id:asset_id ~filename ~filesize:size ~mime in
        let* _ = Storage.upload_base64 ~file ~base64:data in
        Repo.Questionnaire.create_asset_answer
          ~answer_id:(Uuidm.create `V4 |> Uuidm.to_string)
          ~questionnaire_id
          ~question_id
          ~asset_id
      | Model.Event.TextAnswerUpdated (questionnaire_id, question_id, text) ->
        Repo.Questionnaire.update_text_answer ~questionnaire_id ~question_id ~text
      | Model.Event.AssetAnswerUpdated
          (questionnaire_id, question_id, (_, filename, size, mime, data)) ->
        let* asset_id =
          Repo.Questionnaire.find_asset_id ~questionnaire_id ~question_id
          |> Lwt.map
               (CCOpt.to_result
                  (Caml.Format.asprintf
                     "Asset id not found for questionnaire_id %s and question_id %s"
                     questionnaire_id
                     question_id))
          |> Lwt.map CCResult.get_or_failwith
        in
        let* file = Storage.find ~id:asset_id in
        let updated_file =
          let open Sihl.Storage in
          file
          |> Stored.set_filename filename
          |> Stored.set_filesize size
          |> Stored.set_mime mime
        in
        let* _ = Storage.update_base64 ~file:updated_file ~base64:data in
        Repo.Questionnaire.update_asset_answer ~questionnaire_id ~question_id ~asset_id
      | Model.Event.AssetAnswerDelete (questionnaire_id, question_id) ->
        let* asset_id =
          Repo.Questionnaire.find_asset_id ~questionnaire_id ~question_id
          |> Lwt.map
               (CCOpt.to_result
                  (Caml.Format.asprintf
                     "Asset id not found for questionnaire_id %s and question_id %s"
                     questionnaire_id
                     question_id))
          |> Lwt.map CCResult.get_or_failwith
        in
        let* _ = Storage.delete ~id:asset_id in
        Repo.Questionnaire.delete_answer ~questionnaire_id ~question_id
    ;;

    let instantiate_questionnaire ~template_id ~questionnaire_id =
      let* () = Repo.Questionnaire.create ~template_id ~questionnaire_id in
      Lwt.return questionnaire_id
    ;;

    let find id = Repo.Questionnaire.find id

    let create_template ?id ~label ?description () =
      let id = id |> CCOpt.value ~default:(Uuidm.create `V4 |> Uuidm.to_string) in
      let* () = Repo.Questionnaire.create_template ~id ~label ~description in
      Lwt.return id
    ;;

    let answer questionnaire answers =
      let events = Model.Questionnaire.answer questionnaire answers |> CCResult.get_exn in
      let rec handle_events events =
        match events with
        | event :: events ->
          let* () = handle_event event in
          handle_events events
        | [] -> Lwt.return ()
      in
      handle_events events
    ;;

    let add_question ~template_id ~order question =
      let question_id = Model.Question.uuid question in
      let mapping_id = Uuidm.create `V4 |> Uuidm.to_string in
      Sihl.Service.Database.transaction (fun connection ->
          let* () = Repo.Questionnaire.create_question ~connection ~question in
          let* () =
            Repo.Questionnaire.create_mapping
              ~connection
              ~id:mapping_id
              ~template_id
              ~question_id
              ~order
              ~is_required:(Model.Question.is_required question)
          in
          Lwt.return question_id)
    ;;

    let delete_asset_answer ~questionnaire_id ~question_id =
      handle_event (Model.Event.AssetAnswerDelete (questionnaire_id, question_id))
    ;;
  end

  let start () = Lwt.return ()
  let stop _ = Lwt.return ()

  let lifecycle =
    Sihl.Container.Lifecycle.create "quest" ~dependencies:Repo.lifecycles ~start ~stop
  ;;

  let register () =
    Repo.register_migration ();
    Repo.register_cleaner ();
    Sihl.Container.Service.create lifecycle
  ;;
end

module MigrationRepo = Sihl.Service.Migration_repo.MariaDb
module MigrationService = Sihl.Service.Migration.Make (MigrationRepo)
module StorageRepo = Sihl.Service.Storage_repo.MakeMariaDb (MigrationService)
module MariaDb = Make (Repository.MariaDB (MigrationService)) (StorageRepo)
