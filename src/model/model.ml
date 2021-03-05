module Conformist = Conformist

let rec find_index (index : int) ~(predicate : 'a -> bool) (list : 'a list) : int option =
  match list with
  | [] -> None
  | x :: xs -> if predicate x then Some index else find_index (index + 1) ~predicate xs
;;

let update_at ~(index : int) ~(f : 'a -> 'a) (list : 'a list) : 'a list =
  if index < 0
  then list
  else (
    let head = Base.List.take list index in
    let tail = Base.List.drop list index in
    match tail with
    | x :: xs -> head @ f x :: xs
    | _ -> list)
;;

module AnswerInput = struct
  type uuid = string option [@@deriving yojson, show, eq]
  type filename = string [@@deriving yojson, show, eq]
  type size = int [@@deriving yojson, show, eq]
  type mime_type = string [@@deriving yojson, show, eq]
  type data = string [@@deriving yojson, show, eq]
  type text_answer = string [@@deriving yojson, show, eq]

  type asset_answer = uuid * filename * size * mime_type * data
  [@@deriving yojson, show, eq]

  type t =
    | Text of text_answer
    | Asset of asset_answer
  [@@deriving yojson, show, eq]

  let text answer =
    match answer with
    | Text text -> text
    | Asset (_, filename, _, _, _) -> filename
  ;;

  let mime_to_ext mime =
    match mime with
    | "application/pdf" -> "pdf"
    | _ -> "pdf"
  ;;
end

module Event = struct
  type questionnaire_id = string [@@deriving yojson, show, eq]
  type question_id = string [@@deriving yojson, show, eq]

  type t =
    | TextAnswerCreated of questionnaire_id * question_id * AnswerInput.text_answer
    | TextAnswerUpdated of questionnaire_id * question_id * AnswerInput.text_answer
    | AssetAnswerCreated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerUpdated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerDelete of questionnaire_id * question_id
  [@@deriving yojson, show, eq]
end

module Question = struct
  type required = bool [@@deriving yojson, show, eq]
  type label = string option [@@deriving yojson, show, eq]
  type help_text = string option [@@deriving yojson, show, eq]
  type text = string [@@deriving yojson, show, eq]
  type id = string [@@deriving yojson, show, eq]
  type regex = string [@@deriving yojson, show, eq]
  type possible_options = string list [@@deriving yojson, show, eq]
  type mime_types = string list [@@deriving yojson, show, eq]
  type max_file_size = int [@@deriving yojson, show, eq]

  type t =
    | Text of id * label * help_text * text * string option * regex * required
    | Country of id * label * help_text * text * required
    | Select of id * label * help_text * text * possible_options * required
    | YesNo of id * label * help_text * text * required
    | Date of id * label * help_text * text * required
    | File of id * label * help_text * text * mime_types * max_file_size * required
    | Year of id * label * help_text * text * required
  [@@deriving yojson, show, eq]

  let uuid question =
    match question with
    | Text (id, _, _, _, _, _, _) -> id
    | Country (id, _, _, _, _) -> id
    | Select (id, _, _, _, _, _) -> id
    | YesNo (id, _, _, _, _) -> id
    | Date (id, _, _, _, _) -> id
    | File (id, _, _, _, _, _, _) -> id
    | Year (id, _, _, _, _) -> id
  ;;

  let help_text question =
    match question with
    | Text (_, _, help_text, _, _, _, _) -> help_text
    | Country (_, _, help_text, _, _) -> help_text
    | Select (_, _, help_text, _, _, _) -> help_text
    | YesNo (_, _, help_text, _, _) -> help_text
    | Date (_, _, help_text, _, _) -> help_text
    | File (_, _, help_text, _, _, _, _) -> help_text
    | Year (_, _, help_text, _, _) -> help_text
  ;;

  let text question =
    match question with
    | Text (_, _, _, text, _, _, _) -> text
    | Country (_, _, _, text, _) -> text
    | Select (_, _, _, text, _, _) -> text
    | YesNo (_, _, _, text, _) -> text
    | Date (_, _, _, text, _) -> text
    | File (_, _, _, text, _, _, _) -> text
    | Year (_, _, _, text, _) -> text
  ;;

  let label question =
    match question with
    | Text (_, Some label, _, _, _, _, _) -> label
    | Country (_, Some label, _, _, _) -> label
    | Select (_, Some label, _, _, _, _) -> label
    | YesNo (_, Some label, _, _, _) -> label
    | Date (_, Some label, _, _, _) -> label
    | File (_, Some label, _, _, _, _, _) -> label
    | Year (_, Some label, _, _, _) -> label
    | _ -> text question
  ;;

  let is_file question =
    match question with
    | File (_, _, _, _, _, _, _) -> true
    | _ -> false
  ;;

  let is_required question =
    match question with
    | Text (_, _, _, _, _, _, required) -> required
    | Country (_, _, _, _, required) -> required
    | Select (_, _, _, _, _, required) -> required
    | YesNo (_, _, _, _, required) -> required
    | Date (_, _, _, _, required) -> required
    | File (_, _, _, _, _, _, required) -> required
    | Year (_, _, _, _, required) -> required
  ;;

  let set_optional question =
    match question with
    | Text (a, b, c, d, e, f, _) -> Text (a, b, c, d, e, f, false)
    | Country (a, b, c, d, _) -> Country (a, b, c, d, false)
    | Select (a, b, c, d, e, _) -> Select (a, b, c, d, e, false)
    | YesNo (a, b, c, d, _) -> YesNo (a, b, c, d, false)
    | Date (a, b, c, d, _) -> Date (a, b, c, d, false)
    | File (a, b, c, d, e, f, _) -> File (a, b, c, d, e, f, false)
    | Year (a, b, c, d, _) -> Year (a, b, c, d, false)
  ;;

  (* let question_shema question = let open Conformist in match question with | Text (id,
     label, text, default, regex, required) -> make Field.[ string "id"; string "label";
     string "text" ] { id; label; text; default; regex; required } | Country () ;; *)

  let validation_error uuid message = Error (Caml.Format.asprintf "%s,%s" uuid message)

  let validate question answer_input =
    match question, answer_input with
    | question, None ->
      (match is_required question with
      | true -> validation_error (uuid question) "Required"
      | false -> Ok ())
    | Text (_, _, _, _, _, regex, _), Some (AnswerInput.Text answer) ->
      let regex = Sihl.Utils.Regex.of_string regex in
      (match Base.String.is_empty answer, Sihl.Utils.Regex.test regex answer with
      | true, _ -> Ok ()
      | false, true -> Ok ()
      | false, false -> validation_error (uuid question) "Invalid value provided")
    | Country (_, _, _, _, _), Some (AnswerInput.Text _) -> Ok ()
    | Select (_, _, _, _, options, _), Some (AnswerInput.Text answer) ->
      (match Base.List.find options ~f:(String.equal answer) |> Base.Option.is_some with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please select on of the options")
    | YesNo (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      let regex = Sihl.Utils.Regex.of_string "^Yes$|^No$" in
      (match Sihl.Utils.Regex.test regex answer with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please answer with 'Yes' or 'No'")
    | Year (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      let regex = Sihl.Utils.Regex.of_string "^(1|2)\\d\\d\\d$" in
      (match Sihl.Utils.Regex.test regex answer with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please enter a year in the format 1999")
    | Date (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      let regex =
        Sihl.Utils.Regex.of_string "^(1|2)\\d\\d\\d\\-(0|1)\\d\\-(0|1|2|3)\\d$"
      in
      (match Sihl.Utils.Regex.test regex answer with
      | true -> Ok ()
      | false ->
        validation_error (uuid question) "Please enter a date in the format 1990-11-25")
    | ( File (_, _, _, _, supported_mime_types, max_file_sizeMb, _)
      , Some (Asset (_, _, size_byte, mime_type, _)) ) ->
      let size_mb =
        Base.Float.of_int size_byte *. (10. ** -6.)
        |> Base.Float.round_up
        |> Base.Int.of_float
      in
      (match size_mb <= max_file_sizeMb && List.mem mime_type supported_mime_types with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Invalid value provided")
    | question, _ -> validation_error (uuid question) "Invalid value provided"
  ;;

  let is_valid question answer_input =
    let validation = validate question answer_input in
    match validation with
    | Ok _ -> true
    | Error _ -> false
  ;;
end

module QuestionAnswerInput = struct
  type t = Question.t * AnswerInput.t option [@@deriving yojson, show, eq]

  let are_all_required_questions_answered question_input =
    question_input
    |> List.map (fun (question, answer) ->
           match Question.is_required question, answer with
           | true, Some _ -> true
           | true, None -> false
           | false, _ -> true)
    |> List.for_all (fun x -> x)
  ;;

  let are_all_answered_questions_valid question_input =
    question_input
    |> List.map (fun ((question : Question.t), answer) ->
           match answer with
           | Some answer -> Question.is_valid question answer
           | None -> true)
    |> List.for_all (fun x -> x)
  ;;

  let can_questions_answered_get_submitted question_input =
    are_all_required_questions_answered question_input
    && are_all_answered_questions_valid question_input
  ;;

  let filter_asset_out question_answers =
    List.filter
      (fun (question, _) ->
        match question with
        | Question.File _ -> false
        | _ -> true)
      question_answers
  ;;

  let update question_answers question answer =
    question_answers
    |> find_index 0 ~predicate:(fun (existing_question, _) ->
           Question.uuid existing_question == Question.uuid question)
    |> Option.map (fun index ->
           update_at ~index ~f:(fun (question, _) -> question, answer) question_answers)
    |> Option.value ~default:question_answers
  ;;

  let event questionnaire_id current updated =
    match current, updated with
    | (_, Some (AnswerInput.Text _)), (question, Some (AnswerInput.Text new_text)) ->
      Some (Event.TextAnswerUpdated (questionnaire_id, Question.uuid question, new_text))
    | (_, None), (question, Some (AnswerInput.Text new_text)) ->
      Some (Event.TextAnswerCreated (questionnaire_id, Question.uuid question, new_text))
    | (_, Some (AnswerInput.Asset _)), (question, Some (AnswerInput.Asset asset_answer))
      ->
      Some
        (Event.AssetAnswerUpdated (questionnaire_id, Question.uuid question, asset_answer))
    | (_, None), (question, Some (AnswerInput.Asset asset_answer)) ->
      Some
        (Event.AssetAnswerCreated (questionnaire_id, Question.uuid question, asset_answer))
    | (_, Some (AnswerInput.Asset _)), (question, None) ->
      Some (Event.AssetAnswerDelete (questionnaire_id, Question.uuid question))
    | _, _ -> None
  ;;
end

module Questionnaire = struct
  type t =
    { uuid : string
    ; template_uuid : string
    ; label : string
    ; description : string
    ; questions : QuestionAnswerInput.t list
    }
  [@@deriving yojson, show, fields, make, eq]

  let set_questions questions questionnaire = { questionnaire with questions }

  let is_ready_for_submission questionnaire =
    questionnaire.questions |> QuestionAnswerInput.are_all_required_questions_answered
  ;;

  let set_question_to_optional (question, answer) = Question.set_optional question, answer

  let set_question_with_id_to_optional ~question_id ~questions =
    questions
    |> List.map (fun (q, a) ->
           match Question.uuid q == question_id with
           | true -> set_question_to_optional (q, a)
           | false -> q, a)
  ;;

  let set_all_questions_to_optional questionnaire =
    let questions = questionnaire.questions |> List.map set_question_to_optional in
    { questionnaire with questions }
  ;;

  let answer questionnaire answers =
    let open Base in
    let rec loop questions errors events =
      match questions with
      | ((question, _) as current) :: questions ->
        let answer =
          List.find answers ~f:(fun (answer_question, _) ->
              Question.equal answer_question question)
        in
        let answer_input = answer |> Option.bind ~f:(fun (_, answer) -> answer) in
        (match Question.validate question answer_input, answer with
        | Error msg, _ -> loop questions (List.cons msg errors) events
        | Ok (), Some answer ->
          (match QuestionAnswerInput.event questionnaire.uuid current answer with
          | Some event -> loop questions errors (List.cons event events)
          | None -> loop questions errors events)
        | Ok (), None -> loop questions errors events)
      | [] -> events |> List.rev, errors |> List.rev
    in
    let events, errors =
      match answers with
      | [ (question, _) ] when Question.is_file question ->
        let questions =
          List.find questionnaire.questions ~f:(fun (questionnaire_question, _) ->
              Question.equal questionnaire_question question)
          |> Option.map ~f:(fun q -> [ q ])
          |> Option.value ~default:[]
        in
        loop questions [] []
      | _ ->
        let questions = QuestionAnswerInput.filter_asset_out questionnaire.questions in
        loop questions [] []
    in
    match Base.List.is_empty errors with
    | true -> Ok events
    | false -> Error errors
  ;;
end

module AttributeTemplate = struct
  let personal_details = "314afbde-caf6-4164-b86d-6b09ebefdb40"
  let qualified = "f446853b-e341-4e1d-afc9-a86d38f44f42"
  let sample = "3b6a6800-148b-4424-8b18-3ac198b349f6"
  let general = "365f7dbf-1356-4d82-9dba-8735c2c2e5ec"
end

module Attribute = struct
  type t =
    { uuid : string
    ; number : string
    ; attribute_user : string
    ; personal_details : string
    ; qualified : string
    ; sample : string
    ; general : string
    ; note : string
    ; created : Ptime.t
    ; updated : Ptime.t
    }
  [@@deriving show, eq, make, fields]

  let create
      ~id
      ~attribute_user
      ~personal_details
      ~qualified
      ~sample
      ~general
      ?created
      ?updated
      ()
    =
    { uuid = id
    ; number = ""
    ; attribute_user
    ; personal_details
    ; qualified
    ; sample
    ; general
    ; note = ""
    ; created = Option.value ~default:(Ptime_clock.now ()) created
    ; updated = Option.value ~default:(Ptime_clock.now ()) updated
    }
  ;;

  let t =
    let encode m =
      Ok
        ( m.uuid
        , ( m.number
          , ( m.attribute_user
            , ( m.personal_details
              , (m.qualified, (m.sample, (m.general, (m.note, (m.created, m.updated)))))
              ) ) ) )
    in
    let decode
        ( uuid
        , ( number
          , ( attribute_user
            , ( personal_details
              , (qualified, (sample, (general, (note, (created, updated))))) ) ) ) )
      =
      Ok
        { uuid
        ; number
        ; attribute_user
        ; personal_details
        ; qualified
        ; sample
        ; general
        ; note
        ; created
        ; updated
        }
    in
    let open Caqti_type in
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            string
            (tup2
               string
               (tup2
                  string
                  (tup2
                     string
                     (tup2 string (tup2 string (tup2 string (tup2 ptime ptime)))))))))
  ;;

  let has_questionnaire user_attribute questionnaire_id =
    String.equal user_attribute.personal_details questionnaire_id
    || String.equal user_attribute.qualified questionnaire_id
    || String.equal user_attribute.sample questionnaire_id
    || String.equal user_attribute.general questionnaire_id
  ;;

  let fail_if_doesnt_have_questionnaire user_attribute questionnaire_id =
    match has_questionnaire user_attribute questionnaire_id with
    | true -> Ok ()
    | false -> Error "Questionnaire doesn't belong to the attribute"
  ;;

  let set_note note attribute = { attribute with note }
end

module FullAttribute = struct
  type t =
    { uuid : string
    ; number : string
    ; attribute_user : Sihl.User.t
    ; personal_details : Questionnaire.t
    ; qualified : Questionnaire.t
    ; sample : Questionnaire.t
    ; general : Questionnaire.t
    ; note : string
    ; created : Ptime.t
    ; updated : Ptime.t
    }
  [@@deriving show, eq, fields]

  let create
      ~(attribute : Attribute.t)
      ~personal_details
      ~qualified
      ~sample
      ~general
      ~attribute_user
    =
    { uuid = Attribute.uuid attribute
    ; number = Attribute.number attribute
    ; attribute_user
    ; personal_details
    ; qualified
    ; sample
    ; general
    ; note = Attribute.note attribute
    ; created = attribute.created
    ; updated = attribute.updated
    }
  ;;
end
