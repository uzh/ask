# Ask

Questionnaires with support for creating and answering questions

_Note that it's still under active development and it only supports mariadb so far._

## Ask package

### About

Ask allows you to create and answer Questionnaires. Additionaly it provides templating for quesitonnaires.

### Usage

```ocaml
let empty_questionnaire_with_three_questions ~label =
    let open AskService.Questionnaire in
    (* Create a template, e.g. label:"details-template" *)
    let* template_id = create_template ~label () in

    (* Creating Questions *)
    let question1 =
        Model.Question.Text
            ( Uuidm.v `V4 |> Uuidm.to_string
            , Some "age"
            , None
            , "How old are you?"
            , None
            , ""
            , true )
        in
    let question2 =
        Model.Question.YesNo
            ( Uuidm.v `V4 |> Uuidm.to_string
            , Some "student"
            , None
            , "Are you a student?"
            , false )
        in
    let question3 =
        Model.Question.File
            ( Uuidm.v `V4 |> Uuidm.to_string
            , Some "diploma"
            , None
            , "Upload your diploma."
            , [ "application/pdf" ]
            , 100000
            , true )
        in

    (* Adding the questions to the template *)
    let* _ = add_question ~template_id ~order:0 question1 in
    let* _ = add_question ~template_id ~order:1 question2 in
    let* _ = add_question ~template_id ~order:2 question3 in

    (* Instantiate a new questionnaire *)
    let* questionnaire_id =
            instantiate_questionnaire
            ~template_id
            ~questionnaire_id:(Uuidm.v `V4 |> Uuidm.to_string)
        in

    (* Find previously created questionnaire *)
    let* questionnaire =
        find questionnaire_id
        |> Lwt.map (CCOption.to_result "Seed failed, can not create questionnaire")
        |> Lwt.map CCResult.get_or_failwith
      in
      Lwt.return (questionnaire, question1, question2, question3)
;;

let questionnaire_with_three_answered_questions ~label =
      let open AskService.Questionnaire in

      (* Creating an empty questionnaire with the function above *)
      (* e.g. label:"details-template" *)
      let* questionnaire, question1, question2, question3 =
        empty_questionnaire_with_three_questions ~label
      in

      (* Create answers *)
      let answer1 = Some (Model.AnswerInput.Text "18") in
      let answer2 = Some (Model.AnswerInput.Text "Yes") in
      let answer3 =
        Some
          (Model.AnswerInput.Asset
             (None, "new_diploma.pdf", 12345, "application/pdf", "aGVsbG9vb28="))
      in

      (* answering questions *)
      let* () = answer questionnaire [ question1, answer1; question2, answer2 ] in
      (* We have to upload file separately *)
      let* () = answer questionnaire [ question3, answer3 ] in

      (* find answered questionnaire *)
      let questionnaire_id = Model.Questionnaire.uuid questionnaire in
      let* questionnaire =
        find questionnaire_id
        |> Lwt.map (CCOption.to_result "No questionnaire found")
        |> Lwt.map CCResult.get_or_failwith
      in
      Lwt.return (Ok (questionnaire, question1, question2, question3))
    ;;

```

## Ask integrator package

A helper to assign a questionniare to any type which has a uuid.

Quest allows you to create Questionnaires, add some questions and answer them in an easy way. The integrator adds the opportunity to add the questionnaire to any type which has an uuid.

## Usage

```ocaml
let ask_integrator_with_one_filled_questionnaire =
    let open Ask_integrator.Model in

    (* member id has the format uuid and is used to link one of your models (e.g. to a User) *)
    let member_id = Uuidm.v `V4 |> Uuidm.to_string in
    (* member label is a string which should help you where the member id is comming from *)
    let member_label = "user" in
    (* questionnaire_label is a label for the linked questionnaire, it's possible to search for that label for a specified integrator *)
    let questionnaire_label = "personal details" in


    (* Create an answered questionnaire, see function above *)
    let* questionnaire, _, _, _ =
        QuestTest.questionnaire_with_three_answered_questions ~label:"details-template"
        |> Lwt.map CCResult.get_or_failwith
    in

    (* Create the integrator object *)
    AskService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
;;
```

## Development

The project is prepared for development with [VS Code and DevContainers.](./.devcontainer/README.md)
