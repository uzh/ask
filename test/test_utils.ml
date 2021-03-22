open Lwt.Syntax

exception Exception of string

let lwt_check_raises f msg =
  let* result =
    Lwt.catch
      (fun () ->
        let* _ = f () in
        Lwt.return @@ Ok ())
      (fun e -> Lwt.return @@ Error e)
  in
  match result with
  | Ok () -> Alcotest.fail "No exception was thrown"
  | Error (Exception e_msg) ->
    Alcotest.(check string "Correct exception" msg e_msg);
    Lwt.return ()
  | Error (Quest.MariaDb.Questionnaire.Exception e_msg) ->
    Alcotest.(check string "Correct exception" msg e_msg);
    Lwt.return ()
  | Error CCResult.Get_error ->
    Alcotest.(check string "Correct exception" msg "CCResult.Get_error");
    Lwt.return ()
  | Error e ->
    let msg = Caml.Printexc.to_string e
    and stack = Caml.Printexc.get_backtrace () in
    let err_msg = Printf.sprintf "DB: %s%s\n" msg stack in
    Alcotest.fail @@ "Unexpected exception thrown " ^ err_msg
;;

let setup_test () =
  let file_configuration = Sihl.Configuration.read_env_file () in
  let () = Sihl.Configuration.store @@ Option.value file_configuration ~default:[] in
  let () = Logs.set_level (Some Logs.Error) in
  let () = Logs.set_reporter Sihl.Log.default_reporter in
  ()
;;
