open Lwt.Syntax

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
  | Error (Quest.Service.Exception e_msg) ->
    Alcotest.(check string "Correct exception" msg e_msg);
    Lwt.return ()
  | Error e ->
    let msg = Caml.Printexc.to_string e
    and stack = Caml.Printexc.get_backtrace () in
    let err_msg = Printf.sprintf "DB: %s%s\n" msg stack in
    Alcotest.fail @@ "Unexpected exception thrown " ^ err_msg
;;

(* TODO use set_database_url from run *)
let set_database_url () =
  let () =
    if Sys.getenv_opt "DATABASE_URL" == None
    then (
      match
        ( Sys.getenv_opt "SIHL_ENV"
        , Sys.getenv_opt "DEVELOPMENT_DATABASE_URL"
        , Sys.getenv_opt "TEST_DATABASE_URL" )
      with
      | Some "test", _, Some database_url -> Unix.putenv "DATABASE_URL" database_url
      | Some "development", Some database_url, _ ->
        Unix.putenv "DATABASE_URL" database_url
      | _, _, _ -> Unix.putenv "DATABASE_URL" "mariadb://root:password@127.0.0.1:3306/dev")
  in
  ()
;;

let setup_test () =
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter Sihl.Log.default_reporter;
  let () = set_database_url () in
  ()
;;
