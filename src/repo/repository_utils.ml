open Lwt.Syntax

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
end

let raise_caqti_error err =
  match err with
  | Error err -> failwith (Caqti_error.show err)
  | Ok result -> result
;;

let set_fk_check_request =
  Caqti_request.exec Caqti_type.bool "SET FOREIGN_KEY_CHECKS = ?;"
;;

let with_disabled_fk_check f =
  Sihl.Service.Database.query (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      let* () = Connection.exec set_fk_check_request false |> Lwt.map raise_caqti_error in
      Lwt.finalize
        (fun () -> f connection)
        (fun () -> Connection.exec set_fk_check_request true |> Lwt.map raise_caqti_error))
;;

let found_rows_request =
  Caqti_request.find Caqti_type.unit Caqti_type.int "SELECT FOUND_ROWS()"
;;
