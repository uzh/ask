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
    let (Dynparam.Pack (pt, pv)) = values in
    Connection.find (is_unique_request table_name sql_filter pt sql_joins) pv
    |> Lwt.map raise_caqti_error
  | Some uuid ->
    let values = Dynparam.add Caqti_type.string uuid values in
    let (Dynparam.Pack (pt, pv)) = values in
    Connection.find (is_unique_with_uuid_request table_name sql_filter pt sql_joins) pv
    |> Lwt.map raise_caqti_error
;;
