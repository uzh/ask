module type Sig = sig
  val lifecycles : Sihl.Container.Lifecycle.t list
  val register_migration : unit -> unit
  val register_cleaner : unit -> unit
  (* val get : string -> Quest.Model.t option Lwt.t *)
  (* val get_by_label : string -> Quest.Model.t option Lwt.t *)
  (* val insert : Quest.Model.t -> unit Lwt.t *)
  (* val update : Quest.Model.t -> unit Lwt.t *)
end
