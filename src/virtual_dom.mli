module Property : sig
  type 'a t

  type 'a options =
    { message: 'a
    ; stop_propagation: bool
    ; prevent_default: bool 
    }

  type 'a handler =
    | Normal of 'a BsOakJson.Decode.decoder
    | MayStopPropagation of ('a * bool) BsOakJson.Decode.decoder
    | MayPreventDefault of ('a * bool) BsOakJson.Decode.decoder
    | Custom of 'a options BsOakJson.Decode.decoder

  val create : string -> BsOakJson.Encode.value -> 'a t

  val create_ns : string -> string -> BsOakJson.Encode.value -> 'a t

  val on : string -> 'a handler -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Node : sig
  type 'a t

  val text : string -> 'a t

  val node : string -> 'a Property.t list -> 'a t list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

type t

val empty : unit -> t

val create : Dom.element -> t

val patch : ('a -> unit) -> 'a Node.t -> t -> t

val element : t -> Dom.element