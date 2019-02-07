module Attribute = struct
  external attribute_hook: string -> BsOakJson.Encode.value -> BsOakJson.Encode.value = "virtual-dom/virtual-hyperscript/hooks/attribute-hook.js" [@@bs.module]

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
    
  type _ t =
    | Prop : string * BsOakJson.Encode.value -> _ t
    | PropNS : string * string * BsOakJson.Encode.value -> _ t
    | Attr : string * string -> _ t
    | On : string * 'a handler -> 'a t
    | Tagger : ('a -> 'b) * 'a t -> 'b t

  let attribute key value =
    Attr (key, value)

  let property key value =
    Prop (key, value)

  let property_ns namespace key value =
    PropNS (namespace, key, value)

  let on key handler =
    On (key, handler)

  let map tagger property =
    Tagger (tagger, property)

  let to_dict callback properties =
    let props = Js.Dict.empty () in
    let attrs = Js.Dict.empty () in

    let handler_decoder = function
      | Normal decoder ->
        decoder
        |> BsOakJson.Decode.map (fun msg ->
          { message = msg
          ; stop_propagation = false
          ; prevent_default = false 
          })
      | MayStopPropagation decoder ->
        decoder
        |> BsOakJson.Decode.map (fun (msg, stop_propagation) ->
          { message = msg
          ; stop_propagation = stop_propagation
          ; prevent_default = false 
          })
      | MayPreventDefault decoder ->
        decoder
        |> BsOakJson.Decode.map (fun (msg, prevent_default) ->
          { message = msg
          ; stop_propagation = false
          ; prevent_default = prevent_default 
          })
      | Custom decoder ->
        decoder
    in

    let apply_event_options event option =
      let () = 
        if option.stop_propagation 
        then Webapi.Dom.Event.stopPropagation event
        else ()
      in
      let () = 
        if option.prevent_default 
        then Webapi.Dom.Event.preventDefault event
        else ()
      in
      BsOakJson.Decode.succeed option.message
    in

    let rec eval : type a . (a -> unit) -> a t -> unit = fun callback' property ->
      match property with
      | Prop (key, value) -> Js.Dict.set props key value
      | PropNS (namespace, key, value) -> Js.Dict.set props key (attribute_hook namespace value)
      | Attr (key, value) -> Js.Dict.set attrs key (BsOakJson.Encode.string value)      
      | On (key, handler) -> 
        Js.Dict.set props key (Obj.magic (fun event -> 
          let decoder = BsOakJson.Decode.and_then (apply_event_options event) (handler_decoder handler) in
          let val' = (BsOakJson.Decode.decode_value decoder (Obj.magic event)) in
          Belt.Result.mapWithDefault val' () callback' 
        ))
      | Tagger (fn, property) -> eval (fun x -> callback' (fn x))  property
    in
    let () = List.iter (eval callback) properties in
    let () = Js.Dict.set props "attributes" (Obj.magic attrs) in
    props
end

module Vnode = struct
  type t
  
  external create : string -> BsOakJson.Encode.value Js.Dict.t -> t array -> t = "h" 
  [@@bs.module "virtual-dom/index.js"]
  
  external create_text : string -> t = "VText" 
  [@@bs.module "virtual-dom/index.js"]
  [@@bs.new]
end

module Node = struct
  type _ t =
    | Text : string -> _ t
    | Node : string * 'a Attribute.t list * 'a t list -> 'a t
    | Tagger : ('a -> 'b) * 'a t -> 'b t

  let text str =
    Text str

  let node tag attributes children =
    (* prevent xss attack vector *)
    let tag = if tag = "script" then "p" else tag in
    Node (tag, attributes, children)

  let node_ns namespace tag attributes children =
    let ns_prop = Attribute.property "namespace" (BsOakJson.Encode.string namespace) in
    node tag (ns_prop :: attributes) children

  let key_node_pair_to_node (key, node') =
    match node' with
    | Node (tag, attributes, children) -> 
      let key_attribute = Attribute.property "key" (BsOakJson.Encode.string key) in
      Node (tag, key_attribute :: attributes, children)
    | _ -> node'

  let keyed_node tag attributes key_child_pairs =
    node tag attributes (List.map key_node_pair_to_node key_child_pairs)
  
  let keyed_node_ns namespace tag attributes key_child_pairs =
    node_ns namespace tag attributes (List.map key_node_pair_to_node key_child_pairs)

  let map tagger node =
    Tagger (tagger, node)

  let rec to_vnode : type a . (a -> unit) -> a t -> Vnode.t = fun cb node ->
    match node with
    | Text text -> Vnode.create_text text
    | Node (tag, attributes, children) ->
      let attr_dict = Attribute.to_dict cb attributes in
      let children_ar =      
        List.map (to_vnode cb) children 
        |> Array.of_list
      in
      Vnode.create tag attr_dict children_ar
    | Tagger (fn, node) -> to_vnode  (fun x -> cb(fn x))  node
end

type t = (Dom.element * Vnode.t)

type patch

external diff_vnodes: Vnode.t -> Vnode.t -> patch array = "diff"
[@@bs.module "virtual-dom/index.js"]

external create_element : Vnode.t -> Dom.element = "create"
[@@bs.module "virtual-dom/index.js"]

external patch_element : Dom.element -> patch array -> Dom.element = "patch"
[@@bs.module "virtual-dom/index.js"]

let empty _ =
  let vnode = Vnode.create_text "" in
  let element = create_element vnode in
  (element, vnode)

let create element =
  let vnode = Vnode.create_text "" in
  (element, vnode)

let patch callback new_node (element, vnode) =
  let new_vnode = Node.to_vnode callback new_node in
  let patches = diff_vnodes vnode new_vnode in
  let new_element = patch_element element patches in
  (new_element, new_vnode)

let element (element, _) =
  element