open Jest
open Expect

let () = describe "Virtual_dom" (fun () -> 
  let inner_html node =
    Virtual_dom.empty ()
    |> Virtual_dom.patch (fun _ -> ()) node
    |> Virtual_dom.element
    |> Webapi.Dom.Element.innerHTML 
  in

  let namespace_uri node =
    let get_namespace_uri = [%raw {|
      function(el) {
        return el.namespaceURI;
      }
    |}] in
    Virtual_dom.empty ()
    |> Virtual_dom.patch (fun _ -> ()) node
    |> Virtual_dom.element
    |> get_namespace_uri
  in

  describe "Node" (fun () -> 
    describe "#node" (fun () -> 
      test "returns a dom node" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] 
          [ Virtual_dom.Node.node "span" [] []
          ] 
          |> inner_html
        )
        |> toBe("<span></span>")
      ); 
      test "returns nested dom nodes" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] 
          [ Virtual_dom.Node.node "span" [] 
            [ Virtual_dom.Node.node "span" [] []
            ; Virtual_dom.Node.node "span" [] []
            ]
          ] 
          |> inner_html
        )
        |> toBe("<span><span></span><span></span></span>")
      );       
    );
    describe "#node_ns" (fun () -> 
      test "returns a dom node" (fun () ->
        expect (
          Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "div" [] 
          [ Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "span" [] []
          ] 
          |> inner_html
        )
        |> toBe("<span></span>")
      ); 
      test "returns nested dom nodes" (fun () ->
        expect (
          Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "div" [] 
          [ Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "span" [] 
            [ Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "span" [] []
            ; Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "span" [] []
            ]
          ] 
          |> inner_html
        )
        |> toBe("<span><span></span><span></span></span>")
      );  

      test "returns a vnode with a custom namespace" (fun () ->
        expect (
          Virtual_dom.Node.node_ns "http://www.w3.org/2000/svg" "div" [] []
          |> namespace_uri
        )
        |> toBe("http://www.w3.org/2000/svg")
      );     
    );

    describe "#text" (fun () -> 
      test "returns a text node" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] 
          [ Virtual_dom.Node.node "span" [] 
            [ Virtual_dom.Node.text "Hello World!"
            ]
          ] 
          |> inner_html
        )
        |> toBe("<span>Hello World!</span>")
      );
    );    
  );

  describe "Attribute" (fun () -> 
    describe "#property" (fun () -> 
      test "returns a dom with properties assigned" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] [ 
            Virtual_dom.Node.node "table" [ 
              Virtual_dom.Attribute.property "className" (BsOakJson.Encode.string "is-hidden"); 
              Virtual_dom.Attribute.property "width" (BsOakJson.Encode.int 123)
            ] []
          ] 
          |> inner_html
        )
        |> toBe("<table class=\"is-hidden\" width=\"123\"></table>")
      );
    );
    describe "#attribute_ns" (fun () -> 
      test "returns a dom with namespaced properties assigned" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] [ 
            Virtual_dom.Node.node "circle" [ 
              Virtual_dom.Attribute.attribute_ns "http://www.w3.org/1999/xlink"  "xlink:href" "http://example.com";
            ] []
          ] 
          |> inner_html
        )
        |> toBe("<circle xlink:href=\"http://example.com\"></circle>")
      );
    );
    describe "#attribute" (fun () -> 
      test "returns a dom with attributes assigned" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] [ 
            Virtual_dom.Node.node "table" [ 
              Virtual_dom.Attribute.attribute "class" "is-active"; 
              Virtual_dom.Attribute.attribute "width" "234"
            ] []
          ] 
          |> inner_html
        )
        |> toBe("<table class=\"is-active\" width=\"234\"></table>")
      );
    );    
  );
)