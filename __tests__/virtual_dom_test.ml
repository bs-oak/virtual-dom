open Jest
open Expect

let () = describe "Virtual_dom" (fun () -> 
  let inner_html node =
    Virtual_dom.empty ()
    |> Virtual_dom.patch (fun _ -> ()) node
    |> Virtual_dom.element
    |> Webapi.Dom.Element.innerHTML 
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

  describe "Property" (fun () -> 
    describe "#create" (fun () -> 
      test "returns a dom with attributes" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] 
          [ Virtual_dom.Node.node "table" 
            [ Virtual_dom.Property.create "className" (BsOakJson.Encode.string "is-hidden")
            ; Virtual_dom.Property.create "width" (BsOakJson.Encode.int 123)
            ] 
            []
          ] 
          |> inner_html
        )
        |> toBe("<table class=\"is-hidden\" width=\"123\"></table>")
      );
      test "returns a dom with namespaced attributes" (fun () ->
        expect (
          Virtual_dom.Node.node "div" [] 
          [ Virtual_dom.Node.node "table" 
            [ Virtual_dom.Property.create "className" (BsOakJson.Encode.string "is-hidden")
            ; Virtual_dom.Property.create "width" (BsOakJson.Encode.int 123)
            ; Virtual_dom.Property.create_ns "http://www.w3.org/1999/xlink" "xlink:href" (BsOakJson.Encode.string "hello")
            ] 
            []
          ] 
          |> inner_html
        )
        |> toBe("<table class=\"is-hidden\" width=\"123\" xlink:href=\"hello\"></table>")
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
  );
)