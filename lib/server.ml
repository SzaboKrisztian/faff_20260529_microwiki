let run () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun _request -> Dream.html "Hello from Microwiki") ]
