﻿namespace Owin.Hosting
module HttpListener =
  open System
  open System.Collections.Generic
  open System.IO
  open System.Net
  open System.Text
  open Owin

  type System.Net.HttpListenerContext with
    /// Creates an environment variable <see cref="HttpContextBase"/>.
    member context.ToOwinRequest() =
      let request = context.Request
      let headers = new Dictionary<string, seq<string>>() :> IDictionary<string, seq<string>>
      request.Headers.AsEnumerable() |> Seq.iter (fun (k,v) -> headers.Add(k, seq { yield v }))
      let items = new Dictionary<string, obj>() :> IDictionary<string, obj>
      items.["url_scheme"] <- request.Url.Scheme
      items.["server_name"] <- request.Url.Host
      items.["server_port"] <- request.Url.Port
      Request.fromAsync request.HttpMethod
                        (request.Url.AbsolutePath + "?" + request.Url.Query)
                        headers items (request.InputStream.AsyncRead)

  type System.Net.HttpListenerResponse with
    member response.Reply(r:IResponse) =
      if r.Headers.ContainsKey("Content-Length") then
        response.ContentType <- Seq.head(r.Headers.["Content-Length"])
      let statusCode, statusDescription = splitStatus r.Status
      response.StatusCode <- statusCode
      response.StatusDescription <- statusDescription
      r.Headers |> Dict.toSeq |> Seq.iter (fun (k,v) -> v |> Seq.iter (fun v' -> response.Headers.Add(k,v')))
      let output = response.OutputStream 
      r.GetBody()
      |> Seq.map (fun o -> o :?> bytestring |> Seq.toArray)
      //|> Seq.iter (ByteString.transfer output) 
      // Or batch it per byte[]:
      |> Seq.iter (fun buffer -> output.Write(buffer, 0, buffer.Length))
      output.Close()

  type System.Net.HttpListener with
    member listener.AsyncGetContext() = 
      Async.FromBeginEnd(listener.BeginGetContext, listener.EndGetContext)
    static member Start(url, handler:IApplication, ?cancellationToken) =
      let respond_to (listener:HttpListener) = async {
        let! context = listener.AsyncGetContext()
        let request = context.ToOwinRequest()
        let! response = handler.AsyncInvoke(request)
        do context.Response.Reply(response) }
      let server = async { 
        use listener = new HttpListener()
        listener.Prefixes.Add(url)
        listener.Start()
        while true do 
          Async.Start(respond_to listener, ?cancellationToken = cancellationToken) }
      Async.Start(server, ?cancellationToken = cancellationToken)