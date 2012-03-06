(* # Frank

## License

Author: Ryan Riley <ryan.riley@panesofglass.org>
Copyright (c) 2011, Ryan Riley.

Licensed under the Apache License, Version 2.0.
See LICENSE.txt for details.
*)
[<AutoOpen>]
module Frank.Core

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Formatting
open System.Net.Http.Headers
open System.Text
open FSharpx

#if DEBUG
open System.Json
open ImpromptuInterface.FSharp
open NUnit.Framework
open Swensen.Unquote.Assertions
#endif

// ## Define the web application interface

(*
One may define a web application interface using a large variety of signatures. Indeed, if you search the web, you're likely to find a large number of approaches. When starting with `Frank`, I wanted to try to find a way to define an HTTP application using pure functions and function composition. The closest I found was the following:

    type HttpApplication = HttpRequestMessage -> Async<HttpResponseMessage>
    
    let orElse left right = fun request -> Option.orElse (left request) (right request)
    let inline (<|>) left right = orElse left right 

These signatures represent both the application signature and a means for merging multiple applications together into a single application. This allowed for a nice symmetry and elegance in that everything you composed would always have the same signature. Additional functions allow you to map applications to specific methods or uri patterns.

Alas, this approach works only so well. HTTP is a rich communication specification. The simplicity and elegance of a purely functional approach quickly loses the ability to communicate back options to the client. For instance, given the above, how do you return a meaningful `405 Method Not Allowed` response? The HTTP specification requires that you list the allowed methods, but if you merge all the logic for selecting an application into the functions, there is no easy way to recall all the allowed methods, short of trying them all. You could require that the developer add the list of used methods, but that, too, misses the point that the application should be collecting this and helping the developer by taking care of all of the nuts and bolts items.

The next approach I tried involved using a tuple of a list of allowed HTTP methods and the application handler, which used the merged function approach described above for actually executing the application. However, once again, there are limitations. This structure accurately represents a resource, but it does not allow for multiple resources to coexist side-by-side. Another tuple of uri pattern matching expressions could wrap a list of these method * handler tuples, but at this point I realized I would be better served by using real types and thus arrived at the signatures below.

You'll see the signatures above are still mostly present, though they have been changed to better fit the signatures below.
*)

// `HttpApplication` defines the contract for processing any request.
// An application takes an `HttpRequestMessage` and returns an `HttpRequestHandler` asynchronously.
type HttpApplication = HttpRequestMessage -> Async<HttpResponseMessage>

// ## HTTP Response Header Combinators

// `HttpResponseMessage`s are mutable and not very compositional, so here we define a monad.
// If F# allows mutation, why do we need the monad?
// First of all, it allows for the explicit declaration of side effects.
// Second, we can encapsulate state in such a way as to hide header collection access and let you dictate instead the functions to build the response.
// Third, the computation expressions make a nice, convenient DSL.
type HttpResponder<'a> = HttpResponseMessage -> 'a

// Inline combinators for building responses. 
let inline returnM x : HttpResponder<_> = fun _ -> x
let inline (>>=) m k : HttpResponder<_> = fun r -> (k (m r)) r
// Sequential application
let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> returnM (f' m')
let inline map f m = m >>= fun x -> returnM (f x)
let inline lift2 f x y = returnM f <*> x <*> y
// Sequence actions, discarding the value of the first argument.
let inline ( *>) x y = lift2 (fun _ z -> z) x y
// Sequence actions, discarding the value of the second argument.
let inline ( <*) x y = lift2 (fun z _ -> z) x y
// Sequentially compose two reader actions, discarding any value produced by the first
let inline (>>.) m f = m >>= (fun _ -> f)
// Left-to-right Kleisli composition
let inline (>=>) f g = fun x -> f x >>= g
// Right-to-left Kleisli composition
let inline (<=<) x = flip (>=>) x
let inline fold f s = Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)
let inline respondM statusCode builder = 
  let response = new HttpResponseMessage(statusCode)
  builder response
  response

// Computation expression for building responses.
type HttpResponseBuilder(statusCode) =
  member x.Return(a) = returnM a
  member x.ReturnFrom(a) : HttpResponder<_> = a
  member x.Bind(m, k) = m >>= k
  member x.Zero() = returnM ()
  member x.Combine(r1, r2) = r1 >>= fun () -> r2
  member x.TryWith(m, h) : HttpResponder<_> =
    fun env -> try m env
               with e -> (h e) env
  member x.TryFinally(m, compensation) : HttpResponder<_> =
    fun env -> try m env
               finally compensation()
  member x.Using(res:#IDisposable, body) =
    x.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
  member x.Delay(f) = returnM () >>= f
  member x.While(guard, m) =
    if not(guard()) then returnM ()
    else m >>= fun () -> x.While(guard, m)
  member x.For(sequence:seq<_>, body) =
    x.Using(sequence.GetEnumerator(), (fun enum -> x.While(enum.MoveNext, x.Delay(fun () -> body enum.Current))))
  member x.Run(m) = respondM statusCode m

let respond statusCode = new HttpResponseBuilder(statusCode)

// ### General Headers
let Date x : HttpResponder<_> =
  fun response -> response.Headers.Date <- Nullable.create x

let Connection x : HttpResponder<_> =
  fun response -> response.Headers.Connection.ParseAdd x

let Trailer x : HttpResponder<_> =
  fun response -> response.Headers.Trailer.ParseAdd x

let ``Transfer-Encoding`` x : HttpResponder<_> =
  fun response -> response.Headers.TransferEncoding.ParseAdd x

let Upgrade x : HttpResponder<_> =
  fun response -> response.Headers.Upgrade.ParseAdd x

let Via x : HttpResponder<_> =
  fun response -> response.Headers.Via.ParseAdd x

let ``Cache-Control`` x : HttpResponder<_> =
  fun response -> response.Headers.CacheControl <- CacheControlHeaderValue.Parse x

let Pragma x : HttpResponder<_> =
  fun response -> response.Headers.Pragma.ParseAdd x

// ### Response Headers
let Age x : HttpResponder<_> =
  fun response -> response.Headers.Age <- Nullable.create x

let ``Retry-After`` x : HttpResponder<_> =
  fun response -> response.Headers.RetryAfter <- RetryConditionHeaderValue.Parse x

let Server x : HttpResponder<_> =
  fun response -> response.Headers.Server.ParseAdd x

let Warning x : HttpResponder<_> =
  fun response -> response.Headers.Warning.ParseAdd x

let ``Accept-Ranges`` x : HttpResponder<_> =
  fun response -> response.Headers.AcceptRanges.ParseAdd x

let Vary x : HttpResponder<_> =
  fun response -> response.Headers.Vary.ParseAdd x

let ``Proxy-Authenticate`` x : HttpResponder<_> =
  fun response -> response.Headers.ProxyAuthenticate.ParseAdd x

let ``WWW-Authenticate`` x : HttpResponder<_> =
  fun response -> response.Headers.WwwAuthenticate.ParseAdd x

// ### Entity Headers
let Allow x : HttpResponder<_> =
  fun response -> Seq.iter response.Content.Headers.Allow.Add x

let Location x : HttpResponder<_> =
  fun response -> response.Headers.Location <- x

let ``Content-Disposition`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentDisposition <- ContentDispositionHeaderValue x

let ``Content-Encoding`` x : HttpResponder<_> =
  fun response -> Seq.iter response.Content.Headers.ContentEncoding.Add x

let ``Content-Language`` x : HttpResponder<_> =
  fun response -> Seq.iter response.Content.Headers.ContentLanguage.Add x 

let ``Content-Length`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentLength <- Nullable.create x

let ``Content-Location`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentLocation <- x

let ``Content-MD5`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentMD5 <- x

let ``Content-Range`` from _to length : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentRange <- ContentRangeHeaderValue(from, _to, length)

let ``Content-Type`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.ContentType <- MediaTypeHeaderValue x

let ETag tag isWeak : HttpResponder<_> =
  fun response -> response.Headers.ETag <- EntityTagHeaderValue(tag, isWeak)

let Expires x : HttpResponder<_> =
  fun response -> response.Content.Headers.Expires <- Nullable.create x

let ``Last Modified`` x : HttpResponder<_> =
  fun response -> response.Content.Headers.LastModified <- Nullable.create x

// ### Content
let Body content : HttpResponder<_> =
  fun response -> response.Content <- content

#if DEBUG
open System.Json
open ImpromptuInterface.FSharp
open NUnit.Framework
open Swensen.Unquote.Assertions

[<Test>]
let ``test respond without body``() =
  let response = new HttpResponseMessage(HttpStatusCode.OK)
  test <@ response.StatusCode = HttpStatusCode.OK @>
  test <@ response.Content = HttpContent.Empty @>

[<Test>]
let ``test respond with StringContent``() =
  let body = "Howdy"
  let response = respond HttpStatusCode.OK { do! Body (new StringContent(body)) }
  test <@ response.StatusCode = HttpStatusCode.OK @>
  test <@ response.Content.ReadAsStringAsync().Result = body @>

[<Test>]
let ``test respond with negotiated body``() =
  let body = "Howdy"
  let response = respond HttpStatusCode.OK { do! Body (new SimpleObjectContent<_>(body, "text/plain", new XmlMediaTypeFormatter())) }
  test <@ response.StatusCode = HttpStatusCode.OK @>
  test <@ response.Content.ReadAsStringAsync().Result = "<?xml version=\"1.0\" encoding=\"utf-8\"?><string>Howdy</string>" @>
#endif

// ### Allow Header Helpers

// A few responses should return allowed methods (`OPTIONS` and `405 Method Not Allowed`).
// `respondWithAllowHeader` allows both methods to share common functionality.
let internal respondWithAllowHeader statusCode allowedMethods body =
  fun _ -> async {
    return respondM statusCode <| Allow allowedMethods *> Body body }

// `OPTIONS` responses should return the allowed methods, and this helper facilitates method calls.
let options allowedMethods =
  respondWithAllowHeader HttpStatusCode.OK allowedMethods HttpContent.Empty

#if DEBUG
[<Test>]
let ``test options``() =
  let response = options ["GET";"POST"] (new HttpRequestMessage()) |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.OK @>
  test <@ response.Content.Headers.Allow.Contains("GET") @>
  test <@ response.Content.Headers.Allow.Contains("POST") @>
  test <@ not <| response.Content.Headers.Allow.Contains("PUT") @>
  test <@ not <| response.Content.Headers.Allow.Contains("DELETE") @>
#endif

// In some instances, you need to respond with a `405 Message Not Allowed` response.
// The HTTP spec requires that this message include an `Allow` header with the allowed
// HTTP methods.
let ``405 Method Not Allowed`` allowedMethods =
  respondWithAllowHeader HttpStatusCode.MethodNotAllowed allowedMethods
  <| new StringContent("405 Method Not Allowed")

#if DEBUG
[<Test>]
let ``test 405 Method Not Allowed``() =
  let response = ``405 Method Not Allowed`` ["GET";"POST"] (new HttpRequestMessage()) |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.MethodNotAllowed @>
  test <@ response.Content.Headers.Allow.Contains("GET") @>
  test <@ response.Content.Headers.Allow.Contains("POST") @>
  test <@ not <| response.Content.Headers.Allow.Contains("PUT") @>
  test <@ not <| response.Content.Headers.Allow.Contains("DELETE") @>
#endif

// ## Content Negotiation Helpers

let ``406 Not Acceptable`` =
  fun _ -> async {
    return respondM HttpStatusCode.NotAcceptable <| Body (new StringContent("406 Not Acceptable")) }

#if DEBUG
[<Test>]
let ``test 406 Not Acceptable``() =
  let response = ``406 Not Acceptable`` (new HttpRequestMessage()) |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.NotAcceptable @>
#endif

let findFormatterFor mediaType =
  Seq.find (fun (formatter: MediaTypeFormatter) ->
    formatter.SupportedMediaTypes
    |> Seq.map (fun value -> value.MediaType)
    |> Seq.exists ((=) mediaType))

// `formatWith` allows you to specify a specific `formatter` with which to render a representation
// of your content body.
// 
// The `Web API` tries to do this for you at this time, so this function is likely to be clobbered,
// or rather, wrapped again in another representation. Hopefully, this will get fixed in a future release.
// 
// Further note that the current solution requires creation of `ObjectContent<_>`, which is certainly
// not optimal. Hopefully this, too, will be resolved in a future release.
let formatWith mediaType formatter body =
  new SimpleObjectContent<_>(body, mediaType, formatter) :> HttpContent

#if DEBUG
[<Serializable>]
type TestType() =
  let mutable firstName = ""
  let mutable lastName = ""
  member x.FirstName
    with get() = firstName
    and set(v) = firstName <- v
  member x.LastName
    with get() = lastName
    and set(v) = lastName <- v
  override x.ToString() = firstName + " " + lastName

[<Test>]
let ``test formatWith properly format as application/json``() =
  let formatter = new System.Net.Http.Formatting.JsonMediaTypeFormatter()
  let body = TestType(FirstName = "Ryan", LastName = "Riley")
  let content = body |> formatWith "application/json" formatter
  test <@ content.Headers.ContentType.MediaType = "application/json" @>
  let result = content.AsyncReadAsString() |> Async.RunSynchronously
  test <@ result = "{\"firstName\":\"Ryan\",\"lastName\":\"Riley\"}" @>

[<Test>]
let ``test formatWith properly format as application/xml and read as TestType``() =
  let formatter = new System.Net.Http.Formatting.XmlMediaTypeFormatter()
  let body = TestType(FirstName = "Ryan", LastName = "Riley")
  let content = body |> formatWith "application/xml" formatter
  test <@ content.Headers.ContentType.MediaType = "application/xml" @>
  let result = content.AsyncReadAs<TestType>([| formatter |]) |> Async.RunSynchronously
  test <@ result = body @>

[<Test;Ignore>]
let ``test formatWith properly format as application/x-www-form-urlencoded and read as JsonValue``() =
  let formatter = new System.Net.Http.Formatting.JsonMediaTypeFormatter()
  let body = TestType(FirstName = "Ryan", LastName = "Riley")
  let content = body |> formatWith "application/x-www-form-urlencoded" formatter
  test <@ content.Headers.ContentType.MediaType = "application/x-www-form-urlencoded" @>
  let interim = content.AsyncReadAs<JsonValue>([| formatter |]) |> Async.RunSynchronously
  let result = interim.AsDynamic()
  test <@ result?firstName = body.FirstName @>
  test <@ result?lastName = body.LastName @>
#endif

let internal accepted (request: HttpRequestMessage) = request.Headers.Accept.ToString()

let negotiateMediaType formatters =
  let servedMedia =
    formatters
    |> Seq.collect (fun (formatter: MediaTypeFormatter) -> formatter.SupportedMediaTypes)
    |> Seq.map (fun value -> value.MediaType)
  accepted >> Http.Conneg.bestMediaType servedMedia >> Option.map fst

// When you want to negotiate the format of the response based on the available representations and
// the `request`'s `Accept` headers, you can `tryNegotiateMediaType`. This takes a set of available
// `formatters` and attempts to match the best with the provided `Accept` header values using
// functions from `FSharpx.Http`.
let runConneg formatters (f: HttpRequestMessage -> Async<_>) =
  let bestOf = negotiateMediaType formatters
  fun request ->
    match bestOf request with
    | Some mediaType ->
        let formatter = findFormatterFor mediaType formatters
        async {
          let! responseBody = f request
          let formattedBody = responseBody |> formatWith mediaType formatter
          return respondM HttpStatusCode.OK <| ``Content-Type`` mediaType *> ``Vary`` "Accept" *> Body formattedBody }
    | _ -> ``406 Not Acceptable`` request

// ## HTTP Resources

// HTTP resources expose an resource handler function at a given uri.
// In the common MVC-style frameworks, this would roughly correspond
// to a `Controller`. Resources should represent a single entity type,
// and it is important to note that a `Foo` is not the same entity
// type as a `Foo list`, which is where most MVC approaches go wrong. 
// The optional `uriMatcher` parameter allows the consumer to provide
// a more advanced uri matching algorithm, such as one using regular
// expressions.
type HttpResource(uriTemplate, methods, handler, ?uriMatcher) =
  let mutable uriTemplate = uriTemplate
  let uriMatcher =
    defaultArg uriMatcher
    <| fun template (request: HttpRequestMessage) ->
        template = request.RequestUri.AbsolutePath
  with
  member x.Methods = methods
  member x.IsIdentifiedBy(uri) = uriMatcher uriTemplate uri

  // With the ``405 Method Not Allowed`` function, resources can correctly respond to messages.
  // Therefore, we'll extend the `HttpResource` with an `Invoke` method.
  // Without the `Invoke` method, the `HttpResource` is left without any true
  // representation of an `HttpApplication`.
  // 
  // Also note that the methods will always be looked up using the latest set. This could
  // probably be memoized so as to save a bit of time, but it allows us to ensure that all
  // available methods are reported.
  member x.Invoke(request) =
    match handler request with
    | Some h -> h
    | _ -> ``405 Method Not Allowed`` x.Methods request

let private makeHandler(httpMethod, handler) =
  function (request: HttpRequestMessage) when request.Method.Method = httpMethod -> Some(handler request)
         | _ -> None

// Helpers to more easily map `HttpApplication` functions to methods to be composed into `HttpResource`s.
let mapResourceHandler(httpMethod, handler) = [httpMethod], makeHandler(httpMethod, handler)
let get handler = mapResourceHandler(HttpMethod.Get.Method, handler)
let post handler = mapResourceHandler(HttpMethod.Post.Method, handler)
let put handler = mapResourceHandler(HttpMethod.Put.Method, handler)
let delete handler = mapResourceHandler(HttpMethod.Delete.Method, handler)

// We can use several methods to merge multiple handlers together into a single resource.
// Our chosen mechanism here is merging functions into a larger function of the same signature.
// This allows us to create resources as follows:
// 
//     let resource = get app1 <|> post app2 <|> put app3 <|> delete app4
//
// The intent here is to build a resource, with at most one handler per HTTP method. This goes
// against a lot of the "RESTful" approaches that just merge a bunch of method handlers at
// different URI addresses.
let orElse left right =
  fst left @ fst right,
  fun request -> Option.orElse (snd left request) (snd right request)
let inline (<|>) left right = orElse left right

let route uri handler =
  HttpResource(uri, fst handler, snd handler)

let routeTemplate uriTemplate uriMatcher handler =
  HttpResource(uriTemplate, fst handler, snd handler, uriMatcher)

let routeResource uri handlers =
  route uri <| Seq.reduce orElse handlers

let routeTemplatedResource uriTemplate uriMatcher handlers =
  routeTemplate uriTemplate uriMatcher <| Seq.reduce orElse handlers

(* ## HTTP Applications *)

let ``404 Not Found`` : HttpApplication =
  fun request -> async {
    return respondM HttpStatusCode.NotFound <| Body (new StringContent("404 Not Found")) }

let findApplicationFor resources (request: HttpRequestMessage) =
  let resource = Seq.tryFind (fun (r: HttpResource) -> r.IsIdentifiedBy request) resources
  resource |> Option.map (fun r -> r.Invoke)

#if DEBUG
let stub request = async { return respondM HttpStatusCode.OK ignore }
let resource1 = route "/" (get stub <|> post stub)
let resource2 = route "/stub" <| get stub

[<Test>]
let ``test should find nothing at GET /baduri``() =
  let request = new HttpRequestMessage(HttpMethod.Get, new Uri("http://example.org/baduri"))
  let handler = findApplicationFor [resource1; resource2] request
  test <@ handler.IsNone @>

[<Test>]
let ``test should find stub at GET /``() =
  let request = new HttpRequestMessage(HttpMethod.Get, new Uri("http://example.org/"))
  let handler = findApplicationFor [resource1; resource2] request
  test <@ handler.IsSome @>

[<Test>]
let ``test should find stub at POST /``() =
  let request = new HttpRequestMessage(HttpMethod.Post, new Uri("http://example.org/")) 
  let handler = findApplicationFor [resource1; resource2] request
  test <@ handler.IsSome @>

[<Test>]
let ``test should find stub at GET /stub``() =
  let request = new HttpRequestMessage(HttpMethod.Post, new Uri("http://example.org/"))
  let handler = findApplicationFor [resource1; resource2] request
  test <@ handler.IsSome @>
#endif

let mergeWithNotFound notFoundHandler (resources: #seq<HttpResource>) : HttpApplication =
  fun request ->
    let handler = findApplicationFor resources request |> (flip defaultArg) notFoundHandler
    handler request

let merge resources = mergeWithNotFound ``404 Not Found`` resources

// TODO: Need to provide a way to adjust the UriTemplate of each resource as it is nested deeper.

#if DEBUG
[<Test>]
let ``test should return 404 Not Found as the handler``() =
  let app = merge []
  let request = new HttpRequestMessage()
  let response = app request |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.NotFound @>

[<Test>]
let ``test should return 404 Not Found as the handler when other resources are available``() =
  let app = merge [resource1; resource2]
  let request = new HttpRequestMessage(HttpMethod.Get, new Uri("http://example.org/baduri"))
  let response = app request |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.NotFound @>

[<Test>]
let ``test should return stub at GET /``() =
  let app = merge [resource1; resource2]
  let request = new HttpRequestMessage(HttpMethod.Get, new Uri("http://example.org/"))
  let response = app request |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.OK @>

[<Test>]
let ``test should return stub at POST /``() =
  let app = merge [resource1; resource2]
  let request = new HttpRequestMessage(HttpMethod.Post, new Uri("http://example.org/")) 
  let response = app request |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.OK @>

[<Test>]
let ``test should return stub at GET /stub``() =
  let app = merge [resource1; resource2]
  let request = new HttpRequestMessage(HttpMethod.Get, new Uri("http://example.org/stub"))
  let response = app request |> Async.RunSynchronously
  test <@ response.StatusCode = HttpStatusCode.OK @>
#endif
