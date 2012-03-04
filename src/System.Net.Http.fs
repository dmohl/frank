namespace System.Net.Http.Formatting

[<AutoOpen>]
module Extensions =
  type MediaTypeFormatter with
    member x.AsyncReadFromStream(type', stream, headers, context) = Async.AwaitTask <| x.ReadFromStreamAsync(type', stream, headers, context)


namespace System.Net.Http

open System
open System.Net.Http
open System.Net.Http.Formatting
open System.Net.Http.Headers
open System.Threading.Tasks

#if DEBUG
open System.Json
open ImpromptuInterface.FSharp
open NUnit.Framework
open Swensen.Unquote.Assertions
#endif

type EmptyContent() =
  inherit HttpContent()
  override x.SerializeToStreamAsync(stream, context) =
    System.Threading.Tasks.Task.Factory.StartNew(fun () -> ())
  override x.TryComputeLength(length) =
    length <- 0L
    true
  override x.Equals(other) =
    other.GetType() = typeof<EmptyContent>
  override x.GetHashCode() = hash x

type AsyncHandler =
  inherit DelegatingHandler
  val AsyncSend : HttpRequestMessage -> Async<HttpResponseMessage>
  new (f, inner) = { inherit DelegatingHandler(inner); AsyncSend = f }
  new (f) = { inherit DelegatingHandler(); AsyncSend = f }
  override x.SendAsync(request, cancellationToken) =
    Async.StartAsTask(x.AsyncSend request, cancellationToken = cancellationToken)

[<AutoOpen>]
module Extensions =
  open System.Net
  open System.Net.Http
  open System.Net.Http.Headers

  type HttpContent with
    static member Empty = new EmptyContent() :> HttpContent
    member x.AsyncReadAs<'a>() = Async.AwaitTask <| x.ReadAsAsync<'a>()
    member x.AsyncReadAs<'a>(formatters) = Async.AwaitTask <| x.ReadAsAsync<'a>(formatters)
    member x.AsyncReadAs(type') = Async.AwaitTask <| x.ReadAsAsync(type')
    member x.AsyncReadAs(type', formatters) = Async.AwaitTask <| x.ReadAsAsync(type', formatters)
    member x.AsyncReadAsByteArray() = Async.AwaitTask <| x.ReadAsByteArrayAsync()
    member x.AsyncReadAsMultipart() = Async.AwaitTask <| x.ReadAsMultipartAsync()
    member x.AsyncReadAsMultipart(streamProvider) = Async.AwaitTask <| x.ReadAsMultipartAsync(streamProvider)
    member x.AsyncReadAsMultipart(streamProvider, bufferSize) = Async.AwaitTask <| x.ReadAsMultipartAsync(streamProvider, bufferSize)
    member x.AsyncReadAsOrDefault<'a>() = Async.AwaitTask <| x.ReadAsOrDefaultAsync<'a>()
    member x.AsyncReadAsOrDefault<'a>(formatters) = Async.AwaitTask <| x.ReadAsOrDefaultAsync<'a>(formatters)
    member x.AsyncReadAsOrDefault(type') = Async.AwaitTask <| x.ReadAsOrDefaultAsync(type')
    member x.AsyncReadAsOrDefault(type', formatters) = Async.AwaitTask <| x.ReadAsOrDefaultAsync(type', formatters)
    member x.AsyncReadAsStream() = Async.AwaitTask <| x.ReadAsStreamAsync()
    member x.AsyncReadAsString() = Async.AwaitTask <| x.ReadAsStringAsync()
  
type SimpleObjectContent<'a> private (outboundInstance: 'a, inboundContent: HttpContent, mediaType, formatter: MediaTypeFormatter) as x =
  inherit HttpContent()
  do x.Headers.ContentType <- MediaTypeHeaderValue(mediaType)
  new (outboundInstance, mediaType, formatter) = new SimpleObjectContent<'a>(outboundInstance, null, mediaType, formatter)
  new (inboundContent, formatter) = new SimpleObjectContent<'a>(Unchecked.defaultof<'a>, inboundContent, null, formatter)
  member x.AsyncRead() = async {
    let! stream = x.AsyncReadAsStream()
    let! obj = formatter.AsyncReadFromStream(typeof<'a>, stream, inboundContent.Headers, FormatterContext(inboundContent.Headers.ContentType, false))
    let! result = Async.AwaitTask <| (obj :?> Task<obj>)
    return result :?> 'a }
  override x.CreateContentReadStreamAsync() =
    inboundContent.ReadAsStreamAsync()
  override x.SerializeToStreamAsync(stream, context) =
    formatter.WriteToStreamAsync(typeof<'a>, outboundInstance, stream, x.Headers, FormatterContext(x.Headers.ContentType, false), context)
  override x.TryComputeLength(length) =
    length <- -1L
    false

#if DEBUG
module SimpleObjectContentTests =

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
    let body = TestType(FirstName = "Ryan", LastName = "Riley")
    let content = new SimpleObjectContent<_>(body, "application/json", new JsonMediaTypeFormatter())
    test <@ content.Headers.ContentType.MediaType = "application/json" @>
    let result = content.AsyncReadAsString() |> Async.RunSynchronously
    test <@ result = "{\"firstName\":\"Ryan\",\"lastName\":\"Riley\"}" @>

  [<Test>]
  let ``test formatWith properly format as application/xml and read as TestType``() =
    let formatter = new System.Net.Http.Formatting.XmlMediaTypeFormatter()
    let body = TestType(FirstName = "Ryan", LastName = "Riley")
    let content = new SimpleObjectContent<_>(body, "application/xml", formatter)
    test <@ content.Headers.ContentType.MediaType = "application/xml" @>
    let result = content.AsyncReadAs<TestType>([| formatter |]) |> Async.RunSynchronously
    test <@ result = body @>

  [<Test; Ignore>]
  let ``test formatWith properly format as application/x-www-form-urlencoded and read as JsonValue``() =
    let formatter = new System.Net.Http.Formatting.JsonMediaTypeFormatter()
    let body = TestType(FirstName = "Ryan", LastName = "Riley")
    let content = new SimpleObjectContent<_>(body, "application/x-www-form-urlencoded", formatter)
    test <@ content.Headers.ContentType.MediaType = "application/x-www-form-urlencoded" @>
    let interim = content.AsyncReadAs<JsonValue>([| formatter |]) |> Async.RunSynchronously
    let result = interim.AsDynamic()
    test <@ result?firstName = body.FirstName @>
    test <@ result?lastName = body.LastName @>
#endif
