[<AutoOpen>]
module Shared

  open FSharp.Data
  open System.Xml.Linq

  ///exception when not able to find the "Content" directory
  exception CannotFindContent of string

  ///type alias for System.IO.SearchOption.TopDirectoryOnly
  [<Literal>]
  let Siblings = System.IO.SearchOption.TopDirectoryOnly

  /// ensures there is no trailing slash on a string
  let inline (!-/) (x: string) =
    if x.EndsWith "/" then x.[0 .. (-2 + String.length x)] else x

  ///shortcut for System.IO.Path.Combine
  let inline (@@) (x: string) (y: string) =
    System.IO.Path.Combine (x, y) |> System.IO.Path.GetFullPath

  /// recursively finds the relative path to a needle from a starting dir
  let rec findRelativeDir needle (dir: System.IO.DirectoryInfo) =
    match dir with
    | null ->
      failwith <| sprintf "found no parent directory with direct child named `contents`"
    | d ->
      match d.GetDirectories (needle, Siblings) with
      | [||] -> System.IO.Directory.GetParent d.FullName |> findRelativeDir needle
      | _ -> System.IO.Path.Combine (d.FullName, needle)

  /// finds the relative path to a needle from a starting directory
  let findRelativeDirFromPath needle dir = findRelativeDir needle (System.IO.DirectoryInfo dir)

  /// finds the relative path to the "Content" directory
  let findContent = findRelativeDirFromPath "content"

  /// for a given file, replaces all instances of a token with the token replacement value
  /// represented by the tokens parameter (token: string * replacement: string)
  let replaceTokens (file: string) (tokens:(string * string) list) =
    let rec rt (acc: string) = function
    | [] -> acc
    | (tkn: string, rep) :: rest -> rt (acc.Replace (tkn, rep)) rest
    use stream = new System.IO.StreamReader(file)
    let contents = stream.ReadToEnd()
    stream.Close()
    let replaced = rt contents tokens
    use stream = new System.IO.StreamWriter(file)
    stream.Write replaced
    stream.Flush()
    stream.Close()

  (* first open the target file and read all lines into a hashset or list, close file
    then open the example file and do the same, or a sequence even, close file
    re-open the first file in append only mode *)

  /// merges the source example gitignore file with the target, basically goes
  /// line-by-line example file and if it exists in the target file then it
  /// ignores it otherwise it queues it up to be appended to the target gitignore file
  let mergeGitIgnore (example: string) (target: string) =
    let toHashSet (f: string) =
      use sr = new System.IO.StreamReader(f)
      let rec setrec acc =
        match sr.EndOfStream with
        | true -> acc
        | false ->
          let line = sr.ReadLine ()
          match Set.contains line acc with
          | false -> Set.add line acc
          | true  -> acc
          |> setrec
      setrec Set.empty

    let targetSet = toHashSet target
    let exampleSet = toHashSet example
    let missingSet = Set.difference exampleSet targetSet

    if Set.exists (fun _ -> true) missingSet then
      use fw = System.IO.File.AppendText(target)
      fw.WriteLine System.Environment.NewLine
      missingSet |> Seq.iter (fun line ->
        printfn "... adding .gitignore line `%s`" line
        fw.WriteLine line
      )

  let inline xns n = XNamespace.op_Implicit n
  let inline xname n = XName.op_Implicit n

  let private parseXml (file: string) =
    let xml = XDocument.Load(file)
    xml.Root

  /// merges the source example Directory.build.props file with the target basically goes
  /// creates a Set of properties and items and checks for those property or item elements in the
  /// target, it shouldn't matter where or what the <ItemGroup> or <PropertyGroup> is
  let mergeDirectoryBuildProps (example: string) (target: string) =
    let toMap parent (f:string) =
      let xml = parseXml f
      xml.Elements(xname parent)
      |> Seq.collect (fun x -> x.Elements())
      |> Seq.map (fun x -> x.Name.LocalName, x)
      |> Map.ofSeq

    let toPropertyMap (f: string) = toMap "PropertyGroup" f
    let toItemMap (f: string) = toMap "ItemGroup" f
    let targetXml = parseXml target

    let exampleProperties = toPropertyMap example
    let exampleItems = toItemMap example
    let targetProperties = toPropertyMap target
    let targetItems = toItemMap target

    let (_, missingProperties) =
      Map.partition (fun key _ -> targetProperties.ContainsKey key) exampleProperties
    let (_, missingItems) =
      Map.partition (fun key _ -> targetItems.ContainsKey key) exampleItems

    let addChild parent (ele:XElement) =
      if targetXml.Element(xname parent) = null then
        printfn "... no <%s> element found, adding..." parent
        let parent = new XElement(xname parent)
        targetXml.Add(parent)
      printfn "... adding Item: %O" ele
      targetXml.Element(xname parent).Add(ele)

    if Map.exists (fun _ __ -> true) missingItems || Map.exists (fun _ __ -> true) missingProperties then
      missingProperties |> Map.iter (fun _ ele -> addChild "PropertyGroup" ele)
      missingItems |> Map.iter (fun _ ele -> addChild "ItemGroup" ele)
      targetXml.Save (target)

  let DiffMap = Map.ofList <| [
    (".gitignore", mergeGitIgnore)
    ("Directory.Build.props", mergeDirectoryBuildProps)
  ]
