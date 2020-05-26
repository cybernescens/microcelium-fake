open FSharp.CommandLine

let forceFlag =
  commandFlag {
    names ["f"; "force"]
    description "overwrites target files if they exist"
    style SingleHyphenStyle.SingleLong
  }

let seleniumFlag =
  commandFlag {
    names ["s"; "selenium"]
    description "include selenium testing bits"
    style SingleHyphenStyle.SingleLong
  }

let quietFlag =
  commandFlag {
    names ["q"; "quiet"]
    description "do not log anything to console"
    style SingleHyphenStyle.SingleLong
  }

let noDeleteFlag =
  commandFlag {
    names ["r"; "no-delete"]
    description "do not automatically delete any old build files"
    style SingleHyphenStyle.SingleLong
  }

let directoryOption =
  commandOption {
    names ["d"; "dir"; "directory"]
    description "Path to the root of the solution space"
    takesFormat "%s" (fun dir -> dir)
    suggests (fun _ -> [CommandSuggestion.Directories None])
    style SingleHyphenStyle.SingleLong
  }

let projectOption =
  commandOption {
    names ["p"; "project"]
    description "name of the target project"
    takesFormat "%s" (fun project -> project)
    style SingleHyphenStyle.SingleLong

  }

let prefixOption =
  commandOption {
    names ["pre"; "prefix"]
    description "prefix for the target project"
    takesFormat "%s" (fun prefix -> prefix)
    style SingleHyphenStyle.SingleLong
  }

let bootstrapCommand =
  command {
    name "bootstrap"
    description "bootstraps a solution space with microcelium specific build components."
    opt dir in directoryOption |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse System.Environment.CurrentDirectory
    opt project in projectOption |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse "Microcelium.Example"
    opt prefix in prefixOption |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse "Exmpl"
    opt quiet in quietFlag |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse false
    opt force in forceFlag |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse false
    opt noDelete in noDeleteFlag |> CommandOption.zeroOrExactlyOne |> CommandOption.whenMissingUse false
    opt includeSelenium in seleniumFlag |> CommandOption.whenMissingUse false

    let fakeDir = dir @@ ".fake"
    let microceliumDir = dir @@ ".microcelium"
    let targetLibDir = microceliumDir @@ "lib"
    let rootUri =  new System.Uri (microceliumDir, System.UriKind.Absolute)
    let cmdArgs = sprintf "/C dotnet tool install fake-cli --tool-path %s --version 5.*" fakeDir
    let assemblyLocation = System.Reflection.Assembly.GetExecutingAssembly().Location

    if not <| quiet then
      printfn "assembly location is `%s`" assemblyLocation

    let contentDir = assemblyLocation |> System.IO.Directory.GetParent |> string |> findContent
    let exampleDir = contentDir @@ "examples"
    let diffDir = contentDir @@ "diff"
    let libDir = contentDir @@ "lib"
    let libDirRelative = new System.Uri(targetLibDir, System.UriKind.Absolute) |> rootUri.MakeRelativeUri |> string

    if not <| quiet then
      printfn "----- Bootstrapping code building scripts -----"
      printfn "Solution space root is `%s` !!! This should be very root of the repository !!!" dir
      printfn "fake path is `%s" fakeDir
      printfn "microcelium path is `%s" microceliumDir
      printfn "lib relative path is `%s`" libDirRelative
      printfn "project name is `%s`" project
      printfn "project prefix is `%s`" prefix
      printfn "force is `%b`" force
      printfn "includeSelenium is `%b`" includeSelenium

    let tokens =
      [
        ("../lib", libDirRelative)
        ("{project-prefix}", prefix)
        ("{project}", project)
        ("{year}", string System.DateTime.Today.Year)
        ("(*#load \".fake/build.fsx/intellisense.fsx\"*)", "#load \".fake/build.fsx/intellisense.fsx\"")
        ("(* selenium", if includeSelenium then "//(* selenium" else "(* selenium")
      ]

    match System.IO.Directory.Exists dir with
    | true ->
      match System.IO.Directory.Exists fakeDir with
      | false ->
          printfn "fake not installed, installing..."
          let si = System.Diagnostics.ProcessStartInfo
                      ( WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden,
                        FileName = "cmd.exe",
                        Arguments = cmdArgs )
          use p = new System.Diagnostics.Process ( StartInfo = si )
          p.Start () |> ignore
          p.WaitForExit (60 * 1000) |> ignore
      | true -> ()

      System.IO.Directory.EnumerateFiles (libDir, "*.*")
        |> Seq.map (fun x ->
            let fi = System.IO.FileInfo(x)
            let target = System.IO.Path.Combine (targetLibDir, fi.Name)
            x, target, System.IO.File.Exists target)
        |> Seq.iter (fun (src, trg, exists) ->
            if not <| System.IO.Directory.Exists targetLibDir then
              System.IO.Directory.CreateDirectory targetLibDir |> ignore
            if exists then System.IO.File.Delete trg
            printfn "... copying `%s` to `%s`" src trg
            System.IO.File.Copy (src, trg))

      System.IO.Directory.EnumerateFiles (exampleDir, "*.*")
        |> Seq.map (fun x ->
            let fi = System.IO.FileInfo(x)
            let target = System.IO.Path.Combine (dir, fi.Name)
            x, target, System.IO.File.Exists target)
        |> Seq.filter (fun (_, __, exists) -> not exists || force)
        |> Seq.iter (fun (src, trg, exists) ->
            if exists && force then System.IO.File.Delete trg
            printfn "... copying `%s` to `%s`" src trg
            replaceTokens src tokens
            System.IO.File.Copy (src, trg))

      System.IO.Directory.EnumerateFiles (diffDir, "*.*")
        |> Seq.map (fun x ->
            let fi = System.IO.FileInfo(x)
            let temp = System.IO.Path.GetTempFileName()
            let target = System.IO.Path.Combine (dir, fi.Name)
            System.IO.File.Copy(fi.FullName, temp, true)
            temp, fi.Name, target, System.IO.File.Exists target)
        |> Seq.iter (fun (src, name, trg, exists) ->
            replaceTokens src tokens
            match exists with
            | true ->
                printfn "... applying `%s` diffs to `%s`" name trg
                (src, trg) ||> (Map.find name DiffMap)
            | false ->
                printfn "... copying `%s` to `%s`" name trg
                System.IO.File.Copy (src, trg))

      System.IO.Path.Combine (dir, "build-helpers.fsx")
        |> (fun file -> file, System.IO.File.Exists file)
        |> (fun (file, exists) ->
              match exists && not noDelete with
              | false -> ()
              | true ->
                  System.IO.File.Delete file
                  printfn "Deleted unnecessary file: `%s`" file)

      return 0
    | false ->
      return -1
  }

[<EntryPoint>]
let main argv =
  bootstrapCommand |> Command.runAsEntryPoint argv
