namespace Microcelium.Fake

[<AutoOpen>]
module Autos =

  type FileSearch = Option<Fake.IO.IGlobbingPattern>

  let Default = Option<string>.None
  let DefaultFiles = FileSearch.None
  let DefaultProperties = List.empty<string * string>

  type CommandArg =
    | Arg of string * string
    | Switch of string



[<RequireQualifiedAccess>]
module Util =

  open Fake.Core

  let rec last = function
    | [hd] -> hd
    | _ :: tl -> last tl
    | _ -> failwith "Empty list."

  let rec environVarOrNone (keys : string list) =
    match keys with
    | [] -> None
    | x :: xs ->
      match Environment.environVarOrNone x with
      | Some v -> Some v
      | None -> environVarOrNone xs

  let environVar (keys : string list) =
    match (environVarOrNone keys) with
    | None   -> failwith "no Environmental variable values found"
    | Some x -> x

  let environVarOrDefault (keys : string list) (dflt : string) =
    match (environVarOrNone keys) with
    | None   -> dflt
    | Some x -> x

  let toCommandLine cmd (chr: string option) (args: Autos.CommandArg list) =
    let chr' =
      match chr with
      | None -> "/"
      | Some d -> d

    let escape str =
      match System.Text.RegularExpressions.Regex.IsMatch(str, "\\s") with
      | true -> sprintf "\"%s\"" str
      | false -> str

    args
      |> List.collect (
          fun arg ->
            match arg with
            | Switch key -> [sprintf "%s%s" chr' key]
            | Arg (key, value) -> [sprintf "%s%s:%s" chr' key (escape value)]
        )
      |> (fun c args -> c :: args) cmd
      |> List.fold (fun acc x -> acc + (x + " ")) ""



[<RequireQualifiedAccess>]
module Environment =

  open Fake.Core
  open Fake.IO.FileSystemOperators

  [<Literal>]
  let DefaultSeleniumFilter = "(TestCategory=selenium)|(TestCategory=Selenium)"

  /// the build channel, this corresponds to branch as well: e.g.: develop, release, production
  let channel = Util.environVarOrDefault ["build.channel"; "build_channel"] "Production"

  /// is the current channel the production channel?
  let isProduction = channel = "Production"

  /// should artifacts be published
  let runPublish = (Util.environVarOrDefault ["PUBLISH"] "1") = "1"

  /// should artifacts ne cleaned
  let runCleanup = (Util.environVarOrDefault ["CLEANUP"] "1") = "1"

  /// do we have a path for DOTCOVER (.NET Code Coverage tool)
  let dotcover = Util.environVarOrNone ["DOTCOVER"]

  /// the file path to the DotCover executable
  let dotCoverPath = if dotcover.IsSome then dotcover.Value @@ "dotCover.exe" else ""

  /// returns true if we're to run Code Coverage during testing
  let runCoverage = dotcover.IsSome

  /// the path to a local microcelium development/scratch repo
  let microceliumDevNugetPackages = Util.environVarOrNone ["MicroceliumNugetPackages"; "NUGET_PACKAGES_MICROCELIUM"]

  /// the path to the standard .net nuget packages install directory
  let nugetOrgNugetPackages = Util.environVarOrNone ["NuGetCachePath"; "NUGET_PACKAGES"]

  /// the current client
  let clientName = Util.environVarOrNone ["clientName"]

  /// run browser in headless mode
  let runHeadless = System.Convert.ToBoolean (string <| Util.environVarOrDefault ["headless"] "true")

  /// url to hit when testing
  let remoteUrl = Util.environVarOrNone ["remoteUrl"]

  /// an isolated testNameFilter
  let seleniumFilter = Util.environVarOrDefault ["testcasefilter"] DefaultSeleniumFilter

  /// add the teamcity adapter?
  let addTeamCityAdapter = Util.environVarOrDefault ["TeamCityAdapters"] "0" = "1" || BuildServer.buildServer = TeamCity


[<RequireQualifiedAccess>]
module Version =

  open Fake.Core
  open Fake.IO

  type Entry = { prefix: string; suffix: string; raw: string }

  let internal prefix = sprintf "%s.%s"

  let internal increment (v : string) =
    v.Split '.'
    |> (fun x ->
          match x with
            | [|maj;min;pat|] -> sprintf "%d.%d.%d" (int maj) (int min) (int pat + 1 )
            | [|maj;min|]     -> sprintf "%d.%d" (int maj) (int min + 1)
            | _               -> failwithf "Cannot understand version: '%s'" v)

  /// creates a version from either "major.minor" or "major.minor.patch" form
  let from v =
    match BuildServer.buildServer with
    | TeamCity  -> { prefix = prefix v BuildServer.buildVersion; suffix = ""; raw = v }
    | _         -> { prefix = prefix (increment v) "0"; suffix = "developer"; raw = v }

  /// reads version from the first line of the provided file
  let fromFile file = from <| File.readLine file

  /// creates a version from a file in the root of the repository named "Version.ini"
  let fromVersionIni = fun () -> fromFile "Version.ini"

  /// returns a tuple form of the version prefix and suffix
  let inline parts (version:Entry) =
    version.prefix, version.suffix

  /// returns a string of the full version
  let inline toString (version:Entry) =
    match version.suffix with
    | "" -> version.prefix
    | _  -> sprintf "%s-%s" version.prefix version.suffix

  let inline toStringFromParts (pre: string, post: string) =
    match post with
    | "" -> pre
    | _  -> sprintf "%s-%s" pre post



[<RequireQualifiedAccess>]
module Build =

  open Fake.Core
  open Fake.DotNet
  open Fake.IO
  open Fake.IO.FileSystemOperators
  open Fake.IO.Globbing.Operators

  (* Constants *)

  [<Literal>]
  let DefaultFilter = "(TestCategory!=integration)&(TestCategory!=Integration)"

  (* Building *)

  /// default list of MSBuild Codes we do not want to receive warnings for
  let msbNowarn = Some ["CS1591"; "CS0108"; "CS0618"; "CS0672"; "NU1701"]

  /// gets a list of properties that is passable to MSBuild, also configuring version properties and appends an existings list
  let inline msbPropertiesAppend version (exist: (string * string) list) =
    exist @ [("VersionPrefix", fst version); ("VersionSuffix", snd version)] |> List.distinct

  ///gets a list of properties that is passable to MSBuild, also configuring version properties
  let inline msbProperties version = msbPropertiesAppend version DefaultProperties

  let ensureAdapters srcDir (testGlob: FileSearch) =
    if Environment.addTeamCityAdapter then
      let testGlob' =
        match testGlob with
        | None -> !! (srcDir @@ "**/*.Tests.csproj")
        | Some x -> x

      testGlob'
        |> Seq.map (Path.getDirectory >> sprintf "%s package TeamCity.VSTest.TestAdapter")
        |> Seq.iter (ignore << DotNet.exec (fun _ -> DotNet.Options.Create()) "add")

  let ensureSeleniumAdapters srcDir =
    Some !!(srcDir @@ "**/Microcelium.*Selenium*.csproj") |> ensureAdapters srcDir

  let ensureUnitAdapters srcDir =
    Some <| !!(srcDir @@ "**/*.Tests.csproj") --(srcDir @@ "Microcelium.*Selenium*.csproj") |> ensureAdapters srcDir

  ///takes the contents of a directory and zips them up with a version moniker
  ///when cleanup is true will delete the contents of sourceDir
  let packageDeployable publishDir projectName version =
    let sourceDir = publishDir @@ projectName
    let zipName = sprintf @"%s.%s.zip" projectName <| Version.toStringFromParts version
    Trace.logfn "Zip Source: `%s`" sourceDir
    Trace.logfn "Zip name: `%s`" zipName
    !! (sourceDir @@ "**/*.*")
    |> Fake.IO.Zip.createZip sourceDir (publishDir @@ zipName) "" 0 false

    Shell.deleteDir sourceDir
    zipName

  ///calls dotnet pack to create a NuGet package
  let packageNuget slnDir projectName version binDir =
    Trace.logfn "Packaging project for NuGet (dotnet pack): %s" projectName
    DotNet.pack(fun p ->
     { p with
         Configuration = DotNet.BuildConfiguration.Debug
         OutputPath = Some binDir
         NoBuild = true
         MSBuildParams =
           { p.MSBuildParams with
               NodeReuse = false
               NoWarn = msbNowarn
               BinaryLoggers = Some []
               FileLoggers = Some []
               DistributedLoggers = Some []
               Loggers = Some []
               Properties = msbProperties version
               DisableInternalBinLog = true
           }
     }) (slnDir @@ projectName )

  /// calls dotnet publish to publish the project to the local file system
  let packageProject slnDir projectName version binDir =
    Trace.logfn "Packaging project for publish (dotnet publish): %s" projectName
    let projectPath = slnDir @@ projectName
    let publishDir = binDir @@ projectName

    Directory.ensure publishDir
    Shell.cleanDir publishDir

    DotNet.publish (fun p ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          OutputPath = Some publishDir
          NoBuild = true
          MSBuildParams =
            { p.MSBuildParams with
                NodeReuse = false
                NoWarn = msbNowarn
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                Properties = msbProperties version
                DisableInternalBinLog = true
            }
      }) (projectPath)

    let zipname = packageDeployable binDir projectName version
    zipname

  /// this is used for legacy .NET Framework web projects and essentially publishes them to the local file system
  /// because dotnet publish is not available to these projects
  let packageWeb slnDir projectName version binDir =
    let projectPath = slnDir @@ projectName
    let publishDir = binDir @@ projectName

    Directory.ensure publishDir
    Shell.cleanDir publishDir

    MSBuild.build (fun p ->
      { p with
          ToolsVersion = Some "Current"
          Verbosity = Some(Quiet)
          Targets = ["WebPublish"]
          NodeReuse = false
          NoWarn = msbNowarn
          DisableInternalBinLog = true
          BinaryLoggers = Some []
          FileLoggers = Some []
          DistributedLoggers = Some []
          Loggers = Some []
          Properties = msbPropertiesAppend version ["WebPublishMethod", "FileSystem"
                                                    "PublishUrl", publishDir
                                                    "ExcludeGeneratedDebugSymbol", "false"]
       }) (projectPath @@ projectName + ".csproj")

    let zipname = packageDeployable binDir projectName version
    zipname

  ///runs NUnit with no Code Coverage, filter is optional and so will
  ///default to "(TestCategory!=integration)&(TestCategory!=Integration)" when none is provided
  ///this is essentially the same as `dotnet test`. returns the directory path of the test results
  let runTestsNoCoverage (slnFile: string) project outDir (filter: string option) =
    Trace.tracefn "parameter `project` (%s) intentionally unused" project
    DotNet.test (fun p ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          NoBuild= true
          NoRestore = true
          ResultsDirectory = Some outDir
          Filter = if filter.IsNone then Some DefaultFilter else filter
          MSBuildParams =
            { p.MSBuildParams with
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                DisableInternalBinLog = true
                NodeReuse = false
                Verbosity = Some Quiet
            }
      }) (slnFile)
    outDir

  /// runs NUnit with DotCover Code Coverage, filter is optional and so will
  /// default to "(TestCategory!=integration)&(TestCategory!=Integration)" when none is provided
  /// this is essentially the same as `dotcover "dotnet test"`, returns the full path
  /// to the output snapshot file
  let runTestsCoverage (slnFile: string) project outDir (filter:string option) =
    //let logDir = outDir
    let slnName = System.IO.Path.GetFileNameWithoutExtension slnFile
    let snapshotPath = outDir @@ (slnName + ".DotCover.snapshot")
    let testargs =
      [
        ["test"]
        [slnFile]
        ["--configuration"; "debug"]
        ["--no-build"]
        ["--no-restore"]
        ["--filter"; (if filter.IsNone then DefaultFilter else filter.Value)]
        ["/nodeReuse:False"]
        ["-noconsolelogger"]
      ]
      |> List.concat
      |> Args.toWindowsCommandLine

    let coverargs =
      [
        Arg ("TargetExecutable", DotNet.Options.Create().DotNetCliPath)
        Arg ("TargetArguments", testargs)
        Arg ("Filters", sprintf "+:module=%s*;-:*.Tests" project)
        Arg ("Output", snapshotPath)
        //("LogFile", sprintf "%s\\%s.DotCover.cover.log" logDir slnName, true)
      ]
      |> Util.toCommandLine "cover" Default

    Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
    Shell.Exec (Environment.dotCoverPath, coverargs) |> ignore
    snapshotPath

  ///runs DotCover merge process. Takes multiple snapshot files and merges them into one. e.g., if there are three
  ///solution spaces in a repository, this merges them all into one file by the name {project}.DotCover.snapshot
  ///and places it in the {snapshotDir}. returns the full path to the output snapshot file
  let runDotCoverMerge snapshotDir project =
    let snapshots = !! (snapshotDir @@ "*.snapshot") |> Seq.toList
    let mergedFile = snapshotDir @@ (sprintf "%s.DotCover.snapshot" project)

    match snapshots with
    | [] ->  None
    | [head] when mergedFile <> head ->
      Shell.rename mergedFile head
      Some mergedFile
    | [head] when mergedFile = head ->
      Some head
    | _ ->
      let sourcearg = (snapshots |> String.concat ";")
      let coverargs =
        [
          Arg ("Source", sourcearg)
          Arg ("Output", mergedFile)
        ]
        |> Util.toCommandLine "merge" Default

      Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
      Shell.Exec(Environment.dotCoverPath, coverargs) |> ignore
      snapshots |> List.iter File.delete
      Some mergedFile

  /// creates a report for a snapshot file. two primary report types are XML and HTML.
  /// given the input {snapshotFile} outputs the resulting {reportName}.DotCover.{reportType}
  /// file in {snapshotDir}. returns the full path to the output report
  let runDotCoverReport snapshotDir snapshotFile reportName reportType =
    let reportFile = snapshotDir @@ sprintf "%s.DotCover.%s" reportName reportType
    let coverargs =
      [
        Arg ("Source", snapshotFile)
        Arg ("Output", reportFile)
        Arg ("ReportType", reportType)
        Arg ("HideAutoProperties", string true)
      ]
      |> Util.toCommandLine "report" Default

    Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
    Shell.Exec(Environment.dotCoverPath, coverargs) |> ignore
    reportFile

  /// extracts the integer percent result from an XML file
  let extractCoverage (xpath: string option) xmlPath =
    let xp = if xpath.IsNone then "/Root/@CoveragePercent" else xpath.Value
    let (_, value) = Xml.read_Int false xmlPath "" "" xp
    value

  let buildCode slnRoot version (testGlob: FileSearch) =
    ensureUnitAdapters slnRoot
    ensureSeleniumAdapters slnRoot
    DotNet.build (fun p ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          MSBuildParams =
            { p.MSBuildParams with
                NodeReuse = false
                NoWarn = msbNowarn
                Properties = msbProperties version
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                DisableInternalBinLog = true
            }
      }) (slnRoot)


  let buildCodeProperties slnRoot version (testGlob: FileSearch) (msb: (string * string) list) =
    ensureUnitAdapters slnRoot
    ensureSeleniumAdapters slnRoot
    DotNet.build (fun p ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          MSBuildParams =
            { p.MSBuildParams with
                NodeReuse = false
                NoWarn = msbNowarn
                Properties = msbPropertiesAppend version msb
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                DisableInternalBinLog = true
            }
      }) (slnRoot)



[<RequireQualifiedAccess>]
module Selenium =

  open Fake.Core
  open Fake.DotNet
  open Fake.IO.FileSystemOperators
  open Fake.IO.Globbing.Operators
  open Amazon.S3
  open Amazon.S3.Transfer

  let uploadToS3BucketAsync filePath bucket s3key =
    Trace.tracefn "Uploading file '%s' to bucket '%s' with key '%s'" filePath bucket s3key

    use s3Client = new AmazonS3Client()
    use fileTransferUtility = new TransferUtility(s3Client)

    TransferUtilityUploadRequest(
      BucketName = bucket,
      FilePath = filePath,
      Key = s3key,
      ServerSideEncryptionMethod = ServerSideEncryptionMethod.AES256)
      |> fileTransferUtility.UploadAsync
      |> Async.AwaitTask

  let run (remoteUrl: unit -> string) binDir (dlls: FileSearch) (filter: string option) =

    let filter' =
      match filter with
      | None -> Environment.seleniumFilter
      | Some _ -> filter.Value

    let dlls' =
      match dlls with
      | None -> !! (binDir @@ "Microcelium.*Selenium*.dll") -- (binDir @@ "Microcelium.*Testing*.dll")
      | Some _ -> dlls.Value
      |> Seq.fold (fun acc x -> acc + (x + " ")) ""

    Fake.Core.Environment.setEnvironVar "selenium.baseUrl" <| remoteUrl ()
    Fake.Core.Environment.setEnvironVar "webdriver.browser.runheadless" <| string Environment.runHeadless

    let logger = if Environment.addTeamCityAdapter then "teamcity" else "console"
    let exe = DotNet.Options.Create().DotNetCliPath
    let args =
      [
        Arg ("TestCaseFilter", filter')
        Switch "Parallel"
        Arg ("TestAdapterPath", binDir)
        Arg ("logger", logger)
      ]
      |> Util.toCommandLine "" Default
      |> (fun x -> sprintf "vstest %s %s" dlls' x)

    Trace.tracefn "%s %s" exe args
    Shell.Exec (exe, args) |> ignore



[<RequireQualifiedAccess>]
module Targets =

  open Fake.Core
  open Fake.DotNet
  open Fake.IO
  open Fake.IO.FileSystemOperators
  open Fake.IO.Globbing.Operators

  [<Literal>]
  let ChromeName = "chrome"

  [<Literal>]
  let ChromeExeName = "chrome.exe"

  (* Errors *)

  exception NoEnvVarError of string
  exception NoDirectoryError of string

  let internal publishFailNoEnvVar =
    let msg = "no environmental variable set that contains the local nuget package endpoint.\n\
                expecting an environmental variable: `%s`"
    sprintf (Printf.StringFormat<string->string>(msg))
  let internal publishFailNoDir =
    sprintf "cannot find local nuget package endpoint directory: `%s`"

  (* Targets *)

  /// creates a Fake Target that cleans all bin and obj directories recursively from rootDir
  /// and also clears the contents of binDir
  let clean rootDir binDir =
    fun(_: TargetParameter) ->
      if BuildServer.buildServer = TeamCity then
        Process.killAllByName ChromeName
        Process.killAllByName ChromeExeName

      Directory.ensure binDir
      !! (rootDir @@ "**/bin")
      ++ (rootDir @@ "**/obj")
      -- (rootDir @@ "**/node_modules/**")
      -- (rootDir @@ "**/node_modules")
      ++ binDir
      |> Shell.cleanDirs

  /// creates a Fake Target that primarily prints version info to the console, but also
  /// sets the version number in TeamCity
  let version (v: Version.Entry) =
    fun (_:TargetParameter) ->
      Trace.logfn "versionMajorMinor: %s" v.raw
      Trace.logfn "versionPrefix:     %s" v.prefix
      Trace.logfn "versionSuffix:     %s" v.suffix
      Trace.logfn "version:           %s" <| Version.toString v

      if Fake.Core.BuildServer.buildServer = TeamCity then
        Trace.setBuildNumber <| Version.toString v

  ///creates a Fake Target that builds the solution contained at slnDir
  let build slnDir version testGlob =
    fun (_: TargetParameter) ->
      Build.buildCode slnDir version testGlob

  let buildCodeProperties slnDir version testGlob msb =
    fun (_: TargetParameter) ->
      Build.buildCodeProperties slnDir version testGlob msb

  /// creates a Fake Target that takes a sequence of solutionFile * optional filter to run
  /// unit tests for and ultimately merging the results into one coverage result when
  /// code coverage is enabled
  let test (slnFiles: seq<string * string option>) project outDir =
    fun (_: TargetParameter) ->
      match Environment.runCoverage with
      | true ->
        slnFiles |> Seq.iter (fun (file, filter) -> Build.runTestsCoverage file project outDir filter |> ignore)
        Build.runDotCoverMerge outDir project
          |> fun snapshot ->
                match snapshot with
                | Some file ->
                    Build.runDotCoverReport outDir file project "HTML" |> ignore
                    Build.runDotCoverReport outDir file project "XML"
                      |> Build.extractCoverage Default
                      |> sprintf "Coverage: %i%%"
                      |> Trace.tracefn "##teamcity[buildStatus text='{build.status.text}, %s']"

                    Trace.tracefn "##teamcity[importData type='dotNetCoverage' tool='dotcover' path='%s']" file
                | None -> ()
      | false ->
        slnFiles |> Seq.iter (fun (file, filter) -> Build.runTestsNoCoverage file project outDir filter |> ignore )

  /// publishes all contents of binDir
  let publish binDir =
    fun (_: TargetParameter) ->
      Trace.publish ImportData.BuildArtifact binDir

  ///creates a Fake Target that publishes packages to a local nuget repository
  let publishLocal binDir version =
    fun (_: TargetParameter) ->
      let microceliumNugetSource =
        try
          match Util.environVarOrNone ["MicroceliumNugetPackages"; "NUGET_PACKAGES_MICROCELIUM"] with
          | None -> raise <| NoEnvVarError("MicroceliumNugetPackages")
          | Some dir when Path.isDirectory dir -> dir
          | Some dir -> raise <| NoDirectoryError (dir)
        with
          | NoEnvVarError(x) -> failwith <| publishFailNoEnvVar x
          | NoDirectoryError(x) -> failwith <| publishFailNoDir x

      (* if the directory exists then delete the nuget package *)
      let packages = !! (binDir @@ "*.nupkg") |> Seq.cast<string>

      let args package =
        [
          [package]
          [version]
          ["--source"; microceliumNugetSource]
          ["--non-interactive"]
        ]
        |> List.concat
        |> Args.toWindowsCommandLine

      packages
        |> Seq.map (System.IO.Path.GetFileName >> ((@@) microceliumNugetSource))
        |> Seq.filter File.exists
        |> Seq.iter (fun x -> DotNet.exec (id) "nuget delete" (args x) |> ignore)

      let nugetParams (p: NuGet.NuGet.NuGetPushParams) =
        { p with Source = Some microceliumNugetSource }

      packages
        |> Seq.iter (fun x -> DotNet.nugetPush (fun p -> { p with PushParams = nugetParams p.PushParams }) x)

  ///creates a Fake Target that overwrites any locally installed packages of the same name and target version
  ///expects Environmental Variable TargetVersion which is the version you are trying to target and overwrite
  ///and expected NuGetCachePath which is the path to the repository of installed nuget packages
  ///these can both be set on the command line prior to invoking, e.g. set TargetVersion=1.1.3;
  ///however NuGetCachePath should already be set
  let packageLocal srcDir =
    fun (_: TargetParameter) ->
      let config =
        try
          match
            (Util.environVarOrNone ["TargetVersion"; "TARGET_VERSION"],
             Util.environVarOrNone ["NuGetCachePath"; "NUGET_PACKAGES" ]) with
          | (None, _) -> raise <| NoEnvVarError("TargetVersion")
          | (_, None) -> raise <| NoEnvVarError("NuGetCachePath")
          | (Some tv, Some nuget) -> tv, nuget
        with
          | NoEnvVarError(x) -> failwith <| publishFailNoEnvVar x
          | NoDirectoryError(x) -> failwith <| publishFailNoDir x

      let (targetVersion, nugetPackagesDir) = config

      !! (srcDir @@ "*/*.csproj")
      -- (srcDir @@ "*/*.Tests.csproj")
      |> Seq.map (
          System.IO.Path.GetFileNameWithoutExtension >> (fun name ->
            let bdir = (srcDir @@ name @@ "bin/debug/*/" @@ name)
            let files =
              !! (bdir + ".dll")
              ++ (bdir + ".pdb")
              ++ (bdir + ".xml")
            (name, files)
           )
         )
      |> Seq.collect (fun (name, files) -> [for f in files -> (name, f)])
      |> Seq.iter (fun (name, file) ->
          let sf = file |> Path.normalizeFileName |> Path.getFullName
          let fi = FileInfo.ofPath file
          let fx = fi.Directory.Name
          let fn = file |> System.IO.Path.GetFileName
          let targetDir = nugetPackagesDir @@ name @@ targetVersion @@ "lib" @@ fx

          if System.IO.Directory.Exists targetDir then
            Trace.logfn "Copied `%s` to '%s'" sf targetDir
            Shell.copyFile targetDir sf
          else
            Trace.logfn "No File for -Id %s -Version %s" fn targetVersion
        )

  [<Literal>]
  let DefaultHeartbeatName = "Can_reach_dashboard"

  [<Literal>]
  let ChromeDriverName = "chromedriver"

  let internal remoteUrl =
    fun () ->
      try
        match Util.environVarOrNone ["remoteUrl"; "RemoteUrl"; "REMOTE_URL"] with
        | Some url -> url
        | None -> raise <| NoEnvVarError("RemoteUrl")
      with
        | NoEnvVarError(x) -> failwith <| publishFailNoEnvVar x

  let runHeartbeat binDir prefix shotDir (dlls: FileSearch) (heartbeatTestName: string option) =
    fun (_: TargetParameter) ->
      Directory.ensure shotDir
      let heartbeatTestName' =
        match heartbeatTestName with
        | None -> Some DefaultHeartbeatName
        | Some _' -> heartbeatTestName

      Selenium.run remoteUrl binDir dlls heartbeatTestName'

      let clientName = Environment.clientName
      if clientName.IsSome then
        !! (shotDir @@ "*.png")
          |> Seq.map (fun f -> (f, sprintf "%s/%s/Heartbeat_%s.png" clientName.Value prefix (System.DateTime.Now.ToString "yyyy-MM-dd")))
          |> Seq.iter (fun (f, key) -> Selenium.uploadToS3BucketAsync f "microcelium-website-screencaps" key |> Async.RunSynchronously)

  let runFull binDir prefix shotDir (dlls: FileSearch) (testFilter: string option) =
    fun (_: TargetParameter) ->
      Selenium.run remoteUrl binDir dlls testFilter

      let clientName = Environment.clientName
      if clientName.IsSome then
        !! (shotDir @@ "*.png")
          |> Seq.map (fun f -> (f, sprintf "%s/%s/Heartbeat_%s.png" clientName.Value prefix (System.DateTime.Now.ToString "yyyy-MM-dd")))
          |> Seq.iter (fun (f, key) -> Selenium.uploadToS3BucketAsync f "microcelium-website-screencaps" key |> Async.RunSynchronously)

  let prepareSelenium binDir shotDir =
    fun (_: TargetParameter) ->
      if BuildServer.buildServer = TeamCity then
        Process.killAllByName ChromeName
        Process.killAllByName ChromeExeName

      Process.killAllByName ChromeDriverName
      let dirs = !! binDir ++ shotDir
      dirs |> Seq.iter Directory.ensure
      dirs |> Shell.cleanDirs

  let deploySelenium target srcDir =
    fun (_: TargetParameter) ->
      Directory.ensure target
      Shell.cleanDir target
      Zip.unzip target <| srcDir ()
