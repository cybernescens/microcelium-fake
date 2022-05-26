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

  let bstr (x : bool) : string =
    match x with
    | true -> "true"
    | false -> "false"

  let tostr (x: obj) : string = x.ToString()
  let tolower (x: string) : string = x.ToLower()

[<RequireQualifiedAccess>]
module Util =

  open Fake.Core

  let agg (keys: string list) =
    keys |> List.reduce (sprintf "%s, %s")

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
    | None   -> failwithf "no Environmental variable values found for %s" <| agg keys
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
  let ensureCoverlet = Util.environVarOrDefault ["EnsureCoverlet"] "0" = "1" || BuildServer.buildServer = TeamFoundation

  /// get what is considered the "default" bin directory
  let defaultBinDir = Util.environVarOrDefault ["BUILD_BINARIESDIRECTORY"] "./bin"

  /// get what is considered the "default" test results directory
  let defaultTestResultsDir = Util.environVarOrDefault ["COMMON_TESTRESULTSDIRECTORY"] "./test-results"

  /// the include pattern for coverlet
  let defaultCoverletInclude = Util.environVarOrDefault ["CoverletInclude"] "[DataDx.*]*,[Microcelium.*]*"

  /// the exclude pattern for coverlet
  let defaultCoverletExclude = Util.environVarOrDefault ["CoverletExclude"] "[*.Api]*,[*.Data]*,[*.Tests]*,[*.SeleniumTests]*,[Microcelium.Testing*]*,[DataDx.Testing*]*"

  /// publish test results when running tests. default is 0
  let publishTestResults = Util.environVarOrDefault ["PublishTestResults"] "0" = "1"

  /// gets the calculated version patch number, e.g. 1.0.PATCH_NUMBER
  let patchNumber = Util.environVarOrDefault ["BUILD_PATCHNUMBER"] "0"

  /// do not increment version during build
  let doNotIncrement = (Util.environVarOrDefault ["DO_NOT_INCREMENT"; "doNotIncrement"; "DoNotIncrement"] "0") = "1"

  /// gets any passed in VersionPrefix value or uses "1.0"
  let version = 
    let versiontmp = Util.environVarOrNone ["Version"]
    let prefix = Util.environVarOrNone ["VersionPrefix"]
    let suffix = Util.environVarOrNone ["VersionSuffix"]
    let isMatch pattern test = System.Text.RegularExpressions.Regex.IsMatch(pattern, test)

    match versiontmp with
    | Some v -> v
    | None -> match (prefix, suffix) with
              | (Some x, Some y) when isMatch "^\\d+$" y -> sprintf "%s.%s" x y
              | (Some x, Some y) when isMatch "^-.+$" y -> sprintf "%s%s" x y
              | (Some x, Some y) -> sprintf "%s-%s" x y
              | (Some x, None) -> sprintf "%s" x
              | (None, Some _) -> failwith "unable to interpret version from just VersionSuffix"
              | (None, None) -> if BuildServer.buildServer = LocalBuild && not <| doNotIncrement then "0.0" else "1.0"


[<RequireQualifiedAccess>]
module Version =

  open Fake.Core
  open Fake.IO

  type Entry = { prefix: string; suffix: string; raw: string }

  //3.0.0.0 -> 3.0.1.0-developer
  //3.0.0.1 -> 3.0.1.0-developer
  //3.0.0 -> 3.1.0-developer
  //3.0 -> 4.0-developer

  let internal increment (v : string) =
    v.Split '.'
    |> (fun x ->
          match x with
            | [|maj;min;pat;_|] -> sprintf "%d.%d.%d.%d" (int maj) (int min) (int pat + 1) 0
            | [|maj;min;_|]     -> sprintf "%d.%d.%d" (int maj) (int min + 1) 0
            | [|maj;_|]         -> sprintf "%d.%d" (int maj + 1) 0
            | _                 -> failwithf "Cannot understand version: '%s'" v)

  /// creates a version from either "major.minor" or "major.minor.patch" form
  let from (v: string) =
    match (BuildServer.buildServer, Environment.doNotIncrement, v.Split '.' |> Array.length) with
    | (LocalBuild, false, 4) -> { prefix = increment v; suffix = "developer"; raw = v }
    | (LocalBuild, false, 3) -> { prefix = increment v; suffix = "developer"; raw = v }
    | (LocalBuild, false, 2) -> { prefix = increment v; suffix = "developer"; raw = v }
    | _                      -> { prefix = v; suffix = ""; raw = v }

  /// reads version from the first line of the provided file
  let fromFile file = from <| File.readLine file

  /// creates a version from a file in the root of the repository named "Version.ini"
  /// creates a version from a file in the root of the repository named "Version.ini"
  let fromVersionIni = fun () -> fromFile "Version.ini"

  /// creates a version from variables from parent process
  let fromEnvironment = fun () -> from Environment.version
  
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

  (* Building *)

  /// default list of MSBuild Codes we do not want to receive warnings for
  let msbNowarn = Some ["CS1591"; "CS0108"; "CS0618"; "CS0672"; "NU1701"]

  /// gets a list of properties that is passable to MSBuild, also configuring version properties and appends an existings list
  let inline msbPropertiesAppend version (exist: (string * string) list) =
    exist @ [("VersionPrefix", fst version); ("VersionSuffix", snd version)] |> List.distinct

  ///gets a list of properties that is passable to MSBuild, also configuring version properties
  let inline msbProperties version = msbPropertiesAppend version DefaultProperties

  let buildCodeExt slnRoot version (props: (string * string) list) (config: (DotNet.BuildOptions -> DotNet.BuildOptions) option) =
    let defaultOpt = fun (p: DotNet.BuildOptions) ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          MSBuildParams =
            { p.MSBuildParams with
                NodeReuse = false
                NoWarn = msbNowarn
                Properties = msbPropertiesAppend version props
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                DisableInternalBinLog = true
                NoConsoleLogger = false
            }
      }

    let options =
      match config with
      | Some _ -> defaultOpt >> config.Value
      | None   -> defaultOpt

    DotNet.build options slnRoot

  let buildCode slnRoot version  =
    buildCodeExt slnRoot version [] None 
  
  ///takes the contents of a directory and zips them up with a version moniker
  ///when cleanup is true will delete the contents of sourceDir
  let packageDeployable publishDir projectName version =
    let sourceDir = publishDir @@ projectName
    let zipName = sprintf @"%s.%s.zip" projectName <| Version.toStringFromParts version
    Trace.logfn "Zip Source: `%s`" sourceDir
    Trace.logfn "Zip name: `%s`" zipName
    !! (sourceDir @@ "**/*.*")
    |> Zip.createZip sourceDir (publishDir @@ zipName) "" 0 false

    Shell.deleteDir sourceDir
    zipName

  ///calls dotnet pack to create a NuGet package
  let packageNugetExt slnDir projectName version binDir (props: (string * string) list) (config: (DotNet.PackOptions -> DotNet.PackOptions) option)=
    Trace.logfn "Packaging project for NuGet (dotnet pack): %s" projectName

    let defaultOpt = fun (p: DotNet.PackOptions) ->
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
                Properties = msbPropertiesAppend version props
                DisableInternalBinLog = true
                NoConsoleLogger = false
            }
      }

    let options = 
      match config with
      | Some _ -> defaultOpt >> config.Value
      | None   -> defaultOpt

    DotNet.pack options (slnDir @@ projectName )

  let packageNuget slnDir projectName version binDir =
      packageNugetExt slnDir projectName version binDir [] None

  /// calls dotnet publish to publish the project to the local file system
  let packageProjectExt slnDir projectName version binDir (props: (string * string) list) (config: (DotNet.PublishOptions -> DotNet.PublishOptions) option) =
    Trace.logfn "Packaging project for publish (dotnet publish): %s" projectName
    let projectPath = slnDir @@ projectName
    let publishDir = binDir @@ projectName

    Directory.ensure publishDir
    Shell.cleanDir publishDir

    let defaultOpt = fun (p: DotNet.PublishOptions) ->
      { p with
          Configuration = DotNet.BuildConfiguration.Debug
          OutputPath = Some publishDir
          NoBuild = true
          SelfContained = Some false
          MSBuildParams =
            { p.MSBuildParams with
                NodeReuse = false
                NoWarn = msbNowarn
                BinaryLoggers = Some []
                FileLoggers = Some []
                DistributedLoggers = Some []
                Loggers = Some []
                Properties = msbPropertiesAppend version props
                DisableInternalBinLog = true
                NoConsoleLogger = false
            }
      }

    let options =
      match config with
      | Some _ -> defaultOpt >> config.Value
      | None   -> defaultOpt

    DotNet.publish options projectPath

    let zipname = packageDeployable binDir projectName version
    zipname

  let packageProject slnDir projectName version binDir =
      packageProjectExt slnDir projectName version binDir [] None

  /// this is used for legacy .NET Framework web projects and essentially publishes them to the local file system
  /// because dotnet publish is not available to these projects
  let packageWebExt slnDir projectName version binDir (props: (string * string) list) =
    let projectPath = slnDir @@ projectName
    let publishDir = binDir @@ projectName

    Directory.ensure publishDir
    Shell.cleanDir publishDir

    let msprops =
        [("WebPublishMethod", "FileSystem")
         ("PublishUrl", publishDir)
         ("ExcludeGeneratedDebugSymbol", "false")]
         @ props
         |> List.distinct

    MSBuild.build (fun p ->
      { p with
          ToolsVersion = Some "Current"
          Verbosity = Some(Quiet)
          Targets = ["WebPublish"]
          NodeReuse = false
          NoWarn = msbNowarn
          DisableInternalBinLog = true
          NoConsoleLogger = false
          BinaryLoggers = Some []
          FileLoggers = Some []
          DistributedLoggers = Some []
          Loggers = Some []
          Properties = msbPropertiesAppend version msprops
       }) (projectPath @@ projectName + ".csproj")

    let zipname = packageDeployable binDir projectName version
    zipname

  let packageWeb slnDir projectName version binDir =
      packageWebExt slnDir projectName version binDir []



[<RequireQualifiedAccess>]
module Testing =

  open Fake.Core
  open Fake.DotNet
  open Fake.IO
  open Fake.IO.FileSystemOperators
  open Fake.IO.Globbing.Operators
  
  [<Literal>]
  let DefaultUnitTestFilter = "(TestCategory!=integration)&(TestCategory!=Integration)"
  let UnitTestProjects srcDir = GlobbingPattern.createFrom srcDir ++ "**/*.Tests.csproj"

  [<Literal>]
  let DefaultSeleniumTestFilter = "(TestCategory==selenium)"
  let SeleniumTestProjects srcDir = GlobbingPattern.createFrom srcDir ++ "**/*.SeleniumTests.csproj"
  
  let dotNetOpt (opt: DotNet.Options) = opt
  let dotNetExec cmd = DotNet.exec dotNetOpt cmd

  let addCoverlet project = 
    project |> sprintf "%s package coverlet.msbuild" |> dotNetExec "add"

  let ensureCoverlet (projects : string seq) = projects |> Seq.map addCoverlet |> ignore

  //let generateTestReport = 
  //  let resultsDir = Path.getFullName Environment.defaultTestResultsDir |> Path.normalizeFileName

  //  let reports = sprintf "\"-reports:%s\\*.cobertura.xml\"" resultsDir
  //  let args =  new ExecParams(Program = "", CommandLine = "", Args = []) 

  //  Process.shellExec { Program = "" }

  let runTests (coverage: bool) (filter: string) (options: DotNet.TestOptions -> DotNet.TestOptions) (projects: IGlobbingPattern) =
    
    //ensureCoverlet projects
    let includePattern = Environment.defaultCoverletInclude
    let excludePattern = Environment.defaultCoverletExclude
    let resultsDir = Path.getFullName Environment.defaultTestResultsDir |> Path.normalizeFileName
    let coverageJson = resultsDir @@ "coverage.json"
    let isBuildServer = BuildServer.buildServer = TeamFoundation || Environment.publishTestResults

    let merge = if File.exists coverageJson then [("MergeWith", coverageJson)] else []
    let props = merge @ [
      ("CollectCoverage", if coverage then "true" else "false")
      ("CoverletOutput", sprintf "%s\\" resultsDir)
      ("CoverletOutputFormat", "json,cobertura,teamcity")
      ("Include", includePattern)
      ("Exclude", excludePattern)
      ("SkipAutoProps", "true")
    ]
    
    let defaultOptions (o: DotNet.TestOptions) = 
      { o with 
          Configuration = DotNet.BuildConfiguration.Debug
          Filter = Some filter
          ResultsDirectory = Some resultsDir
          Logger = if isBuildServer then Some "trx" else None
          Collect = if isBuildServer then Some "Code Coverage" else None
          MSBuildParams = { 
            o.MSBuildParams with 
              NoWarn = Some ["CS1591"]
              Properties = props } }

    let dotnet project = 
      printfn "running tests for `%s`" project
      DotNet.test (defaultOptions >> options) project
      project

    props |> List.iter (fun (x, y) -> printfn "(%s, %s)" x y)
    projects |> Seq.toList |> List.map dotnet

  let runUnitTestsWithFilter filter = runTests true filter (fun (o: DotNet.TestOptions) -> o)
  let runUnitTests (projects: IGlobbingPattern) = runUnitTestsWithFilter DefaultUnitTestFilter projects
  let runSeleniumTestsWithFilter filter = runTests false filter (fun (o: DotNet.TestOptions) -> o)
  let runSeleniumTests (projects: IGlobbingPattern) = runSeleniumTestsWithFilter DefaultSeleniumTestFilter projects

  /// runs NUnit with DotCover Code Coverage, filter is optional and so will
  /// default to "(TestCategory!=integration)&(TestCategory!=Integration)" when none is provided
  /// this is essentially the same as `dotcover "dotnet test"`, returns the full path
  /// to the output snapshot file
  //let runTestsCoverage (slnFile: string) project outDir (filter:string option) (configuration: DotNet.BuildConfiguration) =
  //  //let logDir = outDir
  //  let slnName = System.IO.Path.GetFileNameWithoutExtension slnFile
  //  let snapshotPath = outDir @@ (slnName + ".DotCover.snapshot")
  //  let testargs =
  //    [
  //      ["test"]
  //      [slnFile]
  //      ["--configuration"; (configuration |> tostr |> tolower)]
  //      ["--no-build"]
  //      ["--no-restore"]
  //      ["--filter"; (if filter.IsNone then DefaultFilter else filter.Value)]
  //      ["/nodeReuse:False"]
  //      ["-noconsolelogger"]
  //    ]
  //    |> List.concat
  //    |> Args.toWindowsCommandLine

  //  let coverargs =
  //    [
  //      Arg ("TargetExecutable", DotNet.Options.Create().DotNetCliPath)
  //      Arg ("TargetArguments", testargs)
  //      Arg ("Filters", sprintf "+:module=%s*;-:*.Tests" project)
  //      Arg ("Output", snapshotPath)
  //      //("LogFile", sprintf "%s\\%s.DotCover.cover.log" logDir slnName, true)
  //    ]
  //    |> Util.toCommandLine "cover" Default

  //  Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
  //  Shell.Exec (Environment.dotCoverPath, coverargs) |> ignore
  //  snapshotPath

  ///runs DotCover merge process. Takes multiple snapshot files and merges them into one. e.g., if there are three
  ///solution spaces in a repository, this merges them all into one file by the name {project}.DotCover.snapshot
  ///and places it in the {snapshotDir}. returns the full path to the output snapshot file

  //let runDotCoverMerge snapshotDir project =
  //  let snapshots = !! (snapshotDir @@ "*.snapshot") |> Seq.toList
  //  let mergedFile = snapshotDir @@ (sprintf "%s.DotCover.snapshot" project)

  //  match snapshots with
  //  | [] ->  None
  //  | [head] when mergedFile <> head ->
  //    Shell.rename mergedFile head
  //    Some mergedFile
  //  | [head] when mergedFile = head ->
  //    Some head
  //  | _ ->
  //    let sourcearg = (snapshots |> String.concat ";")
  //    let coverargs =
  //      [
  //        Arg ("Source", sourcearg)
  //        Arg ("Output", mergedFile)
  //      ]
  //      |> Util.toCommandLine "merge" Default

  //    Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
  //    Shell.Exec(Environment.dotCoverPath, coverargs) |> ignore
  //    snapshots |> List.iter File.delete
  //    Some mergedFile

  ///// creates a report for a snapshot file. two primary report types are XML and HTML.
  ///// given the input {snapshotFile} outputs the resulting {reportName}.DotCover.{reportType}
  ///// file in {snapshotDir}. returns the full path to the output report
  //let runDotCoverReport snapshotDir snapshotFile reportName reportType =
  //  let reportFile = snapshotDir @@ sprintf "%s.DotCover.%s" reportName reportType
  //  let coverargs =
  //    [
  //      Arg ("Source", snapshotFile)
  //      Arg ("Output", reportFile)
  //      Arg ("ReportType", reportType)
  //      Arg ("HideAutoProperties", bstr true)
  //    ]
  //    |> Util.toCommandLine "report" Default

  //  Trace.tracefn "%s %s" Environment.dotCoverPath coverargs
  //  Shell.Exec(Environment.dotCoverPath, coverargs) |> ignore
  //  reportFile

  ///// extracts the integer percent result from an XML file
  //let extractCoverage (xpath: string option) xmlPath =
  //  let xp = if xpath.IsNone then "/Root/@CoveragePercent" else xpath.Value
  //  let (_, value) = Xml.read_Int false xmlPath "" "" xp
  //  value


[<RequireQualifiedAccess>]
module Selenium =

  open Fake.Core
  open Fake.DotNet
  open Fake.IO.FileSystemOperators
  open Fake.IO.Globbing.Operators

  let run (remoteUrl: unit -> string) binDir shotsDir (dlls: FileSearch) (filter: string option) =

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
    Fake.Core.Environment.setEnvironVar "selenium.screenshot.directory" shotsDir

    let logger = if Environment.ensureCoverlet then "teamcity" else "console"
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
      if BuildServer.buildServer <> LocalBuild then
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
      Trace.logfn "buildServer:       %A" <| Fake.Core.BuildServer.buildServer

      if Fake.Core.BuildServer.buildServer <> LocalBuild then
        Trace.setBuildNumber <| Version.toString v

  ///creates a Fake Target that builds the solution contained at slnDir
  let build slnDir version =
    fun (_: TargetParameter) ->
      Build.buildCodeExt slnDir version [] None

  ///creates a Fake Target that builds the solution contained at slnDir
  let buildExt slnDir version (props: (string * string) list) (config: (DotNet.BuildOptions -> DotNet.BuildOptions) option) =
    fun (_: TargetParameter) ->
      Build.buildCodeExt slnDir version props config

  /// creates a Fake Target that takes a sequence of solutionFile * optional filter to run
  /// unit tests for and ultimately merging the results into one coverage result when
  /// code coverage is enabled
  //let testExt (slnFiles: seq<string * string option>) project outDir (configuration: DotNet.BuildConfiguration)=
  //  fun (_: TargetParameter) ->
  //    match Environment.runCoverage with
  //    | true ->
  //      slnFiles 
  //        |> Seq.iter (fun (file, filter) -> Build.runTestsCoverage file project outDir filter configuration |> ignore)
  //      Build.runDotCoverMerge outDir project
  //        |> fun snapshot ->
  //              match snapshot with
  //              | Some file ->
  //                  Build.runDotCoverReport outDir file project "HTML" |> ignore
  //                  Build.runDotCoverReport outDir file project "XML"
  //                    |> Build.extractCoverage Default
  //                    |> sprintf "Coverage: %i%%"
  //                    |> Trace.tracefn "##vso[buildStatus text='{build.status.text}, %s']"

  //                  Trace.tracefn "##vso[importData type='dotNetCoverage' tool='dotcover' path='%s']" file
  //              | None -> ()
  //    | false ->
  //      slnFiles |> Seq.iter (fun (file, filter) -> Build.runTestsNoCoverage file project outDir filter configuration |> ignore )

  ///// creates a Fake Target that takes a sequence of solutionFile * optional filter to run
  ///// unit tests for and ultimately merging the results into one coverage result when
  ///// code coverage is enabled
  //let test (slnFiles: seq<string * string option>) project outDir =
  //  testExt slnFiles project outDir DotNet.BuildConfiguration.Debug

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
  let DefaultHeartbeatName = "Can_land"

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

  let runHeartbeat binDir shotDir (dlls: FileSearch) (heartbeatTestName: string option) =
    fun (_: TargetParameter) ->
      let heartbeatTestName' =
        match heartbeatTestName with
        | None -> Some DefaultHeartbeatName
        | Some _' -> heartbeatTestName

      Selenium.run remoteUrl binDir shotDir dlls  heartbeatTestName'

  let runFull binDir shotDir (dlls: FileSearch) (testFilter: string option) =
    fun (_: TargetParameter) ->
      Selenium.run remoteUrl binDir shotDir dlls testFilter

  let prepareSelenium binDir shotDir =
    fun (_: TargetParameter) ->
      if BuildServer.buildServer <> LocalBuild then
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

  let publishSelenium shotDir =
    fun (_: TargetParameter) ->
      Trace.publish ImportData.BuildArtifact shotDir
