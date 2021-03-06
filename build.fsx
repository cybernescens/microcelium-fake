#r "paket:
nuget NuGet.CommandLine = 4.7.0
nuget Fake.BuildServer.TeamCity
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Runtime
//"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.BuildServer
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

BuildServer.install [ TeamCity.Installer ]
CoreTracing.ensureConsoleListener ()

let runPublish = (Environment.environVarOrDefault "PUBLISH" "1") = "1"
let runCleanup = (Environment.environVarOrDefault "CLEANUP" "1") = "1"
let release = (Environment.environVarOrDefault "Release" "0") = "1"

let versionMajorMinor = "1.0"

/// gets a list of properties that is passable to MSBuild, also configuring version properties and appends an existings list
let inline msbPropertiesAppend version (exist: (string * string) list) =
  exist @ [("VersionPrefix", fst version); ("VersionSuffix", snd version)] |> List.distinct

///gets a list of properties that is passable to MSBuild, also configuring version properties
let inline msbProperties version = msbPropertiesAppend version []
let msbNowarn = Some ["CS1591"; "CS0108"; "CS0618"; "CS0672"; "NU1701"]

let makeVer = sprintf "%s.%s"
let incVer (v : string) =
  v.Split '.'
  |> (fun x ->
        match x with
          | [|maj;min|]     -> sprintf "%d.%d" (int maj) (int min + 1)
          | _               -> failwithf "Cannot understand version: '%s'" v)

// let (versionPrefix, versionSuffix) =
//     match Fake.Core.BuildServer.buildServer with
//     | TeamCity  -> (makeVer versionMajorMinor Fake.Core.BuildServer.buildVersion, "")
//     | _         -> (makeVer (incVer versionMajorMinor) "0", "developer")

let (versionPrefix, versionSuffix) =
    match release with
    | true  -> (makeVer versionMajorMinor "0", "")
    | false -> (makeVer (incVer versionMajorMinor) "0", "developer")

let version =
  match versionSuffix with
  | ""  -> versionPrefix
  | _   -> sprintf "%s-%s" versionPrefix versionSuffix

let srcDir = Path.getFullName "./src"
let binDir = Path.getFullName "./bin"

let package projectName (props: (string * string) list option) =
  let properties = [("PackageVersion", version);("CompileLib", string false)] @ if props.IsSome then props.Value else []
  DotNet.pack(fun p ->
    { p with
        Configuration = DotNet.BuildConfiguration.Debug
        OutputPath = Some binDir
        NoBuild = true
        NoRestore = true
        MSBuildParams =
          { p.MSBuildParams with
              NodeReuse = false
              NoWarn = msbNowarn
              Properties = msbPropertiesAppend (versionPrefix, versionSuffix) properties }
    }) (srcDir @@ projectName)

Target.create "Clean" (fun _ ->
  if runCleanup then
    Fake.IO.Directory.ensure binDir
    !! (srcDir @@ "**/bin")
    ++ (srcDir @@ "**/obj")
    ++ binDir
    |> Shell.cleanDirs
)

Target.create "Version" (fun _ ->
  Trace.logfn "versionMajorMinor: %s" versionMajorMinor
  Trace.logfn "versionPrefix:     %s" versionPrefix
  Trace.logfn "versionSuffix:     %s" versionSuffix
  Trace.logfn "version:           %s" version

  if Fake.Core.BuildServer.buildServer = TeamCity then
    Trace.setBuildNumber version
)

Target.create "Build" (fun _ ->
  DotNet.build (fun p ->
    { p with
        Configuration = DotNet.BuildConfiguration.Debug
        MSBuildParams =
          { p.MSBuildParams with
              NodeReuse = false
              NoWarn = msbNowarn
              Properties = msbPropertiesAppend (versionPrefix, versionSuffix) [("CompileLib", string false)]}
    }) (srcDir @@ "microcelium-fake" @@ "microcelium-fake.fsproj")
)

Target.create "Package" (fun _ ->
  package "microcelium-fake" <| Some [("NoDefaultExcludes", string true)]
)

Target.create "Publish" (fun _ ->
  Trace.publish ImportData.BuildArtifact binDir
)

"Clean"
  ==> "Version"
  ==> "Build"
  ==> "Package"
  =?> ("Publish", runPublish)

Target.runOrDefaultWithArguments <| if runPublish then "Publish" else "Build"
