#r "paket:
nuget NuGet.CommandLine
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.Runtime
//"
#load ".fake/build.fsx/intellisense.fsx"
#load "src/microcelium-fake/lib/microcelium.fsx"
open Fake.Core
open Fake.BuildServer
open Fake.IO
open Fake.Core.TargetOperators
open Microcelium.Fake

BuildServer.install [ TeamFoundation.Installer ]

if BuildServer.buildServer = LocalBuild then
  CoreTracing.ensureConsoleListener ()

(* read EnvVar
  let myEnvVar = Util.environVarOrDefault ["myEnvVarKey1"; "myEnvVarKey2"] "default value"
*)

let version = Version.fromEnvironment () //gets it from the environment
let versionparts = Version.parts version
let versionstr = Version.toString version

let srcDir = Path.getFullName "./src"
let binDir = Environment.defaultBinDir

let project = "microcelium-fake"
let tests = seq { yield (srcDir, Default) }

Target.create "Clean" <| Targets.clean srcDir binDir
Target.create "Version" <| Targets.version version
Target.create "Build" <| Targets.buildExt srcDir versionparts [("CompileLib", bstr true)]
Target.create "Test" <| Targets.test tests project binDir
Target.create "Publish" <| Targets.publish binDir

(* about the only part that needs customized *)
Target.create "Package" (fun _ ->
  let props = [("CompileLib", bstr false); ("NoDefaultExcludes", bstr true)]
  Build.packageNugetExt srcDir "microcelium-fake" versionparts binDir props |> ignore
)

Target.create "ToLocalNuget"  <| Targets.publishLocal binDir versionstr

(* `NuGetCachePath` EnvVar should be set to your Nuget Packages Install dir already, but
    `TargetVersion` should be set prior to running build.bat :
    set TargetVersion=1.14 *)

Target.create "ToLocalPackageRepo" <| Targets.packageLocal srcDir

"Clean"
  ==> "Version"
  ==> "Build"
  ==> "Test"
  ==> "Package"
  =?> ("Publish", Environment.runPublish)

Target.runOrDefault <| if Environment.runPublish then "Publish" else "Test"
