﻿#r "paket:
nuget BlackFox.Fake.BuildTask
nuget JetBrains.dotCover.CommandLineTools
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.Xml
nuget Fake.Core.Target
nuget Fake.Core.Trace
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.NuGet
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.Runtime
//"
#load ".microcelium/lib/microcelium.fsx"

#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#r "netstandard"
#endif

open BlackFox.Fake
open Fake.Core
open Fake.BuildServer
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Microcelium.Fake

BuildServer.install [ TeamFoundation.Installer ]

if BuildServer.buildServer = LocalBuild then
  CoreTracing.ensureConsoleListener ()

(* read EnvVar
  let myEnvVar = Util.environVarOrDefault ["myEnvVarKey1"; "myEnvVarKey2"] "default value"
*)

(* setting the version
  let version = Version.fromVersionIni ()   //looks for a .\Version.ini file
  let version = Version.fromFile "filepath" //looks for a file @ "filepath"
*)

let version = Version.fromEnvironment ()
let versionparts = Version.parts version
let versionstr = Version.toString version

let fullpath = Path.getFullName << Path.normalizeFileName

let srcDir = fullpath "./src"
let binDir = fullpath (Microcelium.Fake.Environment.defaultBinDir)

let project = "{project}"
let tests = seq { yield (srcDir, Default) }

Target.create "Clean" <| Targets.clean srcDir binDir
Target.create "Version" <| Targets.version version
Target.create "Build" <| Targets.build srcDir versionparts
Target.create "Test" <| Targets.test tests project binDir
Target.create "Publish" <| Targets.publish binDir

(* about the only part that needs customized *)
Target.create "Package" (fun _ ->
  //nuget: solutionDir, projectName, version, outputDir
  Build.packageNuget srcDir "{project}" versionparts binDir

  //.NET Framework Web: solutionDir, projectName, version, outputDir
  Build.packageWeb srcDir "{project}.Web" versionparts binDir |> ignore

  //.NET Framework Tasks/Vals or .NET Core Web: solutionDir, projectName, version, outputDir
  Build.packageProject srcDir "{project}.Api" versionparts binDir |> ignore
)

Target.create "ToLocalNuget"  <| Targets.publishLocal binDir versionstr

(* `NuGetCachePath` EnvVar should be set to your Nuget Packages Install dir already, but
    `TargetVersion` should be set prior to running build.bat :
    set TargetVersion=1.14 *)
Target.create "ToLocalPackageRepo" <| Targets.packageLocal srcDir

(* selenium

// from prompt set variables prior to run (case sensitive):
// > set remoteUrl=http://endpointToTest
// > set selenium.username=yourusername
// > set selenium.password=yourpassword
// if you want to watch
// > set headless=false
// you can override any configuration parameter via prefix.ParameterKey
// and you can generally find this at SeleniumProject/TestHelpers/Config.cs

let shotsDir = Path.getFullName "./screenshots"
let selenDir = Path.getFullName "./selenium-bin"
let selenZip = fun () -> !! (binDir @@ "*Selenium*.zip") |> Seq.head
let prefix = "{project-prefix}"

Target.create "PrepareSelenium" <| Targets.prepareSelenium selenDir shotsDir
Target.create "DeploySelenium" <| Targets.deploySelenium selenDir selenZip
Target.create "RunHeartbeat" <| Targets.runHeartbeat selenDir project shotsDir DefaultFiles Default
Target.create "RunSelenium" <| Targets.runFull selenDir prefix shotsDir DefaultFiles Default
Target.create "PublishSelenium" <| Targets.publishSelenium shotsDir

"PrepareSelenium" ==> "DeploySelenium"
"DeploySelenium"  ==> "RunHeartbeat" =?> ("PublishSelenium", Environment.runPublish)
"DeploySelenium"  ==> "RunSelenium" =?> ("PublishSelenium", Environment.runPublish)

//*)

"Clean"
  ==> "Version"
  ==> "Build"
  ==> "Test"
  ==> "Package"
  =?> ("Publish", Environment.runPublish)

Target.runOrDefault <| if Environment.runPublish then "Publish" else "Test"
