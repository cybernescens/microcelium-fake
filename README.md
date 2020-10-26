# Introduction
A basic framework for making build scripts for .NET projects. Attempts
to simplify it as much as possible by requiring very little
customization once deployed to a repository.

# Getting Started
To install at your target:
```
cd ./your-target-repository-root
dotnet tool install --tool-path . microcelium-fake
```

Once installed run:
```
./microcelium-fake --help
```
to see options.

Usually all you will need to do is:
```
./moicrocelium-fake -p=projectName --pre=PREFIX
```

And finally if the solution space is going to consist of Selenium
tests:
```
./moicrocelium-fake -p=projectName --pre=PREFIX -s #[or --selenium]
```

Where not installed before it will install a company-wide
**.gitignore**, **Directory.Build.props**,**build.fsx**, and
**build.sh**. You can run it after an update and it will sync the
.gitignore and Directory.Build.props files without clobbering your
changes. However, the `-f` (force) option must be specified to
overwrite the build.fsx and build.sh files as repository customization
are always going to live in build.fsx.

Once installed direct your attention to the build.fsx file and in
there you will find the following code block:
```
(* about the only part that needs customized *)
Target.create "Package" (fun _ ->
  //nuget: solutionDir, projectName, version, outputDir
  Build.packageNuget srcDir "{project}" versionparts binDir

  //.NET Framework Web: solutionDir, projectName, version, outputDir
  Build.packageWeb srcDir "{project}.Web" versionparts binDir |> ignore

  //.NET Framework Tasks/Vals or .NET Core Web: solutionDir, projectName, version, outputDir
  Build.packageProject srcDir "{project}.Api" versionparts binDir |> ignore
)
```

This is the Packing target and where you specify the artifacts of the
build. In most cases that is about all that needs changed, for more
complex project or projects with a lot of different solutions and
artifacts it will probably get a good deal more complicated.

# Section Needed: Expected Conventions

# Build and Test
To locally build and test this repository just run: `./build.sh`.
