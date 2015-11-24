// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Testing
open System
open System.IO
open System.IO.Compression
#if MONO
#else
#load "packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FCor"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "F# numerical library"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "High performance open source F# numerical library"

// List of author names (for NuGet package)
let authors = [ "StatFactory Ltd" ]

// Tags for your project (for NuGet package)
let tags = "fsharp numerics math stat"

// File system information 
let solutionFile  = "FCor.sln"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*.Tests*.dll"

let matlabTestAssemblies = "tests/**/bin/Release/*.MatlabTests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "Statfactory" 
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "FCor"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/Statfactory"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

let compressFile (path : string) =
    let path = Path.GetFullPath(path)
    let data = File.ReadAllBytes path
    use memoryStream = new MemoryStream()
    use deflateStream = new DeflateStream(memoryStream, CompressionMode.Compress)
    deflateStream.Write(data, 0, data.Length)
    deflateStream.Flush()
    deflateStream.Close()
    let res = memoryStream.ToArray()
    File.WriteAllBytes(path.Replace("dll", "zip"), res)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath, 
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.fsproj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) -> CreateFSharpAssemblyInfo (folderName @@ "AssemblyInfo.fs") attributes)
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" (fun _ ->
    !! "src/**/*.fsproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", "bin" @@ (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs ["bin/FCor"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

//Zip MKL dlls
Target "ZipMKL" (fun _ ->
    compressFile "./bin/FCor.MKL/FCor.MKL.x86.dll"
    compressFile "./bin/FCor.MKL/FCor.MKL.x64.dll"
    compressFile "./bin/FCor.MKL/libiomp5md.x86.dll"
    compressFile "./bin/FCor.MKL/libiomp5md.x64.dll"
)

// --------------------------------------------------------------------------------------
// Build library & test project
Target "Build" (fun _ ->
    !! "src/**/*.fsproj"
      |> MSBuildRelease "" "Build"
      |> ignore
)

Target "BuildTests" (fun _ ->
    !! "tests/**/*.Tests.fsproj"
      |> MSBuildRelease "" "Build"
      |> ignore
)

Target "BuildMatlabTests" (fun _ ->
    !! "tests/**/*.MatlabTests.fsproj"
      |> MSBuildRelease "" "Build"
      |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests_x86" (fun _ ->
    !! testAssemblies
    |> xUnit (fun p -> 
        { p with
            ShadowCopy = false
            TimeOut = TimeSpan.FromMinutes 20.
            ToolPath = "./packages/xunit.runners/tools/xunit.console.clr4.x86.exe"
             })
)

Target "RunMatlabTests_x86" (fun _ ->
    !! matlabTestAssemblies
    |> xUnit (fun p -> 
        { p with
            ShadowCopy = false
            TimeOut = TimeSpan.FromMinutes 20.
            ToolPath = "./packages/xunit.runners/tools/xunit.console.clr4.x86.exe"
             })
)


Target "RunTests_x64" (fun _ ->
    !! testAssemblies
    |> xUnit (fun p -> 
        { p with
            ShadowCopy = false
            TimeOut = TimeSpan.FromMinutes 20.
            ToolPath = "./packages/xunit.runners/tools/xunit.console.clr4.exe"
             })
)

Target "RunMatlabTests_x64" (fun _ ->
    !! matlabTestAssemblies
    |> xUnit (fun p -> 
        { p with
            ShadowCopy = false
            TimeOut = TimeSpan.FromMinutes 20.
            ToolPath = "./packages/xunit.runners/tools/xunit.console.clr4.exe"
             })
)

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw project
    !! "src/**/*.fsproj"
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile 
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)

#endif

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    Paket.Pack(fun p -> 
        { p with
            OutputPath = "bin/FCor"
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes})
)

Target "PublishNuget" (fun _ ->
    Paket.Push(fun p -> 
        { p with
            WorkingDir = "bin/FCor" })
)


// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateReferenceDocs" (fun _ ->
    if not <| executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:REFERENCE"] [] then
      failwith "generating reference documentation failed"
)

let generateHelp' fail debug =
    let args =
        if debug then ["--define:HELP"]
        else ["--define:RELEASE"; "--define:HELP"]
    if executeFSIWithArgs "docs/tools" "generate.fsx" args [] then
        traceImportant "Help generated"
    else
        if fail then
            failwith "generating help documentation failed"
        else
            traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target "GenerateHelp" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target "GenerateHelpDebug" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target "KeepRunning" (fun _ ->    
    use watcher = !! "docs/content/**/*.*" |> WatchChanges (fun changes ->
         generateHelp false
    )

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.Dispose()
)

Target "GenerateDocs" DoNothing

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    //CleanDir tempDocsDir
    //Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    //StageAll tempDocsDir
    //Git.Commit.Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    //Branches.push tempDocsDir
)

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
    
    // release on github
    createClient (getBuildParamOrDefault "github-user" "") (getBuildParamOrDefault "github-pw" "")
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    // TODO: |> uploadFile "PATH_TO_FILE"    
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "CleanDocs"
  ==> "ZipMKL"
  ==> "Build"
  ==> "CopyBinaries"
//  ==> "BuildTests"
//  ==> "BuildMatlabTests"
//  ==> "RunTests_x86"
//  ==> "RunTests_x64"
//  ==> "RunMatlabTests_x86"
//  ==> "RunMatlabTests_x64"
//  ==> "GenerateHelp"
//  ==> "ReleaseDocs"
  ==> "NuGet"
  ==> "PublishNuget"
  ==> "Release"


//"Clean"
//  ==> "AssemblyInfo"
//  ==> "Build"
//  ==> "CopyBinaries"
//  ==> "RunTests"
//  ==> "GenerateReferenceDocs"
//  ==> "GenerateDocs"
//  ==> "All"
//  =?> ("ReleaseDocs",isLocalBuild)
//
//"All" 
//#if MONO
//#else
//  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
//#endif
//  ==> "NuGet"
//  ==> "BuildPackage"
//
//"CleanDocs"
//  ==> "GenerateHelp"
//  ==> "GenerateReferenceDocs"
//  ==> "GenerateDocs"
//
//"CleanDocs"
//  ==> "GenerateHelpDebug"
//
//"GenerateHelp"
//  ==> "KeepRunning"
//    
//"ReleaseDocs"
//  ==> "Release"
//
//"BuildPackage"
//  ==> "PublishNuget"
//  ==> "Release"

RunTargetOrDefault "All"
