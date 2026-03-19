module Build

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Utility

let rec private getArgImpl prefix = function
    | s :: m :: _ when s = prefix -> Some m
    | _ :: rest -> getArgImpl prefix rest
    | [] -> None

let getArgOpt prefix = cache <| fun (o: TargetParameter) ->
    getArgImpl prefix o.Context.Arguments

let getArg prefix ``default`` =
    getArgOpt prefix
    >> Option.defaultValue ``default``

let getArgWith prefix ``default`` = cache <| fun (o: TargetParameter) ->
    match getArgImpl prefix o.Context.Arguments with
    | Some x -> x
    | None -> ``default`` o

let getFlag flag = cache <| fun (o: TargetParameter) ->
    List.contains flag o.Context.Arguments

// Command-line parameters
let version = getArgOpt "-v" >> Option.defaultWith (fun () ->
    (dotnetOutput "nbgv" ["get-version"; "-v"; "SemVer2"]).Trim()
)
let cleanTest o = getArg "--clean-test" "false" o |> System.Boolean.TryParse ||> (&&)

// Constants
let contentBaseDir = slnDir </> "content"
let buildOutputDir = slnDir </> "build"
let packageName = "Bolero.Templates"
let packageOutputFile o = buildOutputDir </> $"{packageName}.{version o}.nupkg"
let testBuildRoot =
    Environment.GetEnvironmentVariable("BOLERO_TEMPLATE_TEST_BUILD_DIR")
    |> Option.ofObj
    |> Option.filter (String.IsNullOrWhiteSpace >> not)
    |> Option.defaultValue (slnDir </> "test-build")

let private generatedProjectDir baseDir projectName args =
    let suffix =
        if List.contains "--render=WebAssembly" args then ".Client"
        else ".Server"
    baseDir </> projectName </> "src" </> (projectName + suffix)

let private launchProfile projectDir =
    use doc = JsonDocument.Parse(File.ReadAllText(projectDir </> "Properties" </> "launchSettings.json"))
    let profile =
        doc.RootElement.GetProperty("profiles").EnumerateObject()
        |> Seq.find (fun p -> p.Value.GetProperty("commandName").GetString() = "Project")
    let url =
        profile.Value.GetProperty("applicationUrl").GetString().Split(';')
        |> Array.find (fun u -> u.StartsWith("http://", StringComparison.OrdinalIgnoreCase))
        |> fun u -> u.TrimEnd('/')
    profile.Name, url

let private waitForHomePage name (proc: Process) url =
    use client = new HttpClient(Timeout = TimeSpan.FromSeconds 5.)
    let deadline = DateTime.UtcNow.AddMinutes 1.
    let mutable lastError = "The application never became reachable."
    let mutable html = None
    while html.IsNone && DateTime.UtcNow < deadline do
        if proc.HasExited then
            failwithf "%s exited before responding at %s." name url
        try
            use response = client.GetAsync(url).Result
            let body = response.Content.ReadAsStringAsync().Result
            if response.IsSuccessStatusCode then
                html <- Some body
            else
                lastError <- $"HTTP %d{int response.StatusCode} from {url}: {body}"
        with ex ->
            lastError <- ex.Message
        if html.IsNone then
            Thread.Sleep 1000
    match html with
    | Some html -> html
    | None -> failwithf "Timed out waiting for %s at %s. Last error: %s" name url lastError

let private assertScriptAsset (name: string) (url: string) (html: string) =
    if html.Contains("InvalidOperationException") then
        failwithf "%s returned an InvalidOperationException page." name
    let script = Regex.Match(html, "_framework/blazor\\.[^\"']+\\.js")
    if not script.Success then
        failwithf "%s did not emit a Blazor framework script tag." name
    use client = new HttpClient(Timeout = TimeSpan.FromSeconds 5.)
    let scriptUrl = Uri(Uri(url + "/"), script.Value)
    use response = client.GetAsync(scriptUrl).Result
    if not response.IsSuccessStatusCode then
        failwithf "%s served %s with HTTP %d." name script.Value (int response.StatusCode)

let private assertStandaloneDotNet10Files baseDir projectName args =
    if List.contains "--render=WebAssembly" args then
        let clientDir = baseDir </> projectName </> "src" </> (projectName + ".Client")
        let fsproj = File.ReadAllText(clientDir </> (projectName + ".Client.fsproj"))
        if not (fsproj.Contains("OverrideHtmlAssetPlaceholders")) then
            failwithf "%s is missing OverrideHtmlAssetPlaceholders in the client project." projectName
        if not (fsproj.Contains("PackageReference Include=\"Microsoft.AspNetCore.Components.WebAssembly\"")) then
            failwithf "%s is missing the Microsoft.AspNetCore.Components.WebAssembly package reference." projectName
        let indexHtml = File.ReadAllText(clientDir </> "wwwroot" </> "index.html")
        if not (indexHtml.Contains("<script type=\"importmap\"></script>")) then
            failwithf "%s is missing the import map placeholder in wwwroot/index.html." projectName
        if not (indexHtml.Contains("_framework/blazor.webassembly#[.{fingerprint}].js")) then
            failwithf "%s is missing the fingerprinted Blazor WebAssembly script placeholder." projectName

let private smokeTestProject baseDir projectName args =
    let projectDir = generatedProjectDir baseDir projectName args
    let profileName, url = launchProfile projectDir
    Trace.tracefn $"Smoke testing {projectName} at {url}"
    let startInfo = ProcessStartInfo("dotnet", $"run --no-build --launch-profile \"{profileName}\"")
    startInfo.WorkingDirectory <- projectDir
    startInfo.UseShellExecute <- false
    startInfo.Environment["BROWSER"] <- "echo"
    use proc = Process.Start(startInfo)
    try
        let html = waitForHomePage projectName proc url
        assertScriptAsset projectName url html
    finally
        if not proc.HasExited then
            proc.Kill(true)
            proc.WaitForExit()

let variantsToTest =
    let serverModes = [
        ("LegacyWasm","LegacyWebAssembly")
        ("LegacyServer","LegacyServer")
        ("IntServer", "InteractiveServer")
        ("IntWasm", "InteractiveWebAssembly")
        ("IntAuto", "InteractiveAuto")
    ]
    [
        for pwak, pwav in [("Pwa", "true"); ("NoPwa", "false")] do
            for minik, miniv in [("Minimal", "true"); ("Full", "false")] do
                for htmlk, reloadv, htmlv in [("Reload", "true", "true"); ("NoReload", "false", "true"); ("NoHtml", "false", "false")] do
                    if not (miniv = "true" && htmlv = "true") then
                        for renderk, renderv in serverModes do
                            if renderv.StartsWith("Legacy") then
                                for hostk, hostv in [("Bolero", "bolero"); ("Razor", "razor"); ("Html", "html")] do
                                    $"{minik}.{renderk}.{hostk}.{htmlk}.{pwak}", [
                                        $"--minimal={miniv}"
                                        $"--hostpage={hostv}"
                                        $"--pwa={pwav}"
                                        $"--html={htmlv}"
                                        $"--hotreload={reloadv}"
                                        $"--render={renderv}"
                                    ]
                            else
                                $"{minik}.{renderk}.{htmlk}.{pwak}", [
                                    $"--minimal={miniv}"
                                    $"--pwa={pwav}"
                                    $"--html={htmlv}"
                                    $"--hotreload={reloadv}"
                                    $"--render={renderv}"
                                ]
                for htmlk, htmlv in [("Html", "true"); ("NoHtml", "false")] do
                    if not (miniv = "true" && htmlv = "true") then
                        $"{minik}.Wasm.{htmlk}.{pwak}", [
                            "--render=WebAssembly"
                            $"--minimal={miniv}"
                            $"--pwa={pwav}"
                            $"--html={htmlv}"
                        ]
    ]

Target.description "Create the NuGet package containing the templates."
Target.create "pack" <| fun o ->
    Shell.cp_r ".paket" "content/application/.paket"
    Paket.pack <| fun p ->
        { p with
            OutputPath = buildOutputDir
            Version = version o
            ToolType = ToolType.CreateLocalTool()
        }

Target.description "Install the locally built template. Warning: uninstalls any previously installed version."
Target.create "install" <| fun o ->
    if (dotnetOutput "new" ["list"]).Contains("bolero-app") then
        dotnet "new" ["uninstall"; packageName]
    dotnet "new" ["install"; packageOutputFile o; "--force"]

Target.description "Test all the template projects by building them."
Target.create "test-build" <| fun o ->
    // For each template variant, create, build and run a new project.
    let testsDir = testBuildRoot
    if cleanTest o && Directory.Exists(testsDir) then
        Directory.Delete(testsDir, recursive = true)
    let now = System.DateTime.Now
    let baseDir = testsDir </> now.ToString("yyyy-MM-dd.HH.mm.ss")
    Directory.CreateDirectory(baseDir) |> ignore
    for name, args in variantsToTest do
        // Prepend a letter and change extension to avoid generating
        // identifiers that start with a number.
        let projectName = "Test." + name
        dotnet' baseDir [] "new" [
            yield "bolero-app"
            yield "--nightly"
            yield! args
            yield "-o"
            yield projectName
        ]
        assertStandaloneDotNet10Files baseDir projectName args
        dotnet' (baseDir </> projectName) [] "build" ["-v"; "n"]
        smokeTestProject baseDir projectName args

Target.description "Run the full release pipeline."
Target.create "release" ignore

// Main dep path with soft dependencies
"pack"
    ==> "install"
    ==> "test-build"
    ==> "release"
|> ignore

Target.runOrDefaultWithArguments "pack"
