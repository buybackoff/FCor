namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open System.IO
open FCor.ExplicitConversion
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

module CsvTypeProviderUtil =

    let getStatVariable (dataFrame : obj) (name : string) =
        let dataFrame = dataFrame:?>DataFrame
        match dataFrame.[name] with
            | StatVariable.Factor(f) -> f:>obj
            | Covariate(c) -> c:>obj

    let getCsvVars (path : string) (delimiter : char[]) (dropVars : string[]) (hasHeaders : bool) (sampleOnly : bool) =
        let svars = Glm.importCsv path delimiter dropVars hasHeaders sampleOnly
        new DataFrame(svars):>obj

[<TypeProvider>]
type public CsvTypeProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "FCor.CsvProvider"
    let csvTy = ProvidedTypeDefinition(asm, ns, "CsvDataFrame", Some(typeof<obj>))
    let filename = ProvidedStaticParameter("Filename", typeof<string>)
    let separator = ProvidedStaticParameter("Separator", typeof<string>, ",")
    let dropColumns = ProvidedStaticParameter("DropColumns", typeof<string>, "")
    let hasHeaders = ProvidedStaticParameter("HasHeaders", typeof<bool>, true)

    do csvTy.DefineStaticParameters([filename;separator;dropColumns;hasHeaders], fun tyName paramValues ->
        let filename = paramValues.[0]:?>string
        let delimiter = (paramValues.[1]:?>string).ToCharArray()
        let dropCols = (paramValues.[2]:?>string).Split(delimiter) |> Array.map (fun s -> s.Trim())
        let hasHeaders = paramValues.[3]:?>bool
        let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)
        let dataFrame = new DataFrame(Glm.importCsv resolvedFilename delimiter dropCols hasHeaders true)
        let ty = ProvidedTypeDefinition(asm, ns, tyName, Some(typeof<obj>))
        ty.HideObjectMethods <- true
        let ctor0 = ProvidedConstructor([], InvokeCode = fun [] -> <@@ CsvTypeProviderUtil.getCsvVars resolvedFilename delimiter dropCols hasHeaders false @@>)
        ty.AddMember ctor0
        let ctor1 = ProvidedConstructor([ProvidedParameter("csvPath", typeof<string>)], InvokeCode = fun [csvPath] -> <@@ CsvTypeProviderUtil.getCsvVars (%%csvPath:string) delimiter dropCols hasHeaders false @@>)
        ty.AddMember ctor1

        dataFrame.Factors |> List.iter (fun factor ->
                                           let name = factor.Name
                                           let prop = ProvidedProperty(name, typeof<Factor>,  GetterCode = fun args -> <@@ CsvTypeProviderUtil.getStatVariable (%%args.[0]:obj) name @@>)
                                           ty.AddMember prop  
                                       )
        dataFrame.Covariates |> List.iter (fun covariate ->
                                               let name = covariate.Name
                                               let prop = ProvidedProperty(name, typeof<Covariate>, GetterCode = fun args -> <@@ CsvTypeProviderUtil.getStatVariable (%%args.[0]:obj) name @@>)
                                               ty.AddMember prop  
                                       )
        let dataFrameProp = ProvidedProperty("DataFrame", typeof<DataFrame>, GetterCode = fun args -> <@@ %%args.[0] @@>)
        ty.AddMember dataFrameProp  
        ty)
    do this.AddNamespace(ns, [csvTy])

[<assembly:TypeProviderAssembly>] 
do()




