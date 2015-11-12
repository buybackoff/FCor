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

    let getStatVariable (statVarMap : obj) (name : string) =
        let statVarMap = statVarMap:?>Map<string, StatVariable>
        if statVarMap.ContainsKey(name) then
            match statVarMap.[name] with
                | StatVariable.Factor(f) -> f:>obj
                | Covariate(c) -> c:>obj
        else null

    let getCsvVars (path : string) (sampleOnly : bool) =
        (Glm.importCsv path sampleOnly |> Array.map (fun svar -> svar.Name, svar) |> Map.ofArray):>obj

[<TypeProvider>]
type public CsvTypeProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "FCor.CsvProvider"
    let csvTy = ProvidedTypeDefinition(asm, ns, "CsvDataFrame", Some(typeof<obj>))
    let filename = ProvidedStaticParameter("filename", typeof<string>)
    do csvTy.DefineStaticParameters([filename], fun tyName [| :? string as filename |] ->
        let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)
        let statVars = Glm.importCsv resolvedFilename true 
        let ty = ProvidedTypeDefinition(asm, ns, tyName, Some(typeof<obj>))
        let ctor0 = ProvidedConstructor([], InvokeCode = fun [] -> <@@ CsvTypeProviderUtil.getCsvVars resolvedFilename false @@>)
        ty.AddMember ctor0
        let ctor1 = ProvidedConstructor([ProvidedParameter("csvPath", typeof<string>)], InvokeCode = fun [csvPath] -> <@@ CsvTypeProviderUtil.getCsvVars (%%csvPath:string) false @@>)
        ty.AddMember ctor1

        statVars |> Array.iter (fun statVar ->
                                   let name = statVar.Name
                                   match statVar with
                                       | StatVariable.Factor(_) ->
                                           let prop = ProvidedProperty(statVar.Name, typeof<Factor>, 
                                                                       GetterCode = fun args -> <@@ CsvTypeProviderUtil.getStatVariable (%%args.[0]:obj) name
                                                                                                 @@>)
                                           ty.AddMember prop  
                                       | Covariate(_) ->
                                           let prop = ProvidedProperty(statVar.Name, typeof<Covariate>, 
                                                                       GetterCode = fun args -> <@@ CsvTypeProviderUtil.getStatVariable (%%args.[0]:obj) name
                                                                                                 @@>)
                                           ty.AddMember prop      
                              )
        ty)
    do this.AddNamespace(ns, [csvTy])

[<assembly:TypeProviderAssembly>] 
do()




