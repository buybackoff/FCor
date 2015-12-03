//#r "./bin/release/Interop.MLApp.dll"
#r "./bin/release/FCor.Tests.dll"
#r "./bin/release/FCor.dll"

//open MLApp
open System
open FCor
open FCor.Tests
open FCor.Tests.Util
open FCor.ExplicitConversion
open FCor.CsvProvider
open FCor.StatModels

type GammaCsv = CsvDataFrame< "gamma.csv" >
let df = new GammaCsv()
let model = glm (df.lifetime <~> df.mfg) true Gamma Ln 100 1e-9
df.lifetime.GetStats()
log(df.lifetime.GetStats().Mean)



