(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
FCore vs Matlab: Pi Monte Carlo
===============================
We will compare here Matlab and FCore implementations of a simple algorithm to calculate Pi by Monte Carlo simulation.
Both implementations will run on a system with Windows 7, 8GB RAM and Intel Core i7 2720QM CPU. 
*)
#r "FCore.dll"
open FCore
open FCore.Random
open FCore.Math   
open System
(**
Here is Matlab implementation:

    [lang=matlab]
    function value = piMC(n)
        x1 = rand(n,1);
        x2 = rand(n,1);
        value = sum(x1.^2+x2.^2 <= 1)/n*4;
    end
    
*)
(**
It takes ca. 2.8 sec to run it for n = 100 000 000.
This is FCore implementation:
*)
let piMC(n : int) =
    MklControl.SetMaxThreads(4)
    use rng = new MT19937Rng()
    use x1 = rand rng n : Vector
    use x2 = rand rng n : Vector
    use inCircle = (x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0 |> eval
    (inCircle.[inCircle].LongLength |> float) / float(n) * 4.0
(**
It takes ca. 2.1 sec to run it for n = 100 000 000, which is 25% less than Matlab.
We could also use a faster Random Number Generator in FCore:
*)
let piMC2(n : int) =
    MklControl.SetMaxThreads(4)
    use rng1 = new SOBOLRng(2)
    use rng2 = rng1.Copy()
    rng1.LeapFrog(0)
    rng2.LeapFrog(1)
    use x1 = rand rng1 n : Vector
    use x2 = rand rng2 n : Vector
    use inCircle = (x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0 |> eval
    (inCircle.[inCircle].LongLength |> float) / float(n) * 4.0
(**
Now it takes ca. 1.85 sec to run for n = 100 000 000.
*)









