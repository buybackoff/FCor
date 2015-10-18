(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCor"
(**
FCor vs Matlab: American Option Binomial Pricing
=================================================
We will compare here Matlab and FCor implementations of binomial pricing of American Put option.
Both implementations will run on a system with Windows 7, 8GB RAM and Intel Core i7 2720QM CPU. 
*)
#r "FCor.dll"
open FCor
open FCor.Random
open FCor.BasicStats
open FCor.ExplicitConversion
open FCor.Math   
open System
(**
Here is Matlab implementation:

    [lang=matlab]
    function value = binomAmerican(S0, K, r, vol, T, n)
        dt = T / n;
        u = exp(vol*sqrt(dt));
        pUp = (u*exp(r*dt) - 1.0) / (u*u - 1.0) * exp(-r*dt);
        pDown = exp(-r*dt) - pUp;
        u2 = (1.0/(u*u))*ones(n+1,1);
        u2(1,:) = 1.0;
        S = S0 *(u^n)*cumprod(u2, 1);
        K2 = repmat(K, n+1,1);
        V = K2 - S;
        V(V < 0) = 0;
        for i = n:-1:1
            V1 = V(1:i);
            V2 = V(2:i+1);
            V(1:i) = pUp * V1 + pDown * V2;
            S = S / u;
            V = max(V, K2-S);
        end  
        value = V(1);
    end

    tic;
    v = zeros(1,100);
    for i = 1:100
        v(i) =  binomAmerican(44, 40, 0.06, 0.4, 1, 1000);
    end
    toc;
    
*)
(**
The loop takes ca. 1.8 sec to run.
This is FCor implementation:
*)
let binomAmerican (S0 : float) (K : float) (r : float) (vol : float) (T : float) n =
    let dt = T / float(n)
    let u = exp(vol * sqrt(dt))
    let pUp = (u*exp(r*dt) - 1.0) / (u*u - 1.0) * exp(-r*dt)
    let pDown = exp(-r*dt) - pUp
    use u2 = new Vector(n+1, 1.0/(u*u))
    u2.[0L] <- 1.0
    use S = S0 *(u**float(n)) * (cumprod u2)
    use K2 = new Vector(n+1, K)
    use V = K2 - S
    evalIn (iif (V.AsExpr .< 0.0) !!0.0 V) V
    let expr1 = S.AsExpr / u
    let expr2 = VectorExpr.Max(V.AsExpr, (K2-S.AsExpr))
    for i in n..(-1)..1 do
        use V1 = V.View(0,i-1)
        use V2 = V.View(1,i)
        evalIn (pUp * V1.AsExpr + pDown * V2.AsExpr) V1
        evalIn expr1 S
        evalIn expr2 V
    V.[0]

MklControl.SetMaxThreads(4)
for i in 1..100 do
    let binOption = binomAmerican 44. 40. 0.06 0.4 1. 1000
    ()
(**
It takes ca. 1.9 sec to run which is comparable to Matlab.
*)





