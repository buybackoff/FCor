(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
FCore vs Matlab: Least Squares Monte Carlo American Option Pricing
==================================================================
We will compare here Matlab and FCore implementations of Least Squares Monte Carlo pricing of American Put option.
Both implementations will run on a system with Windows 7, 8GB RAM and Intel Core i7 2720QM CPU. 
*)
#r "FCore.dll"
open FCore
open FCore.Random
open FCore.BasicStats
open FCore.ExplicitConversion
open FCore.LinearAlgebra
open FCore.Math   
open System
(**
Here is Matlab implementation:

    [lang=matlab]
    function value = lsmPutAmerican(S0, K, r, vol, T, n, sims)
        dt = T / n;
        N = n + 1;
        mu = (r-vol*vol/2)*dt;
        sigR = vol*sqrt(dt);
        R = lognrnd(mu, sigR, sims, N);
        R(:, 1) = S0;
        S = cumprod(R, 2);
        CF = K - S(:, N);
        CF(CF < 0) = 0;
        for i = N-1:-1:2
            Si = S(:, i);
            inMoney = Si < K;
            outMoney = Si >= K;
            X = Si(inMoney);
            Y = CF(inMoney) * exp(-r*dt);
            XReg = horzcat(ones(size(X)), X, X.*X);
            a = XReg\Y;
            C = XReg * a;
            payoff = K - X;
            isExercised = payoff > C;
            Y(isExercised) = payoff(isExercised);
            CF(inMoney) = Y;
            CF(outMoney) = CF(outMoney)*exp(-r*dt);
        end
        value = mean(CF*exp(-r*dt), 1);
    end

    tic;
    v = zeros(1,10);
    for i = 1:10
        v(i) =  lsmPutAmerican(44.0,40.0,0.06,0.4,1,50,100000);
    end
    toc;
    
*)
(**
The loop takes ca. 5.6 sec to run.
This is FCore implementation:
*)
let lsmPutAmerican (S0 : float) (K : float) (r : float) (vol : float) (T : float) (n : int) (sims : int) =
    let dt = T / float(n)
    let N = n + 1
    let mu = (r-vol*vol/2.0)*dt
    let sigma = vol*sqrt(dt)
    use rng = new MT19937Rng()
    let lognormRndM : float -> float -> float -> float -> int -> int -> Matrix = lognormRnd rng
    use R = lognormRndM mu sigma 0.0 1.0 sims N
    R.[*, 0] <- S0 
    use S = cumprod R RowAxis
    use CF = VectorExpr.Max(K - S.ColView(N-1).AsExpr, 0.0) |> eval
    let discF = exp(-r*dt)
    for i in N-2..-1..1 do
        use Si = S.ColView(i)
        use inMoney = Si .< K
        use outMoney = BoolVector.Not inMoney 
        use X = Si.[inMoney]
        use Y = CF.[inMoney] * discF
        if X.LongLength > 0L then
            use XReg = new Matrix(X.LongLength, 3L, 0.0)
            XReg.[*, 0] <- 1.0
            XReg.[*, 1] <- X
            XReg.[*, 2] <- X .^ 2
            use a = qrSolveFull XReg Y
            use C = XReg * a
            use payoff = K - X
            use isExercised = payoff .> C
            Y.[isExercised] <- payoff.[isExercised]
            CF.[inMoney] <- Y
        CF.[outMoney] <- CF.[outMoney] * discF
    mean (CF * discF)


MklControl.SetMaxThreads(4)
for i in 1..10 do
    let lsmOption = lsmPutAmerican 44.0 40.0 0.06 0.4 1.0 50 100000
    ()
(**
It takes ca. 4.2 sec to run which is 25% less than Matlab.
*)



