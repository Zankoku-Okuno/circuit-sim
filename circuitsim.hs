{-
The initial state should probly be settable before time zero.
Richer data types than Bool might alleviate that need, though.

For useability, you should be able to give names to stuff.
Primary names when you create, secondary names as you use in subassemblies.
-- TODO secondary names

Atm, I'm returning all the i/o points from the circuit builder.
To make life easier, perhaps simply lookup any net and driver by name?
If I allow pre-zero initialization, that may not be necessary.

There should be something like a Sampler (kinda dual to Driver) which is able to store
the history of a net, dropping old samples according to a per-Sampler config'd policy
(probable policies are by amount of samples or by time past).
That way a historical record (to do derivatives) need only be stored when requested
by the logic that needs it.
-}


import SimpleCircuitSim


main :: IO ()
main = exampleLatch

exampleInverter = simulate circuit simulation
    where
    circuit :: Circuit (Driver Bool)
    circuit = do
        a <- newNet "a"
        b <- newNet "b"
        input <- newDriver "frontPanel" a
        initially input False
        output <- newDriver "fromInv" b
        initially output False
        inv <- newLogic "inv" [SomeNet a] $ do
            drive 1 output =<< not <$> probe a
        pure input
    simulation :: Driver Bool -> Simulation ()
    simulation input = do
        drive 10 input True
        loopSimUntil isEventQueueEmpty

exampleClock = simulate circuit simulation
    where
    circuit = do
        clk <- newNet "clk"
        clkGen <- newDriver "clkGen" clk
        initially clkGen False
        newLogic "clkGen" [SomeNet clk] $ do
            drive 10 clkGen =<< not <$> probe clk
        pure ()
    simulation () = loopSimFor 50


newNetDrv name = do
    n <- newNet name
    d <- newDriver "default" n
    pure (n, d)

exampleLatch = simulate circuit simulation
    where
    circuit = do
        (set, setD) <- newNetDrv "set"
        (reset, resetD) <- newNetDrv "reset"
        initially setD False
        initially resetD False
        withModule "register" $ do
            reg <- newNet "register"
            regSt <- newDriver "regState" reg
            initially regSt False
            newLogic "latch" [SomeNet set, SomeNet reset] $ do
                s <- probe set
                r <- probe reset
                case (s, r) of
                    (False, False) -> pure ()
                    (True, False) -> drive 3 regSt True
                    (False, True) -> drive 3 regSt False
                    (True, True) -> float 3 regSt
        pure (setD, resetD)
    simulation (set, reset) = do
        drive 10 set True
        drive 15 set False
        drive 30 reset True
        drive 35 reset False
        loopSimUntil isEventQueueEmpty

