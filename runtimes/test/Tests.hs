import Hedgehog (checkParallel)

import Runtimes.MachineATest             (machineATests)

main :: IO ()
main = mapM_ checkParallel [ machineATests ]
