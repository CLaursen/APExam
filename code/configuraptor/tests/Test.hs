-- Rudimentary test suite. Feel free to replace anything.

import Absyn
import Parser
import Elaborator
import Solver

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) all_tests

all_tests = testGroup "all tests" [tests, my_tests]

tests = testGroup "Minimal tests" [
  testCase "parser" $
    parseString dbt @?= Right dbi,
  testCase "elaborator" $
    elaborate dbi @?= Right dbf,
  testCase "solver" $
    solve dbf goal 3 @?= Right sol
  ]
  where
    dbt = "resource r. component c: provides r."
    dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
    dbf = ([R "r"], [("c", [(R "r", (1,0))])])
    goal = [(R "r", (0,1))]
    sol = [("c", 1)]


my_tests = testGroup "my tests" [
  testCase "one resource" $
    parseString resource @?= Right resource_result,
  testCase "components.cr" $
    parseString components @?= Right components_result
  ]
  where
    resource = "resource RGB."
    resource_result = (["RGB"],[])
    components =  "{Resources} \n resource GB-RAM, DIMM-slot. \n resource Intel-CPU-socket, AMD-CPU-socket, x86-processor. \n resource monitor, VGA-port, HDMI-port. \n resource OS. \n resource DKK. \n {RAM} \n component 8GB-module: \n   provides 8 GB-RAM; \n   uses 1 DIMM-slot; \n   uses 500 DKK. \n component 4GB-module: \n   provides 4 GB-RAM; \n   uses 1 DIMM-slot; \n   uses 300 DKK. \n {Motherboards} \n component Intel-motherboard: \n   provides 2 DIMM-slot; \n   provides Intel-CPU-socket; \n   provides VGA-port; {integrated graphics} \n   uses 500 DKK. \n component AMD-motherboard: \n   provides 2 DIMM-slot; \n   provides AMD-CPU-socket; \n   uses 600 DKK. \n {Processors} \n component Intel-i8: \n   provides x86-processor; \n   uses Intel-CPU-socket; \n   uses 800 DKK. \n component AMD-Ryzen6: \n   provides x86-processor; \n   uses AMD-CPU-socket; \n   uses 700 DKK. \n    \n {Graphics cards and monitors} \n component NVIDIA-GPU: \n   provides 2 HDMI-port; \n   uses 1500 DKK. \n component monitor1: \n   provides monitor; \n   requires HDMI-port; \n   uses 1200 DKK. \n component monitor2: \n   provides monitor; \n   requires HDMI-port | VGA-port; \n   uses 1500 DKK. \n {Software} \n component Linux: \n   provides OS; \n   requires x86-processor; \n   uses 0 DKK. {Free!} \n component Windows: \n   provides OS; \n   requires x86-processor; \n   uses 1000 DKK."
    components_result = (["GB-RAM","DIMM-slot","Intel-CPU-socket","AMD-CPU-socket","x86-processor","monitor","VGA-port","HDMI-port","OS","DKK"],[IC "8GB-module" [(CKProvides,RSNum 8 (RSRes "GB-RAM")),(CKUses,RSNum 1 (RSRes "DIMM-slot")),(CKUses,RSNum 500 (RSRes "DKK"))],IC "4GB-module" [(CKProvides,RSNum 4 (RSRes "GB-RAM")),(CKUses,RSNum 1 (RSRes "DIMM-slot")),(CKUses,RSNum 300 (RSRes "DKK"))],IC "Intel-motherboard" [(CKProvides,RSNum 2 (RSRes "DIMM-slot")),(CKProvides,RSRes "Intel-CPU-socket"),(CKProvides,RSRes "VGA-port"),(CKUses,RSNum 500 (RSRes "DKK"))],IC "AMD-motherboard" [(CKProvides,RSNum 2 (RSRes "DIMM-slot")),(CKProvides,RSRes "AMD-CPU-socket"),(CKUses,RSNum 600 (RSRes "DKK"))],IC "Intel-i8" [(CKProvides,RSRes "x86-processor"),(CKUses,RSRes "Intel-CPU-socket"),(CKUses,RSNum 800 (RSRes "DKK"))],IC "AMD-Ryzen6" [(CKProvides,RSRes "x86-processor"),(CKUses,RSRes "AMD-CPU-socket"),(CKUses,RSNum 700 (RSRes "DKK"))],IC "NVIDIA-GPU" [(CKProvides,RSNum 2 (RSRes "HDMI-port")),(CKUses,RSNum 1500 (RSRes "DKK"))],IC "monitor1" [(CKProvides,RSRes "monitor"),(CKRequires,RSRes "HDMI-port"),(CKUses,RSNum 1200 (RSRes "DKK"))],IC "monitor2" [(CKProvides,RSRes "monitor"),(CKRequires,RSOr (RSRes "HDMI-port") (RSRes "VGA-port")),(CKUses,RSNum 1500 (RSRes "DKK"))],IC "Linux" [(CKProvides,RSRes "OS"),(CKRequires,RSRes "x86-processor"),(CKUses,RSNum 0 (RSRes "DKK"))],IC "Windows" [(CKProvides,RSRes "OS"),(CKRequires,RSRes "x86-processor"),(CKUses,RSNum 1000 (RSRes "DKK"))]])