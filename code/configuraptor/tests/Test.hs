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
  parser_tests,
  elaborator_tests,
  solver_tests
  ]


parser_tests = testGroup "parser tests" [
  resource_tests,
  name_tests,
  component_tests,
  mixed_declaration_tests,
  comment_tests,
  testCase "components.cr" $
    parseString components_program @?= Right components_program_result
  ]
  where
    --components.cr program
    components_program =  "{Resources} \n resource GB-RAM, DIMM-slot. \n resource Intel-CPU-socket, AMD-CPU-socket, x86-processor. \n resource monitor, VGA-port, HDMI-port. \n resource OS. \n resource DKK. \n {RAM} \n component 8GB-module: \n   provides 8 GB-RAM; \n   uses 1 DIMM-slot; \n   uses 500 DKK. \n component 4GB-module: \n   provides 4 GB-RAM; \n   uses 1 DIMM-slot; \n   uses 300 DKK. \n {Motherboards} \n component Intel-motherboard: \n   provides 2 DIMM-slot; \n   provides Intel-CPU-socket; \n   provides VGA-port; {integrated graphics} \n   uses 500 DKK. \n component AMD-motherboard: \n   provides 2 DIMM-slot; \n   provides AMD-CPU-socket; \n   uses 600 DKK. \n {Processors} \n component Intel-i8: \n   provides x86-processor; \n   uses Intel-CPU-socket; \n   uses 800 DKK. \n component AMD-Ryzen6: \n   provides x86-processor; \n   uses AMD-CPU-socket; \n   uses 700 DKK. \n    \n {Graphics cards and monitors} \n component NVIDIA-GPU: \n   provides 2 HDMI-port; \n   uses 1500 DKK. \n component monitor1: \n   provides monitor; \n   requires HDMI-port; \n   uses 1200 DKK. \n component monitor2: \n   provides monitor; \n   requires HDMI-port | VGA-port; \n   uses 1500 DKK. \n {Software} \n component Linux: \n   provides OS; \n   requires x86-processor; \n   uses 0 DKK. {Free!} \n component Windows: \n   provides OS; \n   requires x86-processor; \n   uses 1000 DKK."
    components_program_result = (["GB-RAM","DIMM-slot","Intel-CPU-socket","AMD-CPU-socket","x86-processor","monitor","VGA-port","HDMI-port","OS","DKK"],[IC "8GB-module" [(CKProvides,RSNum 8 (RSRes "GB-RAM")),(CKUses,RSNum 1 (RSRes "DIMM-slot")),(CKUses,RSNum 500 (RSRes "DKK"))],IC "4GB-module" [(CKProvides,RSNum 4 (RSRes "GB-RAM")),(CKUses,RSNum 1 (RSRes "DIMM-slot")),(CKUses,RSNum 300 (RSRes "DKK"))],IC "Intel-motherboard" [(CKProvides,RSNum 2 (RSRes "DIMM-slot")),(CKProvides,RSRes "Intel-CPU-socket"),(CKProvides,RSRes "VGA-port"),(CKUses,RSNum 500 (RSRes "DKK"))],IC "AMD-motherboard" [(CKProvides,RSNum 2 (RSRes "DIMM-slot")),(CKProvides,RSRes "AMD-CPU-socket"),(CKUses,RSNum 600 (RSRes "DKK"))],IC "Intel-i8" [(CKProvides,RSRes "x86-processor"),(CKUses,RSRes "Intel-CPU-socket"),(CKUses,RSNum 800 (RSRes "DKK"))],IC "AMD-Ryzen6" [(CKProvides,RSRes "x86-processor"),(CKUses,RSRes "AMD-CPU-socket"),(CKUses,RSNum 700 (RSRes "DKK"))],IC "NVIDIA-GPU" [(CKProvides,RSNum 2 (RSRes "HDMI-port")),(CKUses,RSNum 1500 (RSRes "DKK"))],IC "monitor1" [(CKProvides,RSRes "monitor"),(CKRequires,RSRes "HDMI-port"),(CKUses,RSNum 1200 (RSRes "DKK"))],IC "monitor2" [(CKProvides,RSRes "monitor"),(CKRequires,RSOr (RSRes "HDMI-port") (RSRes "VGA-port")),(CKUses,RSNum 1500 (RSRes "DKK"))],IC "Linux" [(CKProvides,RSRes "OS"),(CKRequires,RSRes "x86-processor"),(CKUses,RSNum 0 (RSRes "DKK"))],IC "Windows" [(CKProvides,RSRes "OS"),(CKRequires,RSRes "x86-processor"),(CKUses,RSNum 1000 (RSRes "DKK"))]])

    -- resource test
resource_tests = testGroup "resource test" [
  testCase "one resource" $
    parseString resource @?= Right resource_result,
  testCase "two resources in same declaration" $
    parseString resources @?= Right resources_result,
  testCase "two declarations with a resource" $
    parseString resources2 @?= Right resources2_result,
  testCase "resource keyword with upper and lower case letters" $
    parseString resource_insensitive @?= Right resource_insensitive_result,
  testCase "resource without whitespace after keyword" $
    parseString resource_nowhitespace @?= Left resource_nowhitespace_result
  ]
  where
    resource = "resource RGB."
    resource_result = (["RGB"],[])
    resources = "resource RGB, Fan."
    resources_result = (["RGB", "Fan"],[])
    resources2 = "resource RGB. resource Fan."
    resources2_result = (["RGB", "Fan"],[])
    resource_insensitive = "rEsOuRcE RGB."
    resource_insensitive_result = (["RGB"],[])
    resource_nowhitespace = "resourceRGB."
    resource_nowhitespace_result = "couldn't parse input"

    -- name test
name_tests = testGroup "name tests" [
  testCase "name with hyphens" $
    parseString name_with_hyphen @?= Right name_with_hyphen_result,
  testCase "name starting with a number" $
    parseString name_start_with_number @?= Right name_start_with_number_result,
  testCase "one of the word in the name only consists of a number" $
    parseString incorrect_name @?= Left incorrect_name_result,
  testCase "name is longer than 32 characters" $
    parseString name_too_long @?= Left name_too_long_result,
  testCase "name starting with a hyphen" $
    parseString name_hyphen_start @?= Left name_hyphen_start_result,
  testCase "name ending with a hyphen" $
    parseString name_hyphen_end @?= Left name_hyphen_end_result
  ]
  where
    name_with_hyphen = "resource R-G-B."
    name_with_hyphen_result = (["R-G-B"],[])
    name_start_with_number = "resource 9in-nail."
    name_start_with_number_result = (["9in-nail"],[])
    incorrect_name = "resource fa-1."
    incorrect_name_result = "couldn't parse input"
    name_too_long = "resource this-name-is-too-long-for-the-database."
    name_too_long_result = "couldn't parse input"
    name_hyphen_start = "resource -Ram."
    name_hyphen_start_result = "couldn't parse input"
    name_hyphen_end = "resource Ram-."
    name_hyphen_end_result = "couldn't parse input"

    -- component test
component_tests = testGroup "component tests" [
  testCase "one component with one clause" $
    parseString component_simple @?= Right component_simple_result,
  testCase "component with two clauses" $
    parseString component_two_clauses @?= Right component_two_clauses_result,
  testCase "component with num in RSpec" $
    parseString component_num_rspec @?= Right component_num_rspec_result,
  testCase "component with a num that is too high" $
    parseString component_num_too_high @?= Left component_num_too_high_result,
  testCase "component with two nums in a row" $
    parseString component_two_nums @?= Right component_two_nums_result,
  testCase "component with two RSpecs" $
    parseString component_two_rspecs @?= Right component_two_rspecs_result,
  testCase "component with an or RSpec" $
    parseString component_rspec_or @?= Right component_rspec_or_result,
  testCase "component with two or in an RSpec" $
    parseString component_rspec_two_or @?= Right component_rspec_two_or_result,
  testCase "component with 'or' and 'and' in an RSpec" $
    parseString component_rspec_or_and @?= Right component_rspec_or_and_result,
  testCase "component with 'or', 'and' and 'num' in an RSpec" $
    parseString component_rspec_or_and_num @?= Right component_rspec_or_and_num_result,
  testCase "component with use clause" $
    parseString component_uses_clause @?= Right component_uses_clause_result,
  testCase "two components" $
    parseString multiple_components @?= Right multiple_components_result,
  testCase "component with parenthesis in RSpec" $
    parseString component_rspec_parenthesis @?= Right component_rspec_parenthesis_result,
  testCase "component with no CName" $
    parseString component_no_cname @?= Left component_no_cname_result,
  testCase "component with no clauses" $
    parseString component_no_clauses @?= Left component_no_clauses_result
  ]
  where
    component_simple = "component GPU : provides CUDA-cores."
    component_simple_result = ([],[IC "GPU" [(CKProvides,RSRes "CUDA-cores")]])
    component_two_clauses = "component GPU : provides CUDA-cores ; requires PCIE-port."
    component_two_clauses_result = ([],[IC "GPU" [(CKProvides,RSRes "CUDA-cores"),(CKRequires,RSRes "PCIE-port")]])
    component_num_rspec = "component Ram : provides 8 GB."
    component_num_rspec_result = ([],[IC "Ram" [(CKProvides,RSNum 8 (RSRes "GB"))]])
    component_num_too_high = "component Ram : provides 1000000 GB."
    component_num_too_high_result = "couldn't parse input"
    component_two_nums = "component Ram : provides 4 8 GB."
    component_two_nums_result = ([],[IC "Ram" [(CKProvides,RSNum 4 (RSNum 8 (RSRes "GB")))]])
    component_two_rspecs = "component Ram : provides 8 GB , RGB."
    component_two_rspecs_result = ([],[IC "Ram" [(CKProvides,RSAnd (RSNum 8 (RSRes "GB")) (RSRes "RGB"))]])
    component_rspec_or = "component Monitor : requires HDMI | Display-port."
    component_rspec_or_result = ([],[IC "Monitor" [(CKRequires,RSOr (RSRes "HDMI") (RSRes "Display-port"))]])
    component_rspec_two_or = "component Monitor : uses HDMI | Display-port | DVI."
    component_rspec_two_or_result = ([],[IC "Monitor" [(CKUses,RSOr (RSOr (RSRes "HDMI") (RSRes "Display-port")) (RSRes "DVI"))]])
    component_rspec_or_and = "component Monitor : uses HDMI , Power | Display-port , Power."
    component_rspec_or_and_result = ([],[IC "Monitor" [(CKUses,RSOr (RSAnd (RSRes "HDMI") (RSRes "Power")) (RSAnd (RSRes "Display-port") (RSRes "Power")))]])
    component_rspec_or_and_num = "component Monitor : uses HDMI , 10 Power | Display-port , 10 Power."
    component_rspec_or_and_num_result = ([],[IC "Monitor" [(CKUses,RSOr (RSAnd (RSRes "HDMI") (RSNum 10 (RSRes "Power"))) (RSAnd (RSRes "Display-port") (RSNum 10 (RSRes "Power"))))]])
    component_uses_clause = "component Monitor : uses monitor-space."
    component_uses_clause_result = ([],[IC "Monitor" [(CKUses,RSRes "monitor-space")]])
    multiple_components = "component Ram : provides 8 GB. component GPU : provides CUDA-cores."
    multiple_components_result = ([],[IC "Ram" [(CKProvides,RSNum 8 (RSRes "GB"))],IC "GPU" [(CKProvides,RSRes "CUDA-cores")]])
    component_rspec_parenthesis = "component Ram : provides 8 GB , (RGB | no-RGB)."
    component_rspec_parenthesis_result = ([],[IC "Ram" [(CKProvides,RSAnd (RSNum 8 (RSRes "GB")) (RSOr (RSRes "RGB") (RSRes "no-RGB")))]])
    component_no_cname = "component : provides GB."
    component_no_cname_result = "couldn't parse input"
    component_no_clauses = "component Ram : ."
    component_no_clauses_result = "couldn't parse input"

    -- mix of resource and component test
mixed_declaration_tests = testGroup "resources and components mixed together" [
  testCase "a resource and a component" $
    parseString resource_component @?= Right resource_component_result,
  testCase "resource, component and resource" $
    parseString res_comp_res @?= Right res_comp_res_result,
  testCase "component, resource and component" $
    parseString comp_res_comp @?= Right comp_res_comp_result
  ]
  where
    resource_component = "resource DKK. component Ram : uses 800 DKK ; provides 2 8 GB."
    resource_component_result = (["DKK"],[IC "Ram" [(CKUses,RSNum 800 (RSRes "DKK")),(CKProvides,RSNum 2 (RSNum 8 (RSRes "GB")))]])
    res_comp_res = "resource DKK. component Ram : uses 800 DKK ; provides 2 8 GB. resource monitor-space."
    res_comp_res_result = (["DKK","monitor-space"],[IC "Ram" [(CKUses,RSNum 800 (RSRes "DKK")),(CKProvides,RSNum 2 (RSNum 8 (RSRes "GB")))]])
    comp_res_comp = "component Ram : uses 800 DKK ; provides 2 8 GB. resource monitor-space. component GPU : uses PCIE-port."
    comp_res_comp_result = (["monitor-space"],[IC "Ram" [(CKUses,RSNum 800 (RSRes "DKK")),(CKProvides,RSNum 2 (RSNum 8 (RSRes "GB")))],IC "GPU" [(CKUses,RSRes "PCIE-port")]])

    --comment test
comment_tests = testGroup "comment tests" [
  testCase "a simple comment" $
    parseString comment_simple @?= Right comment_simple_result,
  testCase "two comments in a row" $
    parseString comment_two_in_a_row @?= Right comment_two_in_a_row_result,
  testCase "comment in comment" $
    parseString comment_in_comment @?= Right comment_in_comment_result,
  testCase "two comments in a comment" $
    parseString comment_two_in_one_comment @?= Right comment_two_in_one_comment_result,
  testCase "comment before a declaration" $
    parseString comment_before_decl @?= Right comment_after_decl_result,
  testCase "comment as whitespace in a declaration" $
    parseString comment_as_whitespace_in_decl @?= Right comment_as_whitespace_in_decl_result,
  testCase "comment after a declaration" $
    parseString comment_after_decl @?= Right comment_after_decl_result,
  testCase "comment without '}'" $
    parseString comment_doesnt_end @?= Left comment_doesnt_end_result,
  testCase "comment in the middle of an RSpec" $
    parseString comment_in_rspec @?= Right comment_in_rspec_result
  ]
  where
    comment_simple = "{this is a comment}"
    comment_simple_result = ([],[])
    comment_two_in_a_row = "{first}{second}"
    comment_two_in_a_row_result = ([],[])
    comment_in_comment = "{outer{inner comment}comment}"
    comment_in_comment_result = ([],[])
    comment_two_in_one_comment = "{outer{first inner}comment{second inner}here}"
    comment_two_in_one_comment_result = ([],[])
    comment_before_decl = "{comment}resource Ram."
    comment_before_decl_result = (["Ram"],[])
    comment_as_whitespace_in_decl = "resource{comment}Ram."
    comment_as_whitespace_in_decl_result = (["Ram"],[])
    comment_after_decl = "resource Ram{comment}."
    comment_after_decl_result = (["Ram"],[])
    comment_doesnt_end = "{comment"
    comment_doesnt_end_result = "couldn't parse input"
    comment_in_rspec = "component Ram : provides 4{comment}GB."
    comment_in_rspec_result = ([],[IC "Ram" [(CKProvides,RSNum 4 (RSRes "GB"))]])

elaborator_tests = testGroup "elaborator tests" [
  lookres_tests
  ]

lookres_tests = testGroup "lookres tests" [
  testCase "same casing in resource list and rname" $
    lookres lookres_simple1 lookres_simple2 @?= Right lookres_simple_result,
  testCase "different casing in resource list and rname" $
    lookres lookres_different_casing1 lookres_different_casing2 @?= Right lookres_different_casing_result,
  testCase "one component with one clause" $
    lookres lookres_not_in_list1 lookres_not_in_list2 @?= Left lookres_not_in_list_result
  ]
  where
    lookres_simple1 = [R "test", R "TeSt2"]
    lookres_simple2 = "test"
    lookres_simple_result = R "test"
    lookres_different_casing1 = [R "test", R "TeSt2"]
    lookres_different_casing2 = "test2"
    lookres_different_casing_result = R "TeSt2"
    lookres_not_in_list1 = [R "test", R "TeSt2"]
    lookres_not_in_list2 = "test3"
    lookres_not_in_list_result = "Couldn't find a resource with any casing of the name: test3"

solver_tests = testGroup "Solver tests" [
  combine_tests,
  verify_tests
  ]

combine_tests = testGroup "combine tests" [
  testCase "a simple combine" $
    combine combine_simple1 combine_simple2 @?= combine_simple_result,
  testCase "resource in 2nd input goes between resources in 1st input" $
    combine combine_into_middle1 combine_into_middle2 @?= combine_into_middle_result,
  testCase "combine two rprofs that results in an empty list" $
    combine combine_to_nothing1 combine_to_nothing2 @?= combine_to_nothing_result,
  testCase "combine capital R is sorted before lower n" $
    combine combine_R_before_n1 combine_R_before_n2 @?= combine_R_before_n_result,
  testCase "combine keep the highest of the requirements for the same resource" $
    combine combine_max_require1 combine_max_require2 @?= combine_max_require_result,
  testCase "combine a mix of different tests" $
    combine combine_mix1 combine_mix2 @?= combine_mix_result
  ]
  where
    combine_simple1 = [(R "r1", (1,0))]
    combine_simple2 = [(R "r2", (1,0))]
    combine_simple_result = [(R "r1", (1,0)), (R "r2",(1,0))]
    combine_into_middle1 = [(R "r1", (1,0)), (R "r3", (1,0))]
    combine_into_middle2 = [(R "r2", (1,0))]
    combine_into_middle_result = [(R "r1", (1,0)), (R "r2", (1,0)), (R "r3", (1,0))]
    combine_to_nothing1 = [(R "r1", (1,0))]
    combine_to_nothing2 = [(R "r1", (-1,0))]
    combine_to_nothing_result = []
    combine_R_before_n1 = [(R "n1", (1,0))]
    combine_R_before_n2 = [(R "R2", (1,0))]
    --sorting is by Ascii order where capital letters are before lower case letters
    combine_R_before_n_result = [(R "R2", (1,0)), (R "n1", (1,0))]
    combine_max_require1 = [(R "r1", (3,4))]
    combine_max_require2 = [(R "r1", (2,2))]
    combine_max_require_result = [(R "r1", (5,4))]
    combine_mix1 = [(R "r0", (1,0)),(R "r1", (1,0)),(R "r3", (1,0)),(R "r5", (1,1)),(R "r6", (1,0)),(R "r9", (1,0))]
    combine_mix2 = [(R "r0", (-1,0)),(R "r2", (1,0)),(R "r4", (1,0)),(R "r5", (1,0)),(R "r7", (1,0)),(R "r8", (1,0))]
    combine_mix_result = [(R "r1",(1,0)),(R "r2",(1,0)),(R "r3",(1,0)),(R "r4",(1,0)),(R "r5",(2,1)),(R "r6",(1,0)),(R "r7",(1,0)),(R "r8",(1,0)),(R "r9",(1,0))]


verify_tests = testGroup "verify tests" [
  testCase "a simple verify" $
    verify verify_simpleDB verify_simpleGoal verify_simpleSol @?= Right verify_simple_result,
  testCase "solution not valid because goal is to low" $
    verify verify_no_validDB verify_no_validGoal verify_no_validSol @?= Left verify_no_valid_result,
  testCase "Component from solution not in the database" $
    verify verify_not_in_dbDB verify_not_in_dbgoal verify_not_in_dbSol @?= Left verify_not_in_db_result,
  testCase "Multiple components in possible solution" $
    verify verify_more_compsDB verify_more_compsGoal verify_more_compsSol @?= Right verify_more_comps_result,
  testCase "One component provides a resource another component then uses" $
    verify verify_use_provideDB verify_use_provideGoal verify_use_provideSol @?= Right verify_use_provide_result,
  testCase "More than 1 of some of the components" $
    verify verify_multiple_of_a_componentDB verify_multiple_of_a_componentGoal verify_multiple_of_a_componentSol @?= Right verify_multiple_of_a_component_result,
  testCase "Same component listed twice in solution" $
    verify verify_comp_listed_twiceDB verify_comp_listed_twiceGoal verify_comp_listed_twiceSol @?= Left verify_comp_listed_twice_result,
  testCase "Same component listed twice in solution with different casing" $
    verify verify_comp_listed_twice_with_dif_CaseDB verify_comp_listed_twice_with_dif_CaseGoal verify_comp_listed_twice_with_dif_CaseSol @?= Left verify_comp_listed_twice_with_dif_Case_result,
  testCase "Component have negate value in solution" $
    verify verify_comp_negative_valDB verify_comp_negative_valGoal verify_comp_negative_valSol @?= Left verify_comp_negative_val_result
  ]
  where
    verify_simpleDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_simpleGoal = [(R "DKK", (3000,0))]
    verify_simpleSol = [("GPU", 1)]
    verify_simple_result = [(R "CUDA",(2048,0)),(R "DKK",(1000,0))]
    verify_no_validDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_no_validGoal = [(R "DKK", (1000,0))]
    verify_no_validSol = [("GPU", 1)]
    verify_no_valid_result = "Solution isn't reachable from goal"
    verify_not_in_dbDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_not_in_dbgoal = [(R "DKK", (1000,0))]
    verify_not_in_dbSol = [("GPU", 1), ("Monitor", 2)]
    verify_not_in_db_result = "Couldn't find the component: 'Monitor' in the database."
    verify_more_compsDB = ([R "CUDA", R "DKK", R "Keyboard", R "Screen"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))]),("Monitor", [(R "DKK", (-1500,0)),(R "Screen", (1,0))]),("Corsair-keyboard", [(R "DKK", (-750,0)),(R "Keyboard", (1,0))])])
    verify_more_compsGoal = [(R "DKK", (5000,0))]
    verify_more_compsSol = [("GPU", 1), ("Corsair-keyboard", 1), ("Monitor", 1)]
    verify_more_comps_result = [(R "CUDA",(2048,0)),(R "DKK",(750,0)),(R "Keyboard",(1,0)),(R "Screen",(1,0))]
    verify_use_provideDB = ([R "CUDA", R "DKK", R "Keyboard", R "USB"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))]),("Motherboard", [(R "DKK", (-1500,0)),(R "USB", (6,0))]),("Corsair-keyboard", [(R "DKK", (-750,0)),(R "Keyboard", (1,0)), (R "USB", (-1,0))])])
    verify_use_provideGoal = [(R "DKK", (5000,0))]
    verify_use_provideSol = [("GPU", 1), ("Corsair-keyboard", 1), ("Motherboard", 1)]
    verify_use_provide_result = [(R "CUDA",(2048,0)),(R "DKK",(750,0)),(R "Keyboard",(1,0)),(R "USB",(5,0))]
    verify_multiple_of_a_componentDB = ([R "CUDA", R "DKK", R "Keyboard", R "Screen"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))]),("Monitor", [(R "DKK", (-1500,0)),(R "Screen", (1,0))]),("Corsair-keyboard", [(R "DKK", (-750,0)),(R "Keyboard", (1,0))])])
    verify_multiple_of_a_componentGoal = [(R "DKK", (10000,0))]
    verify_multiple_of_a_componentSol = [("GPU", 2), ("Corsair-keyboard", 1), ("Monitor", 3)]
    verify_multiple_of_a_component_result = [(R "CUDA",(4096,0)),(R "DKK",(750,0)),(R "Keyboard",(1,0)),(R "Screen",(3,0))]
    verify_comp_listed_twiceDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_comp_listed_twiceGoal = [(R "DKK", (3000,0))]
    verify_comp_listed_twiceSol = [("GPU", 1), ("GPU", 1)]
    verify_comp_listed_twice_result = "The solution contains the same component multiple times"
    verify_comp_listed_twice_with_dif_CaseDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_comp_listed_twice_with_dif_CaseGoal = [(R "DKK", (3000,0))]
    verify_comp_listed_twice_with_dif_CaseSol = [("GPU", 1), ("gpu", 1)]
    verify_comp_listed_twice_with_dif_Case_result = "The solution contains the same component multiple times"
    verify_comp_negative_valDB = ([R "DKK", R "CUDA"],[("GPU", [(R "CUDA", (2048,0)),(R "DKK", (-2000,0))])])
    verify_comp_negative_valGoal = [(R "DKK", (3000,0))]
    verify_comp_negative_valSol = [("GPU", 0)]
    verify_comp_negative_val_result = "One of the components have a negative value"