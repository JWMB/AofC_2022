module Tests

open System
open Xunit

[<Fact>]
let ``D1`` () =
    let input = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""
    let pt1 = D1.part1 input
    Assert.Equal(24000, pt1)
    
    let pt2 = D1.part2 input
    Assert.Equal(45000, pt2)


[<Fact>]
let ``D2`` () =
    let input = """
A Y
B X
C Z
"""
    let pt1 = D2.part1 input
    Assert.Equal(15, pt1)
    
    let pt2 = D2.part2 input
    Assert.Equal(12, pt2)
