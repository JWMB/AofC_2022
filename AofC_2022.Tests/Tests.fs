module Tests

open System
open Xunit

[<Fact>]
let ``template`` () =
    let input = """
"""
    let pt1 = Template.part1 input
    Assert.Equal(0, pt1)
    
    let pt2 = Template.part2 input
    Assert.Equal(0, pt2)


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

[<Fact>]
let ``D3`` () =
    let input = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""
    let pt1 = D3.part1 input
    Assert.Equal(157, pt1)    
    
    let pt2 = D3.part2 input
    Assert.Equal(70, pt2)


[<Fact>]
let ``D4`` () =
    let input = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""
    let pt1 = D4.part1 input
    Assert.Equal(2, pt1)
    
    let pt2 = D4.part2 input
    Assert.Equal(4, pt2)


[<Fact>]
let ``D5`` () =
    let input = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""
    let pt1 = D5.part1 input
    Assert.Equal("CMZ", pt1)
    
    let pt2 = D5.part2 input
    Assert.Equal("MCD", pt2)
