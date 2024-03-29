module Tests

open System
open Xunit

[<Fact>]
let ``template`` () =
    let input = """
"""
    Assert.Equal(0, Template.part1 input)
    Assert.Equal(0, Template.part2 input)


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


[<Fact>]
let ``D6`` () =
    let input = """
mjqjpqmgbljsphdztnvjfqwrcgsmlb
"""
    Assert.Equal(7, D6.part1 input)
    Assert.Equal(6, D6.part1 "nppdvjthqldpwncqszvftbrmjlhg")
    Assert.Equal(10, D6.part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
    Assert.Equal(11, D6.part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
    
    Assert.Equal(26, D6.part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")


[<Fact>]
let ``D7`` () =
    let input = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""
//    let input = """
//$ cd /
//$ ls
//dir a
//dir b
//100 c.txt
//$ cd a
//$ ls
//dir e
//200 f.txt
//$ cd e
//$ ls
//400 i.txt
//"""
    Assert.Equal(95437, D7.part1 input)
    Assert.Equal(24933642, D7.part2 input)


[<Fact>]
let ``D8`` () =
    let input = """
30373
25512
65332
33549
35390
"""
    Assert.Equal(21, D8.part1 input)
    Assert.Equal(8, D8.part2 input)


[<Fact>]
let ``D9`` () =
    let input = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""
    Assert.Equal(13, D9.part1 input)

    let input = """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""
    Assert.Equal(36, D9.part2 input)


[<Fact>]
let ``D10`` () =
    let input = """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""
    Assert.Equal(13140, D10.part1 input)

    let image = """
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"""
    Assert.Equal(Tools.Parsing.cleanWithTrimEmptyLines image, D10.part2 input)




[<Fact>]
let ``D11`` () =
    let input = """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""
    Assert.Equal(10605I, D11.part1 input)
    Assert.Equal(2713310158I, D11.part2 input)




[<Fact>]
let ``D12`` () =
    let input = """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""
    Assert.Equal(31, D12.part1 input)
    Assert.Equal(29, D12.part2 input)




[<Fact>]
let ``D13`` () =
    let input = """
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"""
//    let input = """
//[[1],[2,3,4]]
//[[1],4]
//"""

    Assert.Equal(13, D13.part1 input)
    Assert.Equal(140, D13.part2 input)


[<Fact>]
let ``D14`` () =
    let input = """
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"""
    Assert.Equal(24, D14.part1 input)
    Assert.Equal(93, D14.part2 input)


[<Fact>]
let ``D15`` () =
    let input = """
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"""

    Assert.Equal(26, D15.part1WithY input 10)
    Assert.Equal(56000011, D15.part2WithMinMax input 0 20 )


[<Fact>]
let ``D16`` () =
    let input = """
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""
    Assert.Equal(1651, D16.part1 input)
    Assert.Equal(1707, D16.part2 input)


[<Fact>]
let ``D17`` () =
    let input = """
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
"""
    Assert.Equal(0, D17.part1 input)
    Assert.Equal(0, D17.part2 input)


[<Fact>]
let ``D18`` () =
    let input = """
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"""
    Assert.Equal(64, D18.part1 input)
    Assert.Equal(0, D18.part2 input)


[<Fact>]
let ``D19`` () =
    let input = """
"""
    Assert.Equal(0, D19.part1 input)
    Assert.Equal(0, D19.part2 input)

