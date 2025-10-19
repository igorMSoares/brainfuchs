[bounce.b -- particle automaton
(c) 2025 Daniel B. Cristofani
https://brainfuck.org/
This program is licensed under a Creative Commons Attribution-ShareAlike 4.0
International License (http://creativecommons.org/licenses/by-sa/4.0/).]

+[
  ->>>+>>>>,----------[
    >++++[<----->-]+<--[
      +<++++[>---->++++<<-]>[
        >----[<->-]++<[
          >+<--[
            >++[>+++++<-]>[<<->++>-]<-<-[
              <++++[>->----<<-]>[
                >[-]+++++<--[
                  <++++[>------>+<<-]>[
                    ->[-]+<
                  ]]]]]]]]
  ]>[<]<<<[<]<
]<[
  [-]<[
    [
      >[>>>>>>+<<<<<<-]<-
      [[>]+<[-<]>]>>>>>[
        -<[>+<-]<[>+<-]<[>+<-]>>[<<+>>-]<<<[>>>+<<<-]>>>>[<<<<+>>>>-]>>>++<<<
      ]>[
        -<<[>>+<<-]<[>+<-]<<[>>+<<-]>>>[<<<+>>>-]<<[>>+<<-]>>>>[<<<<+>>>>-]>>++++<<
      ]>>[<++++++++>-]<+<<<[
        -<<<<<<<[<<<]>>>[<+[>>>+<<<-]>>>>]
        <[>>>>>>>>[>>>]>+<<<<[<<<]<<<<<-]
        >>>>>>>>[>>>]>>>[
          <<+[-[>>>+<<<-]>>[>>]<]<[++++++++<<]>>>[-]<<<<[<<<]
        ]<<[-]<<<<[<<<]
      ]<[
        -<<<<<<[<+[<<<+>>>-]<<]<<<[
          [>>[<<<+>>>-]<<<<<]
          >>+[-[>>>+<<<-]>[>]>]<<[>++++<<]>>>[-]<<<<<[<<<]
        ]>>[-]>>>>[>>>]>>>[>>>]>>>
      ]<[>>>>>>>>[++<<+>]<[->]<<<<<<<-]<[<<<<[>+>]<[<<]>>>>>-]<<<<
    ]<<<
  ]>>>>>>>>>>>>>[
    [
      [<<<<<<<+>>>>>+>>-]++++[<++++++++>-]<<-[
        >[>+<-]>--[<++>-]<<-[
          >++<-[
            >[>+<--]>[<+++>-]<+<--[
              <++++[>->++++++<<-]>[
                >>++++++++[<<->--------->-]<+<[
                  >+[<->>+++<---]>--[<++>-]<<[
                    <++++[>---->-<<-]>[
                      [-]>[<+>--]<--[>+<-]]]]]]]]
      ]>.[-]>>>>
    ]++++++++++.[-]>>>
  ]<<<<<<<<<<<<,
]

[This is a...particle automaton? You give it an initial configuration,
then you hit ENTER repeatedly to advance it one step each time. Holding
down ENTER may or may not be unsatisfyingly fast, depending on your
choice of pattern.

Initial configurations can have particles (< > ^ or v to note direction
of movement), \ or / which are fixed walls for particles to bounce off,
X which is a combination of \ and / (and reverses movement by 180 as you
might expect), space which is empty space for particles to move through.

Particles pass through each other, though in most cases the effect is
arguably the same as it should be if they had bounced off each other.

When two particles, or one or more particles and a wall, are in the same
space, it's shown as *. This is not accepted as initial input, but it
would be pretty easy to write a replacement input routine that would
accept overlapping objects in an initial configuration, it'd just also
be somewhat harder to use.

Particles that go off the edge of the configuration should be absorbed
neatly. This includes if they try to move upward or downward to a
shorter line that doesn't have enough trailing spaces explicitly typed.
After any particles have escaped that are going to, the behavior is
provably cyclical (and reversible).

This is in brainfuck; it should run on any command-line interpreter or
any interpreter that does interactive i/o gracefully.

A few sample patterns:

      X
     /v\     
    /   \
   /     \
  /       \
 /         \
/           X
\          /
 \        /
  \      /
   \    /
    \  /
     \/
(All the interior spaces are part of one long path)

/    >    \
 /   >   \ 
  /  >  \  
   / > \   
    />\    
     X            
    \ /    
   \   /    
  \     /  
 \       / 
\         /
This should resynchronize every 480 steps.

    /   >  \
   /     >  \
  / >>>>>>>  \
 /       >    \
/    >  >      \
                
                
                
                
                
                
\              /
 \            /
  \          /
   \        /
    \      /
Little demonstration of reflection.

      /\                  
     /\ \                   
    /  \ \                  
   / /\ \ \                   
   \ \^ / /                 
    \ \/ /
     \  /        
      \/


/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
X                                                       X
X  ^^^vv>^>>v^^v>  ^v  ^^      v>  v>  v^>v^>^^^>v>^>   X
X  >>          >^  <^^^v<          v<  v<          ^<   X
X  <v  <><v^^  <v  v<  ^>    >v    <^  v^  v^^<v>  ^v   X
X  <<  >^^<>>  vv    ^>^>>vv>^v  ^><v  ^>  v<<^>^  ^<   X
X  >v  ^v<v^^  ^^  >v  ^<^^  v<^v  <>  vv  ^v^^>^  ^>   X
X  >>          ^^        ^><>>>><      >>          >>   X
X  v>^^^^^<><<<vv  v^  >v  <>  <^  <^  <vvv^>^^>^>^<^   X
X                    <^    <^>>  ^v                     X
X  >v    ^>^<>>v><^>^><>^  ^^  >^<<>v<v    <>  >^v>v^   X
X    v<>^  v^      <^^<  ^<      <v  <<  >>vv^>>^^<     X
X    ^^>><<  >^^<>^v<  >><><>^^>^<<>v><<v    v<    v<   X
X  v>v<v<<<  vv  ^<^>  <<  v^        ^>      v><v>^<v   X
X  ^^  vv    v>^>v>v>      v>    v^    ><>v        ^^   X
X  >v    v^      <v^^  <^    ^<^><v^v      vv    ^>     X
X  <v>v  >v<v  ^<<><>  <^^>^>  >><vv^  <v  >><^^<>^>^   X
X  ^<      ^<        >^vvv>      <>  ^v^<<>  v^^^  ^v   X
X  v>      v>  ^<  >^^<><      <v>>^><<<<>v><  <<^<     X
X                  <>^>  ^>^<<>    <v      <<  v<v^     X
X  >v^^<>><^><<<<  v>  <<^>        ^<  <>  >v      >^   X
X  ^>          ^v  <v  v^  ^^  >>>>v<      ^>    <vv^   X
X  >^  vvvvv<  ^<  ^^>vv<  >>>^^<<^<<vvv^<^^>    ^<>^   X
X  v>  ^vv<<<  ^>  ^^          <v    <>><        ^^vv   X
X  ^v  v>><>>  v>    vv<>  >^  ^>  <>v<    >><>v>^<<<   X
X  v^          <<              >^    vv  ^<>v  v^v^v^   X
X  >^v^vv^v^^<<<^  ^vv<>v    >>vv  ^^  <v    <v    ><   X
X                                                       X
\XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX/
]
