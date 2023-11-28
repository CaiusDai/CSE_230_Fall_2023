# A Sokoban Clone Implementation in Haskell

CSE 230 Project Proposal



## Team members

Yunbo Lu (PID: A59017356) yul229@ucsd.edu

Leilong Fan (PID: A59024032) lefan@ucsd.edu

Zijie Dai (PID: A59024938) z2dai@ucsd.edu

Hongbei Liu (PID: A59018257) hol033@ucsd.edu



## Project Overview

We're developing a command-line version of Sokoban using Haskell and the Brick library. In this puzzle, the player moves boxes to designated spots within a walled warehouse. Winning requires all boxes to be correctly placed. Here's a conceptual level:

```
#############
#...........#
#..####.....#
#..#..#.....#
#..#..#.....#
#..#$.#.....#
#..#..@.....#
#..#..####..#
#........#..#
#..###.###..#
#.....*.....#
#############
```

- `#` represents walls.
- `.` represents empty space.
- `$` represents a crate.
- `@` represents the player.
- `*` represents the target.

Move the crate (`$`) onto the target (`*`) to win the game.


## Expected Milestones

Though it is a very classic game, we will try to add some special components into the game. Ideally, we will manage to develop the game in different stages:

### Basic Version

The basic version will adhere to traditional Sokoban rules. We plan to develop a multi-level clone of the game in Haskell, utilizing the Brick library. This will enable users to navigate and interact within the game, consistent with the established Sokoban gameplay.

### Advanced Features

After finishing the most basic part, we will try to add the following features into the game to make it more interesting:

Subsequent versions will introduce:

- Tools (bombs, strength potions) for new strategies, like wall destruction.
- An automatic map generator that can generate different maps while ensuring the produced map is solvable.
- A challenge mode with step limits.
- Networked racing Sokoban
- ...

## Components

Currently we have identified the following crucial components of the project:

- Character movement events
- Collision Detection
- Box pushing interaction
- Game state detection (If the game has ended or not)
