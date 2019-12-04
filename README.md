# maze-solver

This is a simple maze solver that uses the A* algorithm to solve mazes in a png format.

## Example

Given 
![Unsolved maze](img/big-maze.png)
it will produce the following picture
![Solved maze](img/big-maze-solved.png)

The format is the same as used by [@mazeaday](https://twitter.com/mazeaday) on Twitter.

## Usage

Build and run the program with

    stack run -- img/big-maze.png img/big-maze-solved.png

## Future work

- It is currently pretty slow on large images. ~30s on the example above.
- I am planning to make fetch mazes from @mazeaday and publish the solution automatically
- Add an option to avoid walls so the output looks nicer.
- Make the solution line thicker so you can see it