package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level0 extends SolutionChecker {
    /* terrain for level 0*/
    
    val level =
      """ST
          |oo
          |oo""".stripMargin
  }
  
  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("level 1 stands at start") {
    new Level1 {
      assert(startBlock.isStanding)
    }
  }

  test("level 1 doesn't stand one step afer start") {
    new Level1 {
      assert(!startBlock.right.isStanding)
      assert(!startBlock.down.isStanding)
    }
  }

  test("level 1 stands 2 steps afer start") {
    new Level1 {
      assert(startBlock.right.right.isStanding)
    }
  }

  test("level 1 left and up after start aren't legal, but down and right are") {
    new Level1 {
      assert(!startBlock.left.isLegal, "left is not legal")
      assert(!startBlock.up.isLegal, "up is not legal")
      assert(startBlock.right.isLegal, "right is legal")
      assert(startBlock.down.isLegal, "down is legal")
    }
  }

  test("level 1 legalNeighbors") {
    new Level1 {
      val legal = startBlock.legalNeighbors
      assert(legal.toSet === Set((startBlock.right, Right), (startBlock.down, Down)))
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(!terrain(Pos(-1,-1)), "-1,-1")
    }
  }

  test("findChar") {
    val vector = Vector(Vector('S', 'o'), Vector('T', 'o'), Vector('o', 'o'))
    new Level1 {
      assert(findChar('S', vector) === Pos(0, 0))
      assert(findChar('T', vector) === Pos(1, 0))
      assert(findChar('o', vector) === Pos(0, 1))
    }
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("isStanding") {
    new Level1 {
      val b = Block(Pos(1,3), Pos(1,4))
      assert( !b.isStanding )
    }
  }
 
  test("isLegal") {
    new Level1 {
      val b1 = Block(Pos(0,0), Pos(0,1))
      val b2 = Block(Pos(2,0), Pos(3,0))
      assert(b1.isLegal)
      assert(!b2.isLegal)
    }
  }

  test("terrainFunction") {
    val vector = Vector(Vector('S','o'), Vector('T','o'), Vector('o','o'))
    new Level1 {
        assert(terrainFunction(vector)(Pos(-1,0)) === false)
        assert(terrainFunction(vector)(Pos(0,2)) === false)
        assert(terrainFunction(vector)(Pos(3,1)) === false)
        assert(terrainFunction(vector)(Pos(3,1)) === false)
        assert(terrainFunction(vector)(Pos(1,1)) === true)
    }
  }

  test("moves") {
    new Level1 {
      val startingBlock = Block(Pos(1, 1), Pos(1, 1))
      assert(startBlock.right == Block(Pos(1, 2), Pos(1, 3)))
    }
  }

  test("neighbors") {
    new Level1 {

      val leftRightUpDown = List(
        (startBlock.left, Left),
        (startBlock.right, Right),
        (startBlock.up, Up),
        (startBlock.down, Down))
      val rightDown = List(
        (startBlock.right, Right),
        (startBlock.down, Down))

      assert(startBlock.neighbors === leftRightUpDown)
      assert(startBlock.legalNeighbors === rightDown)

      val startingBlock = Block(Pos(1, 1), Pos(1, 1))
      val safeBlock = Block(Pos(1, 1), Pos(1, 2))
      
      assert(startingBlock.legalNeighbors.toSet ===
        Set(
          (Block(Pos(2, 1), Pos(3, 1)), Down),
          (Block(Pos(1, 2), Pos(1, 3)), Right)))
      assert(safeBlock.neighbors.toSet ===
        Set(
          (Block(Pos(0, 1), Pos(0, 2)), Up),
          (Block(Pos(2, 1), Pos(2, 2)), Down),
          (Block(Pos(1, 0), Pos(1, 0)), Left),
          (Block(Pos(1, 3), Pos(1, 3)), Right)))
    }
  }  

  test("legalNeighbors") {
    new Level1 {
      val almostSafeBlock = Block(Pos(2, 2), Pos(2, 2))
      assert(almostSafeBlock.legalNeighbors.toSet ===
        Set(
          (Block(Pos(0, 2), Pos(1, 2)), Up),
          (Block(Pos(2, 0), Pos(2, 1)), Left),
          (Block(Pos(2, 3), Pos(2, 4)), Right)))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet ===
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))

      assert(neighborsWithHistory(Block(Pos(1, 2), Pos(1, 3)), List(Right)).toSet ===
        Set(
          (Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)),
          (Block(Pos(1, 1), Pos(1, 1)), List(Left, Right)),
          (Block(Pos(2, 2), Pos(2, 3)), List(Down, Right))))
    }
  }
  
  test("newNeighborsOnly") {
    new Level1 {
      val somePath = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
      val someHistory = Set(
        Block(Pos(1, 2), Pos(1, 3)),
        Block(Pos(1, 1), Pos(1, 1)))

      assert(newNeighborsOnly(somePath, someHistory) === 
        Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("pathsFromStart") {
    new Level0 {
      val path = pathsFromStart
      assert(path.nonEmpty)
    }
    new Level1 {
      assert(pathsFromStart.take(11).toList ===
        from(Stream((Block(startPos, startPos), Nil)), Set(Block(startPos, startPos))).take(11).toList)
    }
  }

  test("level 0 pathsToGoal") {
    new Level0 {
      val (actualEndPosition, actualPath) = pathsToGoal.head
      val expectedEndPosition = Block(Pos(0, 1), Pos(0, 1))
      val expectedPath = List(Up, Right, Down)
      assert(actualEndPosition == expectedEndPosition)
      assert(actualPath == expectedPath)
    }
  }
  
  test("level 0 solution") {
    new Level0 {
      assert(solution == List(Down, Right, Up))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
 
}
