package org.hombro.snake

import scala.util.Random

private class SnakeState(val score: Int, val food: Cell, val direction: Direction, val parts: List[Cell]) {

  private def random(pairs: Set[Cell]): Cell = {
    Random.shuffle(pairs.diff(Set(food) ++ parts).toList).head
  }

  def eat(pairs: Set[Cell]): SnakeState = new SnakeState(score + 1, random(pairs), direction, List(food) ++ parts)

  def move(next: Cell): SnakeState = new SnakeState(score, food, direction, List(next) ++ parts.slice(0, parts.size - 1))

  def turn(d: Direction): SnakeState = if (d.opposite() != direction) new SnakeState(score, food, d, parts) else this
}

case class Snake(private val width: Int, private val height: Int) {
  private var state = new SnakeState(0, (5, 2), East(), List((3, 1), (2, 1), (1, 1)))

  val border: List[Cell] = List(
    List.tabulate(width)(n => List((n, 0), (n, height))),
    List.tabulate(height)(n => List((0, n), (width, height))),
  ).flatMap(l => l.flatten)

  private val pairs = for {
    i <- 1 until width - 1
    j <- 1 until height - 1
  } yield {
    (i, j)
  }

  private def outOfBounds() = {
    val h = state.parts.head
    h._1 <= 0 || h._1 >= width - 1 || h._2 <= 0 || h._2 >= height - 1
  }

  def body: Set[Cell] = state.parts.toSet

  def score: Int = state.score

  def turn(d: Direction): Unit = {
    state = state.turn(d)
  }

  def food: Cell = state.food

  def isAlive: Boolean = {
    val h = state.parts.head
    !outOfBounds() && !state.parts.slice(1, state.parts.size).contains(h)
  }

  def move(): Unit = {
    val head = state.parts.head

    val next = state.direction match {
      case North() => (head._1, head._2 - 1)
      case South() => (head._1, head._2 + 1)
      case East() => (head._1 + 1, head._2)
      case West() => (head._1 - 1, head._2)
    }

    state = if (next.equals(food)) {
      state.eat(pairs.toSet)
    }
    else {
      state.move(next)
    }
  }
}