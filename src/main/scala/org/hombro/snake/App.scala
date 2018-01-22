package org.hombro.snake

import org.scalajs.dom
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.timers.SetIntervalHandle

object App {
  val length: Int = 5
  val timeout: Int = 100
  val keys = mutable.Set.empty[Direction]
  var interval: Option[SetIntervalHandle] = None // need to reference the interval handler from within the loop block

  def main(args: Array[String]): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    dom.window.onkeydown = { (event: dom.KeyboardEvent) =>
      keyDirection(event.keyCode) match {
        case Some(direction) =>
          keys.add(direction)
          event.preventDefault()
        case None =>
      }
    }
    dom.window.onkeyup = { (event: dom.KeyboardEvent) =>
      keyDirection(event.keyCode) match {
        case Some(direction) =>
          keys.remove(direction)
          event.preventDefault()
        case None =>
      }
    }
    val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    val Array(cw, ch) = Array(dom.window.innerWidth, dom.window.innerHeight)
      .map(l => (.95 * l).toInt)
      .map(r => r - (r % length))
    canvas.width = cw
    canvas.height = ch
    dom.document.body.appendChild(canvas)
    gameStart(canvas, context)
  }

  private def inflate(cell: Cell) = {
    (cell._1 * length, cell._2 * length)
  }

  private def drawCell(cell: Cell, canvas: HTMLCanvasElement, context: CanvasRenderingContext2D, color: Color): Unit = {
    context.fillStyle = color
    val inflated = inflate(cell)
    context.fillRect(inflated._1, inflated._2, length, length)
  }

  private def drawBorder(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D): Unit = {
    context.fillStyle = BORDER
    context.fillRect(0, 0, canvas.width, length)
    context.fillRect(0, canvas.height - length, canvas.width, canvas.height)
    context.fillRect(0, 0, length, canvas.height)
    context.fillRect(canvas.width - length, 0, canvas.width, canvas.height)
  }

  private def clear(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D): Unit = {
    context.clearRect(length, length, canvas.width - length - length, canvas.height - length - length)
  }

  private def gameStart(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D): Unit = {
    val snake = Snake(canvas.width / length, canvas.height / length)
    drawBorder(canvas, context)

    interval = Some(js.timers.setInterval(timeout) {
      if (keys.size == 1)
        snake.turn(keys.head)
      snake.move()

      if (!snake.isAlive) {
        dom.window.alert(s"Game over with a final score of ${snake.score}")
        interval.foreach(i => js.timers.clearInterval(i))
      }
      else {
        clear(canvas, context)
        snake.body.foreach(c => drawCell(c, canvas, context, BODY))
        drawCell(snake.food, canvas, context, FOOD)
      }
    })
  }
}
