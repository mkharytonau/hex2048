package hex2048

import cats.effect._
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HashChangeEvent}

import org.scalajs.dom.{Event, console, document, window}

import scala.util.{Success, Try}

object Hex2048App extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    window.addEventListener(
      "load",
      (_: Event) => {
        window.location.hash match {
          case s"#test$radius" if validGameRadius(radius) =>
            val selectedRadius = document.getElementById("gameRadius").asInstanceOf[HTMLInputElement]
            selectedRadius.value = radius.toInt.toString
            game(radius.toInt).unsafeRunAsync {
              case Left(error) => console.log(error)
              case Right(_) =>
            }
          case _ => console.error("Selected radius is not supported, " +
            "please specify a value from [2; 20] range.")
        }
      },
    )

    document.getElementById("playButton").addEventListener("click", (e: Event) => {
      val selectedRadius = document.getElementById("gameRadius").asInstanceOf[HTMLInputElement].value.toInt
      window.location.hash = s"#test$selectedRadius"
    })

    window.addEventListener("hashchange", (e: HashChangeEvent) => {
      window.location.reload()
    })

    IO.pure(ExitCode.Success)
  }

  def validGameRadius(str: String): Boolean = {
    Try(str.toInt) match {
      case Success(value) if value >= 2 && value <= 20 => true
      case _ => false
    }
  }

  def game(radius: Int): IO[Unit] = {
    for {
      gameOrError <- Hex2048Game
        .of[IO](
          config = Hex2048Game.Config(
            radius = radius,
          ),
          rngService = new DataFromServer[IO](
            serverHost = "127.0.0.1",
            serverPort = 13337,
          ),
          htmlRenderer = new Hex2048HtmlRenderer(
            htmlContainerId = "game-container",
            gameXc = window.innerWidth / 2,
            gameYc = window.innerHeight / 2,
            radius = (window.innerHeight * 0.75 / ((2 * radius - 1) * 2) / Hex2048HtmlRenderer.Constants.sin60).toInt,
          ),
        )
        .value
      _ <- gameOrError match {
        case Left(error) => IO(console.error(s"Unable to create game: error: $error"))
        case Right(game) =>
          IO {
            game.draw().unsafeRunAsync(_ => ())
            document.addEventListener(
              "keydown",
              (e: dom.KeyboardEvent) =>
                game.handleKeyPress(e.key).value.unsafeRunAsync {
                  case Left(error) => console.error(s"Error while running IO, error: $error")
                  case Right(result) =>
                    result match {
                      case Left(error) => console.error(s"Handle key(${e.key}) press was unsuccessful: error: $error")
                      case Right(_) => console.log(s"Handle key press was successful")
                    }
                },
            )
          }
      }
    } yield ()
  }
}
