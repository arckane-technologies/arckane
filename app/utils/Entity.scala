package utils
import utils.DatabaseOps.Node

trait Entity {
  val url: String
  val node: Node
}

trait EntityProps
