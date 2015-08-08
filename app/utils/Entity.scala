package utils
import utils.DatabaseOps.Node

trait Entity {
  val id: Int
  val node: Node
}
