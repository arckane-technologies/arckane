package decision

object DecisionSystem {

  implicit val decisionSystem = new DecisionSystem {

    def lifeRatio (crowdSize: Int, influence: Double): Double =
      2.0

    def lifeModifier (life: Double, influence: Double): Double =
      0.1

    def commitRatio (crowdSize: Int, influence: Double): Double =
      2.0

    def commitModifier (life: Double, influence: Double): Double =
      0.1

    def influenceRatio (crowdSize: Int): Double =
      2.0

    def influenceModifier (targetsInfluence: Double, votersInfluence: Double): Double =
      0.1
  }
}

trait DecisionSystem {

  def lifeRatio (crowdSize: Int, influence: Double): Double

  def lifeModifier (life: Double, influence: Double): Double

  def commitRatio (crowdSize: Int, influence: Double): Double

  def commitModifier (life: Double, influence: Double): Double

  def influenceRatio (crowdSize: Int): Double

  def influenceModifier (targetsInfluence: Double, votersInfluence: Double): Double
}
