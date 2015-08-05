package decision

trait DecisionSystem {

  def lifeRation (crowdSize: Int, influence: Double): Double

  def lifeModifier (life: Double, influence: Double): Double

  def commitRatio (crowdSize: Int, influence: Double): Double

  def commitModifier (life: Double, influence: Double): Double

  def influenceRatio (crowdSize: Int): Double

  def influenceModifier (targetsInfluence: Double, votersInfluence: Double): Double
}
