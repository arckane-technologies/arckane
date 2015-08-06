package decision

object SimpleLogarithmicCommunity {

  /** In a Simple Logarithmic Community generations of members are formed
    * by the levels of a breath first search insertion binary tree, so the
    * first generation is made of 1 member, de seconds of 2, the third of 4,
    * etc. All life and commitment decisions are made when at least the equivalent
    * of 1 generation agrees, to achieve this every member of a generation
    * is given 1.0/gs influence points where gs is the generation size.
    */
  implicit val simpleLogarithmicCommunity = new DecisionSystem {

    def lifeRatio (crowdSize: Int, influence: Double): Double =
      1.0

    def lifeModifier (life: Double, influence: Double): Double =
      influence

    def commitRatio (crowdSize: Int, influence: Double): Double =
      1.0

    def commitModifier (commit: Double, influence: Double): Double =
      influence

    def influenceRatio (crowdSize: Int): Double =
      logarithmicCommunity(2)(crowdSize)

    def influenceModifier (targetsInfluence: Double, votersInfluence: Double): Double =
      votersInfluence
  }
}

trait DecisionSystem {

  import scala.math._

  def lifeRatio (crowdSize: Int, influence: Double): Double

  def lifeModifier (life: Double, influence: Double): Double

  def commitRatio (crowdSize: Int, influence: Double): Double

  def commitModifier (commit: Double, influence: Double): Double

  def influenceRatio (crowdSize: Int): Double

  def influenceModifier (targetsInfluence: Double, votersInfluence: Double): Double

  /** Utils
    */
  def logBofX(b: Int, x: Double): Double = log(x) / log(b)

  /**
    * logaritmicCommunity is a function designed to give total decision power to the first
    * base minus one members, the half to the next log base, etc: for example:
    *
    * logCommunity(2)(0) = 1.0
    * logCommunity(2)(1) = 0.5
    * logCommunity(2)(2) = 0.5
    * logCommunity(2)(3) = 0.25
    *         ...
    * logCommunity(2)(6) = 0.25
    * logCommunity(2)(7) = 0.125
    *         ...
    * logCommunity(2)(15) = 0.125
    *
    * An other example:
    *
    * logCommunity(10)(0) = 1.0
    *         ...
    * logCommunity(10)(9) = 1.0
    * logCommunity(10)(10) = 0.5
    *         ...
    * logCommunity(10)(99) = 0.5
    * logCommunity(10)(100) = 0.25
    *         ...
    * logCommunity(10)(999) = 0.25
    *
    * The second argument is the crowd size before the integration of the next member.
    */
  def logarithmicCommunity(b: Int) = (1 + (_:Double)) andThen (logBofX(b, _:Double)) andThen floor _ andThen (pow(2, _:Double)) andThen (1 / (_:Double))
}
