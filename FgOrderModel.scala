package fatgateway

import java.time.Instant
import java.util.UUID

import common.LimitOrder

/**
  * Created by Grim on 07/01/2018.
  */
object FgOrderModel {
  case class Execution(baseChange : BigDecimal, counterChange : BigDecimal)

  /**
    * Represents an order that has been sent to the server
    * @param ourId Internally assigned UUID
    * @param timeSent Time the order was sent
    * @param order The limit order that was sent (price qty etc)
    * @param execs Executions associated with this order
    * @param state Our knowledge of this order's state
    */
  case class FgOrder(ourId : UUID,
                     timeSent : Instant,
                     order : LimitOrder,
                     execs : Vector[Execution],
                     state : FgOrderState) {
    /**
      * Total executions so far, in base
      */
    val baseExec : BigDecimal = execs.map(_.baseChange).sum

    /**
      * Gives lower and upper bound for the impact this order will have on our position (in base)
      * @return
      */
    def baseBounds : (BigDecimal,BigDecimal) = state match {
      case Sent | Accepted(_) | Open(_) => (order.baseAmount min baseExec, order.baseAmount max baseExec)
      case Closed(_) | Rejected(_)      => (baseExec,baseExec)
    }
  }

  sealed trait FgOrderState
  case object Sent extends FgOrderState
  case class Accepted(clid : UUID) extends FgOrderState
  case class Rejected(reason : String) extends FgOrderState
  case class Open(clid : UUID) extends FgOrderState
  case class Closed(clid : UUID) extends FgOrderState

  object FgOrder {
    /**
      * Orders start life here...
      * @param timeSent
      * @param order
      * @return
      */
    def createSent(timeSent: Instant, order : LimitOrder) : FgOrder = FgOrder(
      ourId = UUID.randomUUID(),
      timeSent = timeSent,
      order = order,
      execs = Vector[Execution](),
      state = Sent
    )
  }
}
