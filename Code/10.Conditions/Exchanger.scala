package tacp.monitors

import ox.scl._

class MonitorExchanger[A] extends tacp.clientServer.ExchangerT[A]{
  def exchange(x: A): A = ???

  def shutdown() = {}

}
