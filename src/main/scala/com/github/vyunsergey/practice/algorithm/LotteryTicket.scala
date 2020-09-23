package com.github.vyunsergey.practice.algorithm

object LotteryTicket {
  def main(args: Array[String]): Unit = {
    val secret: Secret = Secret(Console.in.readLine())
    val secretNum: Int = 3
    val numTickets: Int = Console.in.readLine().toInt
    val tickets: List[Ticket] = 0.until(numTickets).toList.map(_ => Ticket(Console.in.readLine()))
    tickets.map(lucky(secret)(secretNum)(_)).foreach { l =>
      if (l) println("Lucky") else println("Unlucky")
    }
  }

  def lucky(secret: Secret)(number: Int)(ticket: Ticket): Boolean = {
    ticket.contains(secret) >= number
  }

  case class Secret(value: List[Int])
  object Secret {
    def apply(str: String): Secret = new Secret(
      str.split(" ").map(_.toInt).toList
    )
  }

  case class Ticket(value: List[Int]) {
    def contains(secret: Secret): Int = {
      secret.value.map(this.value.contains(_)).map(if (_) 1 else 0).sum
    }
  }
  object Ticket {
    def apply(str: String): Ticket = new Ticket(
      str.split(" ").map(_.toInt).toList
    )
  }
}
