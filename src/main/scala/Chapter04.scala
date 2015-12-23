package redbook

object Chapter04 {
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(_) => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(x) if (f(x)) => this
      case Some(x) if (! f(x)) => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
