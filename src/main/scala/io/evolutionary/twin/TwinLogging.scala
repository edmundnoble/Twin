package io.evolutionary.twin

import org.log4s._

trait TwinLogging { self =>
  val logger = org.log4s.getLogger(self.getClass)
}
