package com.github.vyunsergey

import org.scalatest.flatspec.AnyFlatSpec

class MainTest extends AnyFlatSpec {

  "main hello" should "greeting" in {
    Main.hello()
  }

}
