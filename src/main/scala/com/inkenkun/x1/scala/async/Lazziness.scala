package com.inkenkun.x1.scala.async

class Lazziness extends LazzinessTrait {
  def method = {
    println( "Lazziness.method" )
    lazzillyPrint
  }
}

trait LazzinessTrait extends DependTrait {
  val first = 5;
  lazy val counter: Int = { println( "called,LazzinessTrait.counter!" ); first * dep }

  def lazzillyPrint: Unit = {
    println( "LazzinessTrait.lazzillyPrint!" )
    println( s"counter:${counter}")
  };
}

trait DependTrait {
  def dep: Int = 10;
}
