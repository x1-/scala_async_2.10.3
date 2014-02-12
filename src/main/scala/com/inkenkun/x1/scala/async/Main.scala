package com.inkenkun.x1.scala.async

import scala.collection.parallel.availableProcessors
import concurrent.{ Await, future, Future }
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration._
import scala.util.{Failure, Success}

/**
 * scala2.10から導入されたFutureとPromiseのサンプル・ソースです。
 *
 * 1〜100までの数字の中から任意の6個を選びます。
 * 6個の数値を足した合計が奇数か偶数か判定します。
 */
object Main extends App {

  override def main( args: Array[String] ): Unit = {

    // 1. Futureの基本的な使い方
    standard1


  }

  /**
   * 1. Futureの基本的な使い方
   * 1〜100までの数字の中から任意の5個を選ぶ。
   * 5個の数値を足した合計が奇数か偶数か判定し、ひたすらprintする。
   * 戻り値は偶数の数。
   * ・・という処理を非同期で行います。
   *
  before await:283 sec
c:1,2,3,4,5, isEven:false
c:1,2,3,4,6, isEven:true
   :
c:95,97,98,99,100, isEven:false
c:96,97,98,99,100, isEven:true
Await.result:37643760
even number: 37643760
onComplete:699375 sec
   */
  def standard1: Unit = {

    val stime = System.currentTimeMillis()

    val fs = future {
      ( 1 to 100 ).combinations( 5 ).foldLeft( 0 )( (n, c) => {
        val sum = c.sum
        val isEven = sum % 2 == 0
        println( s"c:${c.mkString(",")}, isEven:${isEven}" )
        if ( isEven ) n + 1 else n
      })
    }

    fs onComplete {
      case Failure(e) => println( s"error: ${e.getMessage}" )
      case Success(r) => println( s"even number: ${r}" )

      println( s"onComplete:${System.currentTimeMillis()-stime} sec" )
    }

    println( s"before await:${System.currentTimeMillis()-stime} sec" )

    val res = Await.result( fs, Duration.Inf )
    println( s"Await.result:${res}" )
  }
}
