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
//    standard1
    // 2. 見通しの悪いコールバックチェイン。
    callback1

  }

  /**
   * 1. Futureの基本的な使い方
   *  1〜100までの数字の中から任意の5個を選ぶ。
   *  5個の数値を足した合計が奇数か偶数か判定し、ひたすらprintする。
   *  戻り値は偶数の数。
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

    // Futureを返すfutureメソッドを呼び出します。
    val fs = future {
      ( 1 to 100 ).combinations( 5 ).foldLeft( 0 )( (n, c) => {
        //println( s"c:${c.mkString(",")}, isEven:${c.sum % 2 == 0}" )
        if ( c.sum % 2 == 0 ) n + 1 else n
      })
    }

    // Futureの完了。Futureは値もしくは例外を持つとき、完了したことになります。
    fs onComplete {
      case Failure(e) => println( s"error: ${e.getMessage}" )
      case Success(r) => println( s"even number: ${r}" )

      println( s"onComplete:${System.currentTimeMillis()-stime} sec" )
    }

    // 下記のように書き直すこともできます。
//    fs onSuccess {
//      case r => println( s"even number: ${r}" )
//    }
//    fs onFailure {
//      case e => println( s"error: ${e.getMessage}" )
//    }

    println( s"before await:${System.currentTimeMillis()-stime} sec" )

    val res = Await.result( fs, Duration.Inf )
    println( s"Await.result:${res}" )
  }

  /**
   * 2. 見通しの悪いコールバックチェイン。
   *  2-1. 1〜100までの数字の中から任意の5個を選ぶ。
   *       5個の数値を足した合計が奇数か偶数か判定し、偶数の数を数える。
   * ↑ここまでが非同期で行う1番目のブロック
   *  2-2.最終的に算出された偶数の値を割り切れなくなるまで2で割り、割った回数を返却する。
   * ↑ここまでが非同期で行う2番目のブロック
   * ・・という処理を非同期で行います。
   *
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
  def callback1: Unit = {

    val stime = System.currentTimeMillis()

    // 2-1の処理を行うFuture。
    val f21 = future {
      ( 1 to 20 ).combinations( 3 ).foldLeft( 0 )( (n, c) => {
        val isEven = c.sum % 2 == 0
        //println( s"c:${c.mkString(",")}, isEven:${isEven}" )
        if ( isEven ) n + 1 else n
//        if ( c.sum % 2 == 0 ) n + 1 else n
      })
    }

    // 2-1が失敗したら2-2の処理は行いません
    f21 onFailure {
      case e => println( s"error: ${e.getMessage}" )
    }

    // 2-1が成功したら2-2の処理を行います
    f21 onSuccess {
      case r => {

        println( s"onSuccess:${System.currentTimeMillis()-stime} sec" )

        // 2-2の処理を行うFuture。
        val f22 = future {
          def divide2( ev: Int, s: Int=0 ) {
            ev match {
              case x if ( x < 2 ) => s
              case x => divide2( x/2, s+1 )
            }
          }
        }
        f22 onComplete {
          case Failure(e) => println( s"f22 error: ${e.getMessage}" )
          case Success(r) => println( s"f22 divided number: ${r}" )
        }
//        val res = f22
        Await.result( f22, Duration.Inf )
      }
    }

    println( s"before await:${System.currentTimeMillis()-stime} sec" )

    val res = Await.result( f21, Duration.Inf )
    println( s"Await.result:${res}" )
  }
}
