package com.inkenkun.x1.scala.async

import scala.collection.parallel.availableProcessors
import concurrent.{ Await, future, Future }
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration._
import scala.util.{Failure, Success}

/**
 * scala2.10から導入されたFutureとPromiseのサンプル・ソースです。
 *
 * 1〜50までの数字の中から任意の5個を選びます。
 * 5個の数値を足した合計が奇数か偶数か判定します。
 */
object Main extends App {

  def n = 50  // 母数
  def r = 5 // 任意の5個

  override def main( args: Array[String] ): Unit = {

    // 1. Futureの基本的な使い方
//    standard1
    // 2. 見通しの悪いコールバックチェイン。
//    callback1
    // 3. for内包表記によるコールバックチェイン。
//    callback2
    // 4. 実用的な並列処理。
//    pragmatic

    // lazy valの検証
    val laz = new Lazziness()
    laz.method
//Lazziness.method
//LazzinessTrait.lazzillyPrint!
//called,LazzinessTrait.counter!
//counter:15
  }

  /**
   * 1. Futureの基本的な使い方
   *  1〜50までの数字の中から任意の5個を選ぶ。
   *  5個の数値を足した合計が奇数か偶数か判定する。
   *  偶数がいくつあるかを戻り値とする。
   * ・・という処理を非同期で行います。
   */
  def standard1: Unit = {

    val stime = System.currentTimeMillis()

    // Futureを返すfutureメソッドを呼び出します。
    val fs = future {
      ( 1 to n ).combinations( r ).count( _.sum % 2 == 0 )
    }

    // Futureの完了。Futureは値もしくは例外を持つとき、完了したことになります。
    fs onComplete {
      case Failure(e) => println( s"error: ${e.getMessage}" )
      case Success(r) => println( s"even number: ${r}" )

      println( s"onComplete:${System.currentTimeMillis()-stime} ms" )
    }

    // 下記のように書き直すこともできます。
//    fs onSuccess {
//      case r => println( s"even number: ${r}" )
//    }
//    fs onFailure {
//      case e => println( s"error: ${e.getMessage}" )
//    }

    println( s"before await:${System.currentTimeMillis()-stime} ms" )

    val res = Await.result( fs, Duration.Inf )
    println( s"Await.result:${res}" )
  }

  /**
   * 2. 見通しの悪いコールバックチェイン。
   *  2-1. 1〜50までの数字の中から任意の5個を選ぶ。
   *       5個の数値を足した合計が奇数か偶数か判定し、偶数の数を数える。
   *   ↑ここまでが非同期で行う1番目のブロック
   *  2-2.最終的に算出された偶数の値を割り切れなくなるまで2で割り、割った回数を返却する。
   *   ↑ここまでが非同期で行う2番目のブロック
   *
   * Futureのバックグラウンドで使われているスレッドはForkJoinPoolのWorkerスレッド（Daemonスレッド）なので、それ以降の処理がないとJavaVMが終了してしまいます。
   * というわけで下のf22は実行されませんw
   */
  def callback1: Unit = {

    val stime = System.currentTimeMillis()

    // 2-1の処理を行うFuture。
    val f21 = future {
      println( s"f21 start:${System.currentTimeMillis()-stime} ms" )
      ( 1 to n ).combinations( r ).count( _.sum % 2 == 0 )
    }

    // 2-1が失敗したら2-2の処理は行いません。
    f21 onFailure {
      case e => println( s"error: ${e.getMessage}" )
    }

    // 2-1が成功したら2-2の処理を行います。
    f21 onSuccess {
      case r => {

        println( s"onSuccess:${System.currentTimeMillis()-stime} ms" )
        println( s"result of f21:${r}" )

        // 2-2の処理を行うFuture。
        val f22 = future {
          def divide2( ev: Int, s: Int=0 ): Int = {
            ev match {
              case x if ( x < 2 ) => s
              case x => divide2( x/2, s+1 )
            }
          }
          divide2( r )
        }
        f22 onSuccess {
          case r => println( s"f22 divided number: ${r}" )
        }
        f22 onFailure {
          case e => println( s"f22 error: ${e.getMessage}" )
        }
      }
    }

    println( s"before await:${System.currentTimeMillis()-stime} ms" )

    val res = Await.result( f21, Duration.Inf )
    println( s"Await.result:${res}" )
  }

  /**
   * 3. for内包表記によるコールバックチェイン。
   *  3-1. 1〜50までの数字の中から任意の5個を選ぶ。
   *       5個の数値を足した合計が奇数か偶数か判定し、偶数の数を数える。
   *   ↑ここまでを非同期で行う
   *  3-2.最終的に算出された偶数の値を割り切れなくなるまで2で割り、割った回数を返却する。
   */
  def callback2: Unit = {

    val stime = System.currentTimeMillis()

    // 3-1の処理を行うFuture。
    val f31 = future {
      println( s"f31 start:${System.currentTimeMillis()-stime} ms" )
      ( 1 to n ).combinations( r ).count( _.sum % 2 == 0 )
    }

    val f32 = for {
      f <- f31
    } yield {
      println( s"f32 yield:${System.currentTimeMillis()-stime} ms" )
      // 3-2の処理を行います。
      def divide2( ev: Int, s: Int=0 ): Int = {
        ev match {
          case x if ( x < 2 ) => s
          case x => divide2( x/2, s+1 )
        }
      }
      val d = divide2( f )
      println( s"f32 divided number: ${d}" )
      d
    }

    // 3-1が成功したら3-2の処理を行います。
    f32 onComplete  {
      case Success(r) =>
        println( s"f32 onComplete get: ${r}" )
      case Failure(e) => println( s"f32 error: ${e.getMessage}" )
    }

    println( s"before await:${System.currentTimeMillis()-stime} ms" )

    val res = Await.result( f32, Duration.Inf )
    println( s"Await.result:${res}" )
  }

  /**
   * 4. 実用的な並列処理。
   *  4-1. 1〜50までの数字の中から任意の5個を選ぶ。
   *       50までの数字の中から5個を選ぶ組み合せは2,118,760通りである。
   *  4-2. この2,118,760通りの組み合せに対して、5個の数値を足した合計が奇数か偶数か判定し、偶数の数を数える。
   *       2,118,760通りをプロセッサ数分のスレッドで偶数判定を行う。
   *  4-3. 最終的に算出された偶数の値を割り切れなくなるまで2で割り、割った回数を返却する。
   *
   */
  def pragmatic: Unit = {

    val stime = System.currentTimeMillis()

    // 4-1. 1〜50までの数字の中から任意の5個を選びます。
    val size = combins( n, r )
    println( s"combination number:${size}, ${System.currentTimeMillis()-stime} ms" )

    // 1スレッドが処理する組数です。
    val block: Int = size / availableProcessors

    // 組み合せを生成します。
    val cs = ( 1 to n ).combinations( r ).toIndexedSeq
    println( s"combination generated:${System.currentTimeMillis()-stime} ms" )

    val newcs = for ( i <- 1 to availableProcessors ) yield {
      //cs.drop( (i-1) * block ).take( block )
      cs.slice( (i-1) * block, i * block )
    }
    // 4-2. スレッドを生成します。
    val futures = for ( i <- 1 to availableProcessors )
      yield future {
      newcs(i-1).count( _.sum % 2 == 0 )
    }

    println( s"before await:${System.currentTimeMillis()-stime} ms" )

    // 4-2.の結果を待つ受けます。
    val f42 = Await.result( Future.sequence( futures ), Duration.Inf )
    println( s"f42 complete:${System.currentTimeMillis()-stime} ms" )

    // 4-2が成功したら4-3の処理を行います。
    def divide2( ev: Int, s: Int=0 ): Int = {
      ev match {
        case x if ( x < 2 ) => s
        case x => divide2( x/2, s+1 )
      }
    }
    val d = divide2( f42.sum )
    println( s"f43 divided number: ${d}" )
    println( s"f43 complete:${System.currentTimeMillis()-stime} ms" )
  }

  def combins( n: Int, r: Int ): Int = {
    r match {
      case 0 => 1
      case 1 => n
      case x if ( x == n ) => 1
      case _ => combins( n-1, r-1 ) + combins( n-1, r )
    }
  }
}
