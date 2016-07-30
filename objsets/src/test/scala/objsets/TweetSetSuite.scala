package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    
    val list0 = Nil
    val list1 = new Cons(c, list0)
    val list2 = new Cons(d, list1)
    val list3 = new Cons(c, list2)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("isEmpty") {
    new TestSets {
      assert(set1.isEmpty === true)
      assert(set2.isEmpty === false)
    }
  }

  test("contains") {
    new TestSets {
      assert(set1.contains(c) === false)
      assert(set2.contains(new Tweet("H2", "b body", 192)) === false)
      assert(set2.contains(new Tweet("H2", "a body", 192)) === true)
      assert(set3.contains(new Tweet("H3", "b body", 193)) === true)
    }
  }

  test("remove") {
    new TestSets {
      assert(set1.remove(new Tweet("H2", "b body", 191)).isEmpty)
      assert(size(set2.remove(new Tweet("H2", "b body", 192))) === 1)
      assert(size(set2.remove(new Tweet("H2", "a body", 192))) === 0)
      assert(size(set3.remove(new Tweet("H2", "a body", 193))) === 1)
      assert(size(set3.remove(new Tweet("H2", "b body", 193))) === 1)
    }
  }
  
  test("filter") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union") {
    new TestSets {
      assert(size(set1.union(set2)) === 1)
      assert(size(set2.union(set3)) === 2)
      assert(size(set4c.union(set4d)) === 4)
      assert(size(set5.union(set1)) === 4)
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted") {
    new TestSets {
      assert(set5.mostRetweeted.text.endsWith(" body"))
      assert(set4c.mostRetweeted.retweets === 20)
    }
    
    intercept[NoSuchElementException] {
      new TestSets {
    	  set1.mostRetweeted
      }
    }
  }

  test("TweetList#append") {
    new TestSets {
      assert(list0.append(list0) === Nil)
      
      assert(list0.append(list1).head.toString === c.toString)
      assert(list0.append(list1).tail === Nil)
      
      assert(list0.append(list2).head === d)
      assert(list0.append(list2).tail.head === c)
      assert(list0.append(list2).tail.tail === Nil)
      
      assert(list3.append(list1).tail.tail.head === c)
      assert(list3.append(list1).tail.tail.tail.head === c)
      assert(list3.append(list1).tail.tail.tail.tail === Nil)
    }
  }
  
  test("descendingByRetweet") {
    new TestSets {
      assert(set1.descendingByRetweet.isEmpty)
      assert(set2.descendingByRetweet.tail === Nil)
      assert(set3.descendingByRetweet.tail.tail === Nil)
      
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.head.retweets === 20)
      assert(trends.tail.head.retweets === 20)
      assert(trends.tail.tail.head.retweets === 9)
      assert(trends.tail.tail.tail.head.retweets === 7)
      assert(trends.tail.tail.tail.tail === Nil)
    }
  }
}
