DynScala
========

Fan language supports dynamic method dispatch using operator `->`
DynScala brings this feature to Scala using an ugly operator `-->'`  

Sample:
-------

    scala> import dynscala.DynScala._
    import dynscala.DynScala._

    scala> val s = "i am a string"
    s: java.lang.String = i am a string

    scala> s-->'nonExistingMethod("foo", "bar")
    dynscala.DynScala$MethodMissingError: method missing nonExistingMethod(foo, bar)
          at dynscala.DynScala$Meta$.dynscala$DynScala$Meta$$fail(DynScala.scala:51)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:41)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:41)
          at dy...

    scala> classOf[String].trap { (receiver, site) => 
             println("hah, trapped " + receiver + " " + site)
           } // trap inserts method missing handler for String type

    scala> s-->'nonExistingMethod("foo", "bar")
    hah, trapped i am a string nonExistingMethod(foo, bar)

Installed method missing handlers can be scoped by mixing in DynScala trait.

    scala> object foo extends dynscala.DynScala {   
             classOf[List[Int]].trap { (r, s) => List(1,2,3,4,5) }
             println(List[Int]()-->'weird())
           }
    defined module foo

    scala> object boo extends dynscala.DynScala {
             println(List[Int]()-->'weird())
           }
    defined module boo

    scala> foo
    List(1, 2, 3, 4, 5)

    scala> boo
    dynscala.DynScala$MethodMissingError: method missing weird()
          at dynscala.DynScala$Meta$.dynscala$DynScala$Meta$$fail(DynScala.scala:50)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:40)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:40)
          at scala.Option.getOrElse(Option.scala:61)
          at dynscala.DynScala$Meta$.trap(DynScala.scala:40)
          at dynscala.DynScala$DynCal...


DynQuery example:
-----------------

File src/test/scala/example/DynQuery.scala contains a simple example providing   
Grails like query builder (well, without any features to make it useful).  

    case class Book(title: String, date: java.util.Date, author: String)

    val q1 = Book-->'findByTitle("The Stand")
    val q2 = Book-->'findByTitleLike("Harry Pot%")
    val q3 = Book-->'findByDateGreaterThan(someDate)
 
    assert(q1 == "select author,date,title from book where title='The Stand'")
    assert(q2 == "select author,date,title from book where title like 'Harry Pot%'")
    assert(q3 == "select author,date,title from book where date > '" + someDate + "'")

