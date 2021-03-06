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

    scala> implicit val trap = fail[Any] // inserts default method missing handler
    ...

    scala> s-->'nonExistingMethod("foo", "bar")
    dynscala.DynScala$MethodMissingError: method missing nonExistingMethod(foo, bar)
          at dynscala.DynScala$Meta$.dynscala$DynScala$Meta$$fail(DynScala.scala:51)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:41)
          at dynscala.DynScala$Meta$$anonfun$2.apply(DynScala.scala:41)
          at dy...

    scala> implicit val trap = new Trap[String] { 
             def apply(s: String, site: CallSite) = println("hah, trapped " + s + " " + site) 
           } // inserts method missing handler for String type

    scala> s-->'nonExistingMethod("foo", "bar")
    hah, trapped i am a string nonExistingMethod(foo, bar)

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

