package example

import dynscala._

/** Grails like queries (just a demo/proof-of-concept)
 */
trait DynQuery extends DynScala {
  classOf[AnyRef].trap((receiver, site) => {
    def tableName = receiver.getClass.getName.split('$').last.toLowerCase
    def fieldNames = companion.getDeclaredFields.map(_.getName)
    def companion = Class.forName(receiver.getClass.getName.substring(0, receiver.getClass.getName.length - 1))
    
    val queryDesc = site.name.drop("findBy".length).toString
    val fieldDesc = 
      if (queryDesc.contains("Like") || queryDesc.contains("GreaterThan"))
        queryDesc.replace("Like", " like ").replace("GreaterThan", " > ")
      else 
        queryDesc + "="
    val where = fieldDesc.toLowerCase + site.params.mkString("'", ",", "'")
    "select " + fieldNames.mkString(",") + " from " + tableName + " where " + where
  })
}

object Example {
  case class Book(title: String, date: java.util.Date, author: String)

  object BookQueries extends DynQuery {
    val someDate = new java.util.Date

    val q1 = Book-->'findByTitle("The Stand")
    val q2 = Book-->'findByTitleLike("Harry Pot%")
    val q3 = Book-->'findByDateGreaterThan(someDate)
    
    println(q1 + "\n" + q2 + "\n" + q3)
    assert(q1 == "select author,date,title from book where title='The Stand'")
    assert(q2 == "select author,date,title from book where title like 'Harry Pot%'")
    assert(q3 == "select author,date,title from book where date > '" + someDate + "'")
  }
}

