package uri_shooter

import scala.collection.mutable.ListBuffer

class Long2Short{
  
	private var count=0
	//private val urlList = new ListBuffer[String]
	//val base = 62
	
	def getCount(): Int={
	  return count	
	}
	
	def convert(url: String): String = {
	  	  	  	 
	  if(url.length() == 0)
	  {
	    throw new IllegalArgumentException("Url length must greater than 0")  
	  }
	  	  
	  val index = urlList.append(url)	  
	  
	  val base62 = new Base62
	  
	  def shortUrl = base62.encode(index)
	  
	//  println(shortUrl)
	  
	  //println(base62.decode(shortUrl))
	  
	  //println(index);
	  
	  return shortUrl
	  
	}
	
}
