package uri_shooter


import scala.collection.mutable.ListBuffer

object urlList {
  
	//private val urlList = new ListBuffer[String]
	private val urlList = new scala.collection.mutable.ArrayBuffer[String]

	def append(s: String): Int ={
	  
	  var index = 0
	  
	  this.synchronized{	    
	  urlList += s
	  index = urlList.length -1
	  }
	  return index  
	}
	
	def count(): Int = {
	  
	  return urlList.length
	}
	
	def get(i: Int): String ={
	  
	  val count = urlList.length
	  
	  if(i+1 > count)
	  {
		  return ""
	  }
	  
	  return urlList.toList(i)	  
	}
}