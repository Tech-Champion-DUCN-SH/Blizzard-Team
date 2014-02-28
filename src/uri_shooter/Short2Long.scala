package uri_shooter

/**
  * Providing access to one hashed URI
  * look up the shortened URI to get the longer one
  */


class Short2Long{
  
	private var count=0	
	val base = 62
	
	def getCount(): Int={
	  return count	
	}
	
	def convert(surl: String): String = {	  	  	  	 	  
	  	  
	    if (surl.size > 6 || surl.size == 0) {
	      
	    	throw new IllegalArgumentException("baseString length must be 1-6")
	    }
	  
	  val base62 = new Base62
	  
	  val index = base62.decode(surl).toInt
	  
	  //println(index)
	  
	  return urlList.get(index)
	  
	}
	
}

