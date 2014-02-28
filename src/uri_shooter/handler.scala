package uri_shooter

class handler(svrname:String) {
  val ERR = -1
  val OK=0
  val LONG=1
  val SHORT=2
  val SVR_NAME=svrname
  val HEAD="HTTP/1.1 200 OK\r\nConnection:close\r\nServer:Blizzard\r\nContent-Type: application/json\r\nContent-Length: "
  //val HEAD="" 
      
      
  def main(args: Array[String]) = println("=========")

/*  processReq("longUrl","http://www.baidu.com/s?ie=utf-8&bs=scala+%E4%BA%A7%E7%94%9F%E9%9A%8F%E6%9C%BA%E6%95%B0&f=8&rsv_bp=1&rsv_spt=3&wd=scala+%E9%9A%8F%E6%9C%BA%E6%95%B0&rsv_sug3=3&inputT=789")
  processReq("longUrl","http://www.ericsson.com/thecompany/company_facts/history1")
  processReq("shortUrl","1")
  processReq("shortUrl","0")
  processReq("shortUrl", "2")
  processReq("shortUrl", "")
  processReq("longUrl", "")
  processReq("", "xx")*/

  def processReq(urltype: Int, url: String):(Int,String) =  {
    //val shortUrl = "shortUrl"
    //val longUrl = "longUrl" 
    if (!url.isEmpty())
      urltype match {
        case LONG =>
          return processLong("longUrl", url)
        case SHORT =>
          return processShort("shortUrl", url)
        case _ =>
          return processError("404", "Not found")
      }
    else
      return processError("404", "Not found")

  }

  def processLong(urltype: String, url: String): (Int,String) = {
    val returnString ="{\"kind\": \"shorten\"," ++
      "\"shortUrl\": \""++ SVR_NAME  ++ LongtoShorturl(url) ++ "\"," ++
      "\"longUrl\": \"" ++ url ++ "\"}"    
   // println(returnString)
    val returnString1 =HEAD + returnString.length.toString  + "\r\n\r\n"+ returnString
    return (OK,returnString1)
    //printarry(returnString)	

  }

  def processShort(urltype: String, url: String): (Int,String) = {
    var s2l = ShorttoLongurl(url)
    if (s2l.isEmpty()) {
      processError("404", "Not Found")
    } else {
      val returnString = "{\"kind\": \"expand\"," ++
        "\"shortUrl\": \""++ SVR_NAME  ++ url ++ "\"," ++
        "\"longUrl\": \"" ++ s2l ++ "\"}"
     // println(returnString)
      val returnString1 =HEAD + returnString.length.toString+"\r\n\r\n"+ returnString
      return (OK,returnString1)
    }
  }

  def processError(errCode: String, errMsg: String): (Int,String) = {
    val returnString = "{\"error\": true," ++
      "\"code\":" ++ errCode ++ "," ++
      "\"message\": \"" ++ errMsg ++ "\"}"
    val returnString1 =HEAD + returnString.length.toString+"\r\n\r\n"+ returnString
    //println(returnString)
    return (ERR,returnString1)
  }

  def printarry(args: Array[String]) {
    var i = 0
    while (i < args.length) {
      println(args(i))
      i += 1
    }
  }

  def ShorttoLongurl(url: String): String =
    {
      val surl = new Short2Long
      var lurl = surl.convert(url)
      return lurl
    }
  def LongtoShorturl(url: String): String =
    {
      val lurl = new Long2Short
      var surl = lurl.convert(url)
      return surl

    }

}