package uri_shooter
import akka.actor._
import java.net.InetSocketAddress
import akka.util.ByteString
import akka.actor.IO.ReadHandle
import scala.collection.mutable.ArrayBuffer

class web_svr(ip: String, port: Int, convert: Int) extends Actor {
  val ERR = -1
  val OK = 0
  val WRONG = (ERR, "")
  val LONG = 1
  val SHORT = 2
  val SVR_NAME = "http://server-name/"

  val WRONG3 = (ERR, LONG, "")

  val RES_ERR = "ERROR"

  var WorkerIndex = 0
  val MaxWorker = 8
  var workers = ArrayBuffer[ActorRef]()

  var ok_counter = 0
  var err_counter = 0

  val CONVERT = convert

  override def preStart {
    IOManager(context.system).listen(new InetSocketAddress(ip, port))
    create_workers(MaxWorker)
  }

  def receive = {
    case IO.NewClient(server) =>
      server.accept()
    case IO.Read(rHandle, bytes) => {
      call_worker(rHandle, bytes, get_worker_index())
    }
  }

  def create_workers(MaxWorker: Int) {
    for (i <- 0 to MaxWorker) {
      var worker = context.actorOf(Props(new Worker), i.toString)
      workers += worker
    }

  }

  class Worker extends Actor {
    def receive = {
      case (rHandle: ReadHandle, bytes: ByteString) =>
        //println("worker received message!")     
        do_worker(rHandle: ReadHandle, bytes: ByteString)
      case other =>
        println("Ignore")
        println(other)
    }
  }

  def get_worker_index(): Int = {
    WorkerIndex += 1
    return WorkerIndex % MaxWorker
  }

  def call_worker(rHandle: ReadHandle, bytes: ByteString, Index: Int) = {
    //println("call worker " + Index.toString)
    workers(Index) ! (rHandle, bytes)
  }

  def do_worker(rHandle: ReadHandle, bytes: ByteString) = {

    val raw = bytes.decodeString("US-ASCII").trim
    //println(raw);
    val Res = decode_url(raw) match {
      case (OK, url) => handle_url(url)
      case WRONG =>
        update_counter(ERR)
        RES_ERR
    }
    //val head:ByteString = ByteString("HTTP/1.1 200 OK\r\n\r\n")
    val Response = ByteString(Res)
    //println(Response.decodeString("US-ASCII").trim);
    rHandle.asSocket.write(Response)
    rHandle.close()
  }

  def rm_host(host: String, data: String): String = {
    var i = data.indexOf(host)
    if (i >= 0) {
      return data.substring(i + host.length(), data.length())
    } else {
      return ""
    }
  }
  def split_url(bytes: String): (Int, Int, String) = {
    val SLASH = '/'
    val EQUE = '='
    val PrifexLong = "/urlshortener/url?longUrl"
    val PrifexShort = "/urlshortener/url?shortUrl"
    val URL = "url"
    val bg = 0

    val bg1 = bytes.indexOf(EQUE, bg)
    if (bg1 >= 0) {
      val prefix = bytes.substring(bg, bg1)
      val data = bytes.substring(bg1 + 1, bytes.length)
      /*      
      println(prefix)
      println(PrifexLong)
      println(PrifexShort)
*/
      //println(data)
      prefix match {
        case PrifexLong =>
          //println(1)
          return (OK, LONG, data)
        case PrifexShort =>
          //println(2)
          val index = rm_host(SVR_NAME, data)
          return (OK, SHORT, index)
        case _ =>
          //println(3)
          return WRONG3

      }
    }
    return WRONG3
  }

  def update_counter(status: Int) {
    status match {
      case OK => ok_counter = ok_counter + 1
      case ERR => err_counter = err_counter + 1
    }
  }

  def handle_url(bytes: String): String = {
    split_url(bytes) match {
      case WRONG3 => return RES_ERR
      case (OK, cmd, url) =>
        var Handler = new handler(SVR_NAME)
        var (status: Int, res: String) = Handler.processReq(cmd, url)
        update_counter(status)
        return res
    }
  }

  def replace(rStr: String, rFix: String, rRep: String): String = {
    //    println(13)
    var l = 0
    var gRtnStr = rStr
    var gRtnStr1 = rStr
    val fLeng = rFix.length()
    val rLeng = rRep.length()

    val sLeng = rStr.length()
    var curLen = 0

    l = gRtnStr.indexOf(rFix, l)
    while (l >= 0) {
      gRtnStr1 = gRtnStr.substring(0, l) + rRep
      curLen = gRtnStr.length()
      if ((l + fLeng) <= curLen) {
        gRtnStr1 = gRtnStr1 + gRtnStr.substring(l + fLeng, curLen)
      }
      l += rLeng

      //search next 
      gRtnStr = gRtnStr1
      l = gRtnStr.indexOf(rFix, l)
    }

    return gRtnStr.substring(0, gRtnStr.length)
  }

  def uniform(rawurl: String): String = {
    val tab = List(
      ("%2B", "+"),
      ("%2F", "/"),
      ("%3F", "?"),
      ("%3D", "="),
      ("%26", "&"),
      ("%20", " "))
    var url = rawurl
    var i: Int = 0
    var len: Int = tab.length
    while (i < len) {
      var (s, d) = tab(i)
      url = replace(url, s, d)
      i = i + 1
    }

    //    println(("uninform",url))
    return url

  }
  def decode_url(bytes: String): (Int, String) = {
    val SP = " "
    val GET = "GET"
    val bg = 0

    val bg1 = bytes.indexOf(SP, bg)
    if (bg1 >= 0) {
      val hd = bytes.substring(0, bg1)
      if (GET != hd) {
        return WRONG
      }
      val bg2 = bytes.indexOf(SP, bg1 + 1)
      if (bg2 >= 0) {
        val rawurl = bytes.substring(bg1 + 1, bg2)
        //println(CONVERT)
        if (CONVERT == 1) {
          val url = uniform(rawurl)
          return (OK, url)
        } else return (OK, rawurl)
      }
    }
    return WRONG
  }
}

object Application {
  def main(args: Array[String]) {
    if (args.length < 3) {
      println("Miss parameters. Args[0]: IP, Args[1]: Port Args[2]: Convert\nExample: 127.0.0.1 80 0")
      return
    }
    //val ip="146.11.24.107"
    //val ip="127.0.0.1"
    //val port = 80
    val ip = args(0)
    val port = args(1).toInt
    val convert = args(2).toInt
    //println(convert)
    ActorSystem().actorOf(Props(new web_svr(ip, port, convert)))
  }
}