import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.message._


//noinspection DuplicatedCode
object RestClient extends App {

  val command = args.head
  val params = parseArgs(args)
  val url = args.last

  def parseArgs(args: Array[String]): Map[String, List[String]] = {
    def nameValuePair(paramName: String): (String, List[String]) = {
      def values(commaSeperatedValues: String): List[String] = commaSeperatedValues.split(',').toList

      val index = args.indexWhere(_ == paramName)
      (paramName, if (index == -1) Nil else values(args(index + 1)))
    }

    Map(nameValuePair("-d"), nameValuePair("-h"))
  }

  def handlePostRequest(): Unit = {
    println("POST request")
    val httppost = new HttpPost(url)
    headers.foreach {
      httppost.addHeader(_)
    }
    httppost.setEntity(formEntity)
    val responseBody = new DefaultHttpClient().execute(httppost, new BasicResponseHandler())
    println(responseBody)
  }

  def headers: List[BasicHeader] = {
    for (nameValue <- params("-h")) yield {
      val tokens = splitByEqual(nameValue)
      new BasicHeader(tokens(0), tokens(1))
    }
  }

  def formEntity: UrlEncodedFormEntity = {
    def toJavaList(scalaList: List[BasicNameValuePair]): java.util.List[BasicNameValuePair] = {
      java.util.Arrays.asList(scalaList: _*)
    }

    def formParams: List[BasicNameValuePair] = {
      for (nameValue <- params("-d")) yield {
        val tokens = splitByEqual(nameValue)
        new BasicNameValuePair(tokens(0), tokens(1))
      }
    }

    new UrlEncodedFormEntity(toJavaList(formParams), "UTF-8")
  }

  def splitByEqual(nameValue: String): Array[String] = nameValue.split('=')

  require(args.length >= 2, "at minimum you should specify action(post, get, delete, options) and url")

  def handleGetRequest(): Unit = {
    println("GET request")
    val query = params("-d").mkString("&")
    val httpget = new HttpGet(s"$url?$query")
    headers.foreach {
      httpget.addHeader(_)
    }
    val responseBody = new DefaultHttpClient().execute(httpget, new BasicResponseHandler())
    println(responseBody)
  }

  def handleDeleteRequest(): Unit = {
    println("DELETE request")
    val httpDelete = new HttpDelete(url)
    val httpResponse = new DefaultHttpClient().execute(httpDelete)
    println(httpResponse.getStatusLine)
  }

  def handleOptionsRequest(): Unit = {
    println("OPT request")
    val httpOptions = new HttpOptions(url)
    headers.foreach {
      httpOptions.addHeader(_)
    }
    val httpResponse = new DefaultHttpClient().execute(httpOptions)
    println(httpOptions.getAllowedMethods(httpResponse))
  }

  command match {
    case "post" => handlePostRequest()
    case "get" => handleGetRequest()
    case "delete" => handleDeleteRequest()
    case "options" => handleOptionsRequest()
  }
}
