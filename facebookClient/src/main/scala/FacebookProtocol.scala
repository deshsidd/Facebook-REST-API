
import scala.collection.mutable.ArrayBuffer
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport._
import spray.routing._
import spray.json.DefaultJsonProtocol._
import akka.actor._

object FacebookProtocol {

  import spray.json._

  case class User(id:Int , name:String , username:String , password:String)

  case class LoginData(userid : String, data : Array[Byte])

  case class Post(creator:String , content:String , time:String, token : LoginData,id : Int)

  case class Page(id:Int,creator:String, posts: Vector[Post])

  case class Group(id:Int,creator:String,name:String)

  case class Image(imagedata:String,caption:String,id:Int, token : LoginData)
  case class AESKeyMap(mapKey : String, aes : Array[Byte])

  case class KeyAndData(key : Array[Byte], creator : String, content : String, time : String)

  //case class Quiz(id: String, question: String, correctAnswer: String)

  case object UserCreated

  case object UserAlreadyExists

  case object GroupCreated

  case object GroupAldreadyExists

  case object UserDeleted

  case class FriendRequest(id:Int)

  case class RequestSent(id:Int)

  case class RequestAccepted(id:Int)

  case class AcceptRequest(id:Int)

  case class PageRequest(pageid:Int,userid:Int,ctx:RequestContext)

  case class GetFriendList(ctx:RequestContext)

  case class AcceptAll(ctx:RequestContext)

  case class Posting(senderid:Int,pageid:Int,newpost:Post,ctx:RequestContext)

  case class GetProfile(ctx:RequestContext)

  case class JoinGroup(id:Int,ctx:RequestContext)

  case class GroupPosting(id:Int,newpost:Post,ctx:RequestContext)

  case class UploadImage(image:Image,ctx:RequestContext)

  case class ImageRequest(imageid:Int,senderid:Int,ctx:RequestContext)

  var Users = Vector(User(1,"siddhant","siddhantd28","sid"), User(2,"anuj","anuj","@nuj") )

  var Groups= Vector[Group]()

  var AllUsers = scala.collection.mutable.Map(1 -> "siddhant" , 2 -> "anuj")


  // json (un)marshalling

  object User extends DefaultJsonProtocol {
    implicit val userformat = jsonFormat4(User.apply)






  }
  object LoginData extends DefaultJsonProtocol{
    implicit val dataformat = jsonFormat2(LoginData.apply)
  }

  object Post extends DefaultJsonProtocol{

    implicit val postformat = jsonFormat5(Post.apply)
  }

  object Group extends DefaultJsonProtocol{

    implicit val groupformat = jsonFormat3(Group.apply)
  }

   object Image extends DefaultJsonProtocol{

    implicit val imageformat = jsonFormat4(Image.apply)
  }

object AESKeyMap extends DefaultJsonProtocol{

 implicit val imageformat = jsonFormat2(AESKeyMap.apply)
}

object KeyAndData extends DefaultJsonProtocol{

 implicit val imageformat = jsonFormat4(KeyAndData.apply)
}


}
