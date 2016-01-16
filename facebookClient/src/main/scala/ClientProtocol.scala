import java.security._
import akka.actor._

case object StartOperation
case object GetErrors
case object ReturnErrors
case object AddFriend
case object AcceptAllRequests
case object GetFriendsList
case object UserCreated
case object AddFriends
case object FriendsAdded
case object AcceptedRequests
case object StartPostingOne
case object StartPostingTwo
case object StartGettingPosts

case class ReceiveAesKey(selfId : Int, key : Array[Byte])
case class GetClientRefFromServer(userId : Int)
case class ReturnAesKey(userPuk : PublicKey)
case class AddFriend(friendId : Int)
case class AcceptRequest(friendId : Int)
case class InitializeUser(name : String, userName : String)
case class ReceiveErrorsCount(num : Int)
case class GetAnotherUserProfile(userId : Int)
case class PostOnUserPage(userId : Int, postData : String, id:Int, key:PublicKey)
case class GetUserWallPage(userId : Int, postId : Int)

case class UploadAnImage(imagedata:String,caption:String)
case class StartUploadingImages()
case class GetAnImage(userId:Int,imageId:Int)
case class StartGettingAnImage()
