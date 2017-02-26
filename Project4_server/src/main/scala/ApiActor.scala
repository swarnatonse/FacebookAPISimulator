import java.security.KeyFactory
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import javax.crypto.Cipher

import akka.actor.{Props, ActorSystem, ActorLogging, Actor}
import akka.util.Timeout
//import com.sun.org.apache.xml.internal.security.utils.Base64
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol
import spray.routing._
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import spray.httpx.unmarshalling._
import spray.http.HttpEntity
import scala.concurrent.Await
import java.net.InetAddress
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet
import scala.collection.JavaConversions._
import scala.util.Random
import akka.pattern.ask
import akka.dispatch.Futures
import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.routing.RoundRobinRouter
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import spray.routing.SimpleRoutingApp
import spray.json._
import scala.concurrent.Await
import akka.util.Timeout
import org.apache.commons.codec.binary.Base64

import scala.concurrent.Await

object UserProtocol extends DefaultJsonProtocol {
  case class ClientInfo(requesterid:Int)
  case class User(id:Int,name: String, about:String, age:String, var education:String, var gender:String, location:String)
  case class Post(ownerId:Int, caption:String, content:String, var pic:String, var token:String)
  case class Page(ownerId:Int, name:String, description:String, pagetype:String, location:String)
  case class Photo(ownerId:Int, var url:String, var caption:String, albumId:String)
  case class AlbumPhoto(url:String)
  case class Album(ownerId:Int, photos:List[AlbumPhoto], var description:String)
  case class Friend(friendID:Int, EnKey:String)
  case class PublicKeyList(pklist:List[Tuple2[Int, String]])

  implicit val UserFormat = jsonFormat7(User)
  implicit val PostFormat = jsonFormat5(Post)
  implicit val ClientFormat = jsonFormat1(ClientInfo)
  implicit val PageFormat = jsonFormat5(Page)
  implicit val PhotoFormat = jsonFormat4(Photo)
  implicit val AlbumPhotoFormat = jsonFormat1(AlbumPhoto)
  implicit val AlbumFormat = jsonFormat3(Album)
  implicit val PublicKeyListFormat = jsonFormat1(PublicKeyList)
}
import UserProtocol._

object FriendListProtocol{
  var FriendList = scala.collection.mutable.LinkedHashMap[Int, List[Int]]()

  FriendList += (123456 -> List(111111))
  FriendList += (111111 -> List(123456, 234567))
  FriendList += (234567 -> List(111111))

  def findFriends(id:Int):List[Int]={
    if(FriendList.contains(id)) {
      return FriendList(id)
    } else { return null }
  }

  def addFriend(selfID:Int, friendID:Int): String = {
    if(isFriend(friendID, selfID) == false) {
      var x = FriendList(selfID)
      x = friendID :: x
      FriendList(selfID) = x
      "Success"
    } else { "Failure" }
  }
  def createEmptyFriendList(id:Int): String = {
    if(FriendList.contains(id) != true) {
      FriendList += (id -> List())
      "Success: Friend List created successfully"
    } else{ "Failure: Friend List already present" }
  }

  def deleteFriendList(id:Int, clientid:Int): String = {
    var friendList1 = List[Int]()
    var friendList2 = List[Int]()

    if(!FriendList.contains(id) || !FriendList.contains(clientid)){
      return "Failure: Friend List does not exist for this ID, errorcode=-1"
    }

    for(f_id <- FriendList(id)){
      if(f_id != clientid){
        friendList1 = f_id :: friendList1
      }
    }

    FriendList -= id
    FriendList += (id -> friendList1)

    for(f_id <- FriendList(clientid)){
      if(f_id != id){
        friendList2 = f_id :: friendList2
      }
    }
    FriendList -= clientid
    FriendList += (clientid -> friendList2)

    "Success: Friend deleted, code=1"
  }

  def isFriend(otherid:Int, id:Int):Boolean = {
    if(FriendList(id).contains(otherid)) return true
    return false
  }

//  def returnKey(id:Int, friendid:Int):String = {
//    println("Swarna: Entered this code for id "+id+" searching for friend "+friendid)
//    if(id == friendid) return "same_client"
//    for (f_id <- FriendList(id)) {
//      println("f_id is "+f_id)
//      if (f_id._1 == friendid) {
//        println("Swarna: "+f_id._2)
//        return f_id._2
//      }
//    }
//    return null
//  }
}

object UserMappingProtocol{
  var UserMapping = scala.collection.mutable.Map[Int, User]()
  UserMapping += (111111 -> User(111111, "Swarna", "Is a weirdo", "24", "Manipal Institute of Dragon Sciences","Female","Udupi"))
  UserMapping += (123456 -> User(123456, "Mythili", "Is sick", "23", "Someplace in Chennai", "Female", "Salem"))
  UserMapping += (234567 -> User(234567, "Vikaasa", "Is busy with IDM", "22", "University of Florida", "Male", "Chennai"))

  def findUser(id:Int):User={
    if(checkUserExists(id)) {
      return UserMapping(id).copy()
    } else {return null}
  }

  def addNewUser(id:Int, newUser:User): String = {
    if(checkUserExists(id) == false) {
      var tokens = newUser.gender.split(" ")
      if(tokens(1) != null){     KeyProtocol.PublicKeyMapping += (id -> tokens(1))  }
      newUser.gender = tokens(0)
      UserMapping += (id -> newUser)
      "Success: User added successfully"
    } else{ "Failure: User with that ID already exists" }
  }

  def checkUserExists(id:Int):Boolean = {
    if(UserMapping.contains(id)){
      return true
    }
    return false
  }

  def deleteUser(id:Int): String = {
    if(checkUserExists(id)) {
      UserMapping -= id
      "Success: User deleted successfully"
    } else{ "Failure: User with that ID doees not exist" }
  }

  def updateUser(updatedUser:User) = {
    if (checkUserExists(updatedUser.id)) {
      var name: String = ""
      var age: String = ""
      var about: String = ""
      var education: String = ""
      var gender: String = ""
      var location: String = ""
      var oldUser = UserMapping(updatedUser.id)
      if (updatedUser.name != "") {
        name = updatedUser.name
      } else {
        name = oldUser.name
      }
      if (updatedUser.about != "") {
        about = updatedUser.about
      } else {
        about = oldUser.about
      }
      if (updatedUser.age != 0) {
        age = updatedUser.age
      } else {
        age = oldUser.age
      }
      if (updatedUser.education != "") {
        education = updatedUser.education
      } else {
        education = oldUser.education
      }
      if (updatedUser.gender != "") {
        gender = updatedUser.gender
      } else {
        gender = oldUser.gender
      }
      if (updatedUser.location != "") {
        location = updatedUser.location
      } else {
        location = oldUser.location
      }
      var tobeupdated = new User(updatedUser.id, name, about, age, education, gender, location)
      var key = KeyProtocol.PublicKeyMapping(updatedUser.id)
      deleteUser(updatedUser.id)
      addNewUser(updatedUser.id, tobeupdated)
      "Success: User updated successfully"
    } else{ "Failure: User with that ID does not exist" }
  }

}

object PostProtocol{
  var PostMapping = scala.collection.mutable.Map[Int, Post]()
  /*PostMapping += (1 -> Post(111111, "", "Feeling bored!!!", ""))
  PostMapping += (2 -> Post(123456, "", "Not feeling well, somebody should comfort me", ""))
  PostMapping += (3 -> Post(234567, "", "IDM sucks, DOS rulezzzzz", ""))*/

  var postID = 1

  def returnPost(postId:Int):Post = {
    if(PostMapping.contains(postId)) {
      return PostMapping(postId).copy()
    } else {return null}
  }

  def deletePost(postId:Int): String = {
    if(PostMapping.contains(postId)) {
      PostMapping -= postId
      "Success: Post deleted successfully,code=1"
    }else{ "Failure: Post with that ID does not exist,error code=-1" }
  }

  def addPost(newPost:Post): String = {
    if(PostMapping.contains(postID) == false) {
      PostMapping += (postID -> newPost)
      postID = postID + 1
      "Success: Post added successfully, ID="+(postID).toString()
    }else{ "Failure: Post with that ID does not exist, error code=-1" }
  }

  def updatePost(postId:Int, updatePost:Post) = {
    if (PostMapping.contains(postId)) {
      var caption = ""
      var content = ""
      var pic = ""
      var old_post = returnPost(postId)
      if (updatePost.caption != "") {
        caption = updatePost.caption
      } else {
        caption = old_post.caption
      }
      if (updatePost.content != "") {
        content = updatePost.content
      } else {
        content = old_post.content
      }
      if (updatePost.pic != "") {
        pic = updatePost.pic
      } else {
        pic = old_post.pic
      }

      var updatedpost = new Post(old_post.ownerId, caption, content, pic, "")
      deletePost(postId)
      //addPost(postId, updatedpost)
      PostMapping += (postId -> updatedpost)
      "Success: Post updated successfully=1"
    }else{ "Failure: Post with that ID does not exist=-1" }
  }
}

object PageProtocol{
  var PageMapping = scala.collection.mutable.Map[Int, Page]()
  /*PageMapping += (1 -> Page(111111, "Led Zeppelin", "English Rock Band", "Band", "London"))
  PageMapping += (2 -> Page(123456, "The Alchemist", "Philosophy by Paulo Coelho", "Book", "Brazil"))
  PageMapping += (3 -> Page(123456, "Shawshank Redemption", "Breakout movie", "Movie", "Portland"))*/

  var pageID = 1

  def returnPage(postId:Int):Page = {
    if(PageMapping.contains(postId)) {
      return PageMapping(postId)
    } else {return null}
  }

  def deletePage(pageId:Int): String = {
    if(PageMapping.contains(pageId)) {
      PageMapping -= pageId
      "Success: Page deleted successfully, code=1"
    }else{ "Failure: Page with that ID does not exist, error code=-1" }
  }

  def addPage(newPage:Page) = {
    if(PageMapping.contains(pageID) == false) {
      PageMapping += (pageID -> newPage)
      pageID = pageID + 1
      "Success: Page added successfully, ID ="+(pageID).toString()
    }else{ "Failure: Page with that ID exists already, error code=-1"}
  }

  def updatePage(pageId:Int, updatePage:Page) = {
    if (PageMapping.contains(pageId)) {
      var name = ""
      var description = ""
      var pagetype = ""
      var location = ""
      var old_page = returnPage(pageId)
      if (updatePage.name != "") {
        name = updatePage.name
      } else {
        name = old_page.name
      }
      if (updatePage.description != "") {
        description = updatePage.description
      } else {
        description = old_page.description
      }
      if (updatePage.pagetype != "") {
        pagetype = updatePage.pagetype
      } else {
        pagetype = old_page.pagetype
      }
      if (updatePage.location != "") {
        location = updatePage.location
      } else {
        location = old_page.location
      }
      var updatedpage = new Page(old_page.ownerId, name, description, pagetype, location)
      deletePage(pageId)
      //addPage(pageId, updatedpage)
      PageMapping += (pageId -> updatedpage)
      "Success: Page updated successfully"
    }else{ "Failure: Page with that ID does not exist" }
  }
}

object PhotoProtocol{
  var PhotoMapping = scala.collection.mutable.Map[Int, Photo]()
  /*PhotoMapping += (1 -> Photo(111111, "", "Going to school", 0))
  PhotoMapping += (2 -> Photo(123456, "", "Coffee date", 0))*/

  var photoID = 1

  def addPhoto(newPhoto:Photo): String = {
    if(PhotoMapping.contains(photoID) == false) {
      PhotoMapping += (photoID -> newPhoto)
      photoID = photoID + 1
      "Success: Photo successfully added, ID="+(photoID).toString()
    } else { "Failure: Photo with that ID already exists, error code=-1" }
  }

  def deletePhoto(photoId:Int): String =
  {
    if(PhotoMapping.contains(photoId)) {
      PhotoMapping -= photoId
      "Success: Photo deleted successfully, code=1"
    } else { "Failure: Photo does not exist, code=-1"}
  }

  def returnPhoto(photoId:Int): Photo =
  {
    if(PhotoMapping.contains(photoId)) {
      return PhotoMapping(photoId).copy()
    }else {return null}
  }

  def updatePhoto(photoId:Int, updatePhoto:Photo): String= {
    if(PhotoMapping.contains(photoId)) {
      var caption = ""
      var old_photo = returnPhoto(photoId)
      if (updatePhoto.caption != "") {
        caption = updatePhoto.caption
      } else {
        caption = old_photo.caption
      }
      var updatedphoto = new Photo(old_photo.ownerId, updatePhoto.url, caption, old_photo.albumId)
      deletePhoto(photoId)
      //addPhoto(photoId, updatedphoto)
      PhotoMapping += (photoId -> updatedphoto)
      "Succeess: Photo updated successfully=1"
    } else { "Failure: Photo with that ID does not exist=-1" }
  }
}

object AlbumProtocol{
  var AlbumMapping = scala.collection.mutable.Map[Int, Album]()
  //AlbumMapping += (1 -> Album(234567, List(AlbumPhoto(1, ""), AlbumPhoto(2,"")), "Random description"))
  var albumID = 1

  def returnAlbum(albumId:Int):Album={
    if(AlbumMapping.contains(albumId)) {
      return AlbumMapping(albumId).copy()
    }else{return null}
  }

  def createAlbum(album:Album):String={
    if(AlbumMapping.contains(albumID) == false) {
      AlbumMapping += (albumID -> album)
      albumID = albumID + 1
      "Success: Successfully added album, ID="+(albumID).toString()
    }else{"Failure: Album with that ID already exists, error code=-1"}
  }

  def deleteAlbum(albumId:Int): String =
  {
    if(AlbumMapping.contains(albumId)) {
      AlbumMapping -= albumId
      "Success: Successfully deleted album, code=1"
    } else { "Failure: Album does not exist, error code=-1"}
  }

  def addPhotosToAlbum(albumId:Int, updatedPhotos:Album):String={
    if(AlbumMapping.contains(albumId)) {
      var old_album = AlbumMapping(albumId)
      var photoList = List[AlbumPhoto]()
      photoList = updatedPhotos.photos ::: photoList
      var description = updatedPhotos.description
      deleteAlbum(albumId)
      var new_album = new Album(old_album.ownerId, photoList, description)
      //createAlbum(albumId, new_album)
      AlbumMapping += (albumId -> new_album)
      "Success: Succesfully updated album=1"
    } else { "Failure: Album not found=-1"}
  }

  def deletePhotosFromAlbum(albumId:Int, photoIds:List[Int])={
    var old_album = AlbumMapping(albumId)
    var photoList = old_album.photos
    var photoList_new = List[AlbumPhoto]()
    /*for (photoId <- photoIds){
      for(photo <- photoList){
        if(photoId != photo.aPhotoID){
          photoList_new = photo :: photoList_new
        }
      }
    }*/
    deleteAlbum(albumId)
    var new_album = new Album(old_album.ownerId, photoList_new, old_album.description)
    //createAlbum(albumId, new_album)
  }

  def updateDescription(albumId:Int, description:String): Unit =
  {
    var old_album = AlbumMapping(albumId)
    deleteAlbum(albumId)
    var new_album = new Album(old_album.ownerId, old_album.photos, description)
    //createAlbum(albumId, new_album)
  }
}

object KeyProtocol{
  var PublicKeyMapping = scala.collection.mutable.Map[Int, String]()
  var PostKeyMapping = scala.collection.mutable.Map[Int, List[Tuple2[Int, String]]]()
  var PhotoKeyMapping = scala.collection.mutable.Map[Int, List[Tuple2[Int, String]]]()
  var AlbumKeyMapping = scala.collection.mutable.Map[Int, List[Tuple2[Int, String]]]()

  var SessionMapping = scala.collection.mutable.Map[Int, String]()

  implicit val cipherRsa:Cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")

  def returnPostKey(postid:Int, clientid: Int): String ={
    var x = PostKeyMapping(postid)
    var y = x.toMap
    return y(clientid)
  }
  def returnPhotoKey(photoid:Int, clientid: Int): String ={
    var x = PhotoKeyMapping(photoid)
    var y = x.toMap
    return y(clientid)
  }
  def returnAlbumKey(albumid:Int, clientid: Int): String ={
    var x = AlbumKeyMapping(albumid)
    var y = x.toMap
    return y(clientid)
  }

  def addSession(clientid:Int, sessiontoken:String) = {
    SessionMapping += (clientid -> sessiontoken)
  }

  def verifySession(clientid:Int, tokenstring:String): Boolean = {
    var token = SessionMapping(clientid)
    if(token == tokenstring) return true
    return false
  }

  def removeSession(clientid:Int, sessiontoken:String) = {
    if(verifySession(clientid, sessiontoken)) SessionMapping -= clientid
    else "Error!"
  }
  def generateToken():String = {
    var randomString=Random.alphanumeric.take(10).mkString
    return randomString
  }

  def encryptToken(tokenString:Array[Byte],clientid:Int) = {
    var publicKey = PublicKeyMapping(clientid)
    var publicbytes=Base64.decodeBase64(publicKey)
    var keyFactory = KeyFactory.getInstance("RSA");
    var publicKeySpec = new X509EncodedKeySpec(publicbytes)
    var publickey = keyFactory.generatePublic(publicKeySpec)
    cipherRsa.init(Cipher.ENCRYPT_MODE,publickey)
    Base64.encodeBase64String(cipherRsa.doFinal(tokenString))
  }
  //decrypt aes key
  def decryptToken(tokenString:String,clientid:Int) = {
    var publicKey1 = PublicKeyMapping(clientid)
    var publicbytes=Base64.decodeBase64(publicKey1)
    var keyFactory = KeyFactory.getInstance("RSA");
    var publicKeySpec = new X509EncodedKeySpec(publicbytes)
    var publicKey = keyFactory.generatePrivate(publicKeySpec)
    cipherRsa.init(Cipher.DECRYPT_MODE, publicKey )
    //new String(cipher.doFinal(Base64.decodeBase64(mysteryKey)))
    new String(cipherRsa.doFinal(Base64.decodeBase64(tokenString)))
  }

}

class ApiActor extends Actor with HttpService with ActorLogging {
  var friends = List[User]()
  var posts = List[Post]()
  var user = List[User]()
  var pages = List[Page]()
  var photo = List[Photo]()
  var album = List[Album]()

  def actorRefFactory = context

  def receive = runRoute(apiRoute)

  implicit val system = ActorSystem("FBsim")

  val friendactor = system.actorOf(Props(new FriendListWorker()))
  val useractor = system.actorOf(Props(new UserWorker()))
  val postactor = system.actorOf(Props(new PostWorker()))
  val pageactor = system.actorOf(Props(new PageWorker()))
  val photoactor = system.actorOf(Props(new PhotoWorker()))
  val albumactor = system.actorOf(Props(new AlbumWorker()))
  val initactor1 = system.actorOf(Props(new FriendListWorker()))
  val initactor2 = system.actorOf(Props(new UserWorker()))

  import UserProtocol._

  val apiRoute: Route =
    path("friendlist" / IntNumber / IntNumber) { (clientid, id) =>
      get {
        log.info("Building get route")

        complete {
          friends = List[User]() // This is the actual code
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(friendactor ? getFriendList(id, clientid), timeout.duration).asInstanceOf[List[User]]
          //friends
          future
        }
      } ~ put {
        log.info("Put route for friendlist")
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(friendactor ? putFriendList(id), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ post {
        log.info("Building post route")
        complete {
          /*implicit val timeout = Timeout(Duration(10, TimeUnit.SECONDS))
          val future = Await.result(friendactor ? postFriendList(id, clientid), timeout.duration).asInstanceOf[String]
          future*/
          initactor1 ! postFriendList(id, clientid)
          "Adding friends operation done"
        }
      } ~ delete {
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(friendactor ? deleteFriendList(id, clientid), timeout.duration).asInstanceOf[String]
          future
        }
      }
    } ~ path("user" / IntNumber / IntNumber ) { (id, clientid) =>
      get {
        user = List[User]()
        log.info("Entered get user")
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(useractor ? getUser(id, clientid), timeout.duration).asInstanceOf[List[User]]
          future
        }
      } ~ put {
        handleWith { user_new: User =>
          /*implicit val timeout = Timeout(Duration(10, TimeUnit.SECONDS))
          val future = Await.result(useractor ? putUser(id, user_new), timeout.duration).asInstanceOf[String]
          future*/
          initactor2 ! putUser(id, user_new)
          "Adding Users operation done"
        }
      } ~ post {
        handleWith { user_update: User =>
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(useractor ? postUser(id, user_update), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ delete {
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(useractor ? deleteUser(id, clientid), timeout.duration).asInstanceOf[String]
          future
        }
      }
    } ~ path("post" / IntNumber / IntNumber / Segment) { (id : Int, clientid : Int, token : String) =>
      get {
        posts = List[Post]()
        complete{
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(postactor ? getPost(id, clientid, token), timeout.duration).asInstanceOf[List[Post]]
          future
        }
      } ~ put {
        handleWith { post: Post =>
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(postactor ? putPost(id, post), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ post {
        handleWith { posty: Post =>
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(postactor ? postPost(id, posty), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ delete {
        complete{
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(postactor ? deletePost(id, clientid, token), timeout.duration).asInstanceOf[String]
          future
        }
      }
    } ~ path("page" / IntNumber / IntNumber) { (id, clientid) =>
      get {
        pages = List[Page]()
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(pageactor ? getPage(id, clientid), timeout.duration).asInstanceOf[List[Page]]
          future
        }
      } ~ put {
        handleWith { page: Page =>
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(pageactor ? putPage(id, page), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ post {
        pages = List[Page]()
        handleWith { page: Page =>
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(pageactor ? postPage(id, clientid, page), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ delete {
        complete {
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(pageactor ? deletePage(id, clientid), timeout.duration).asInstanceOf[String]
          future
        }
      }
    } ~ path("photo" / IntNumber / IntNumber) { (id, clientid) =>
      get {
        photo = List[Photo]()
        complete {
          /*if(clientid == PhotoProtocol.returnPhoto(id).ownerId || FriendListProtocol.isFriend(PhotoProtocol.returnPhoto(id).ownerId, clientid)) {
              photo = PhotoProtocol.returnPhoto(id) :: photo
            }
            photo*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(photoactor ? getPhoto(id, clientid), timeout.duration).asInstanceOf[List[Photo]]
          future
        }
      } ~ put {
        handleWith { newPhoto: Photo =>
          /*PhotoProtocol.addPhoto(id, newPhoto)*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(photoactor ? putPhoto(id, newPhoto), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ post {
        handleWith { updatePhoto: Photo =>
          /*if(PhotoProtocol.returnPhoto(id).ownerId == clientid) {
            PhotoProtocol.updatePhoto(id, updatePhoto)
          }
          "Photo is updated"*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(photoactor ? postPhoto(id, clientid, updatePhoto), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ delete {
        complete {
          /*if(PhotoProtocol.returnPhoto(id).ownerId == clientid){
            PhotoProtocol.returnPhoto(id)
          }
          "Photo is deleted"*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(photoactor ? deletePhoto(id, clientid), timeout.duration).asInstanceOf[String]
          future
        }
      }
    } ~ path("album" / IntNumber / IntNumber) { (id, clientid) =>
      get {
        album = List[Album]()
        complete {
          /*if (clientid == AlbumProtocol.returnAlbum(id).ownerId || FriendListProtocol.isFriend(AlbumProtocol.returnAlbum(id).ownerId, clientid)) {
              album = AlbumProtocol.returnAlbum(id) :: album
          }
          album*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(albumactor ? getAlbum(id, clientid), timeout.duration).asInstanceOf[List[Album]]
          future
        }
      } ~ put {
        handleWith { album: Album =>
          /* AlbumProtocol.createAlbum(id, album)
        "Album created"*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(albumactor ? putAlbum(id, clientid, album), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ post {
        handleWith { listofphotos: Album =>
          /*if(clientid == AlbumProtocol.returnAlbum(id).ownerId){
            AlbumProtocol.addPhotosToAlbum(id, listofphotos)
          }
          "Album updated"*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(albumactor ? postAlbum(id, clientid, listofphotos), timeout.duration).asInstanceOf[String]
          future
        }
      } ~ delete {
        complete {
          /*if(clientid == AlbumProtocol.returnAlbum(id).ownerId){
            AlbumProtocol.deleteAlbum(id)
          }
          "Album deleted"*/
          implicit val timeout = Timeout(Duration(60, TimeUnit.SECONDS))
          val future = Await.result(albumactor ? deleteAlbum(id, clientid), timeout.duration).asInstanceOf[String]
          future
        }
      }

    } ~ path("postkey" / IntNumber / IntNumber) { (postid, clientid) =>
      get {
        complete {
          val post = PostProtocol.returnPost(postid)
          var friendlist = FriendListProtocol.FriendList(post.ownerId)
          var keylist = List[Tuple2[Int, String]]()
          for (f_id <- friendlist) {
            if (UserMappingProtocol.checkUserExists(f_id)) {
              keylist = new Tuple2(f_id, KeyProtocol.PublicKeyMapping(f_id)) :: keylist
            }
          }
          keylist = new Tuple2(post.ownerId, KeyProtocol.PublicKeyMapping(post.ownerId)) :: keylist
          keylist
        }
      } ~ post {
        handleWith { aeskeylist: List[Tuple2[Int, String]] =>
          KeyProtocol.PostKeyMapping += (postid -> aeskeylist)
          "Success: Added to Post Key Mapping"
        }
      }
    } ~ path("pickey" / IntNumber / IntNumber) { (photoid, clientid) =>
      get {
        complete {
          val pic = PhotoProtocol.returnPhoto(photoid)
          var friendlist = FriendListProtocol.FriendList(pic.ownerId)
          var keylist = List[Tuple2[Int, String]]()
          for (f_id <- friendlist) {
            if (UserMappingProtocol.checkUserExists(f_id)) {
              keylist = new Tuple2(f_id, KeyProtocol.PublicKeyMapping(f_id)) :: keylist
            }
          }
          keylist = new Tuple2(pic.ownerId, KeyProtocol.PublicKeyMapping(pic.ownerId)) :: keylist
          keylist
        }
      } ~ post {
        handleWith { aeskeylist: List[Tuple2[Int, String]] =>
          KeyProtocol.PhotoKeyMapping += (photoid -> aeskeylist)
          "Success: Added to Post Key Mapping"
        }
      }
    }~ path("albkey" / IntNumber / IntNumber) { (albid, clientid) =>
      get {
        complete {
          val album = AlbumProtocol.returnAlbum(albid)
          var friendlist = FriendListProtocol.FriendList(album.ownerId)
          var keylist = List[Tuple2[Int, String]]()
          for (f_id <- friendlist) {
            if (UserMappingProtocol.checkUserExists(f_id)) {
              keylist = new Tuple2(f_id, KeyProtocol.PublicKeyMapping(f_id)) :: keylist
            }
          }
          keylist = new Tuple2(album.ownerId, KeyProtocol.PublicKeyMapping(album.ownerId)) :: keylist
          keylist
        }
      } ~ post {
        handleWith { aeskeylist: List[Tuple2[Int, String]] =>
          KeyProtocol.AlbumKeyMapping += (albid -> aeskeylist)
          "Success: Added to Post Key Mapping"
        }
      }
    }~ path("login" / IntNumber) { clientid =>
      get{
        complete{
          var token = KeyProtocol.generateToken()
          KeyProtocol.addSession(clientid, token)
          var encrypted = KeyProtocol.encryptToken(token.getBytes(), clientid)
          encrypted
        }
      }
    }
}

class FriendListWorker extends Actor{
  def receive ={
    case getFriendList(id, clientid) => {
      var friends = List[User]()
      var x = FriendListProtocol.findFriends(id)
      if(x != null) {
        if (clientid == id || x.contains(clientid)) {
          for (friendid <- x) {
            if(UserMappingProtocol.checkUserExists(friendid)) {
              friends = UserMappingProtocol.findUser(friendid) :: friends
            }
          }
        }
      }
      sender ! friends
    }
    case putFriendList(id) => {
      sender ! FriendListProtocol.createEmptyFriendList(id)
    }
    case postFriendList(id, clientid) => {
      var r1 = FriendListProtocol.addFriend(id, clientid)
      //var r2 = FriendListProtocol.addFriend(clientid, id)
      if(r1 == "Success"){
        sender ! "Success: Friend added successfully, code=1"
      }else{ sender ! "Failure: Friend not added, error code=-1"}

    }
    case deleteFriendList(id, clientid) => {
      //if(id == clientid) {
        sender ! FriendListProtocol.deleteFriendList(id, clientid)
      //} else{ sender ! "Failure: Client cannot delete, error code=-1"}
    }

  }
}

class UserWorker extends Actor{
  def receive ={
    case getUser(id, clientid) =>{
      var user = List[User]()
      var user_temp = UserMappingProtocol.findUser(id)
      if(user_temp != null) {
        if (clientid == id || FriendListProtocol.isFriend(clientid, id)) {
          //user_temp.gender = user_temp.gender + " " + FriendListProtocol.returnKey(id, clientid)
          user = user_temp :: user
        }
//        } else {
//          var user_temp2 = new User(user_temp.id, user_temp.name, "","", "", user_temp.gender, "")
//          user_temp2.education = user_temp2.education + " " + FriendListProtocol.returnKey(id, clientid)
//          user = user_temp2 :: user
//        }
      }
//      if(user != null){
//        user(0).education + " " + FriendListProtocol.returnKey(id, clientid)}
      sender ! user

    }
    case putUser(user_newid, user_new) => {
      sender ! UserMappingProtocol.addNewUser(user_newid, user_new)
    }
    case postUser(userid, updated_user) => {
      if(updated_user.id == userid) {
        sender ! UserMappingProtocol.updateUser(updated_user)
      } else{ sender ! "Failure: Client cannot update user" }
    }
    case deleteUser(id, clientid) => {
      if(clientid == id) {
        sender ! UserMappingProtocol.deleteUser(id)
      }else{ sender ! "Failure: Client cannot delete user" }
    }
  }
}

class PostWorker extends Actor{
  def receive = {
    case getPost(id, clientid, token) => {
        var posts = List[Post]()
      if(KeyProtocol.verifySession(clientid, token)) {
        var post = PostProtocol.returnPost(id)
        if (post != null) {
          if (clientid == post.ownerId || FriendListProtocol.isFriend(post.ownerId, clientid)) {
            post.pic = post.pic + " " + KeyProtocol.returnPostKey(id, clientid) + " " + KeyProtocol.PublicKeyMapping(post.ownerId)
            posts = post :: posts
            //println("Swarna: " + PostProtocol.returnPost(id))
          }
        }
        /*if(posts != null){
        println("Swarna: Code goes here")
        posts(0).pic + " " + FriendListProtocol.returnKey(posts(0).ownerId, clientid)
        println("Swarna: "+posts(0).pic)}*/
      }
        sender ! posts
    }
    case putPost(id, post) => {
      if(KeyProtocol.verifySession(post.ownerId, post.token)) {
        sender ! PostProtocol.addPost(post)
      }else{ sender ! "User Authentication failed! Error=-1"}
  }
    case postPost(id, posty) => {
      if(KeyProtocol.verifySession(posty.ownerId, posty.token)) {
        var post_old = PostProtocol.returnPost(id)
        if (post_old != null) {
          if (posty.ownerId == post_old.ownerId) {
            sender ! PostProtocol.updatePost(id, posty)
          } else {
            sender ! "Failure: Client cannot update post=-1"
          }
        } else {
          sender ! "Failure: Post does not exist=-1"
        }
      }else{ sender ! "User Authentication failed! Error=-1"}
    }
    case deletePost(id, clientid, token) => {
      if (KeyProtocol.verifySession(clientid, token)) {
        var post = PostProtocol.returnPost(id)
        if (post != null) {
          if (clientid == post.ownerId) {
            sender ! PostProtocol.deletePost(id)
          } else {
            sender ! "Failure: Client cannot delete post, error code=-1"
          }
        } else {
          sender ! "Failure: Post does not exist, error code=-1"
        }
      }
    }
  }
}

class PageWorker extends Actor{
  def receive = {
    case getPage(id, clientid) =>{
      var pages = List[Page]()
      var page = PageProtocol.returnPage(id)
      if(page != null) {
        pages = page :: pages
      }
      sender ! pages
    }
    case putPage(id, page) =>{
      sender ! PageProtocol.addPage(page)
    }
    case postPage(id, clientid, page) =>{
      var page_old = PageProtocol.returnPage(id)
      if(page_old != null) {
        if (clientid == page_old.ownerId) {
          sender ! PageProtocol.updatePage(id, page)
        }else{ sender ! "Failure: Client cannot update page"}
      }else{ sender ! "Failure: Page does not exist"}
    }
    case deletePage(id, clientid) =>{
      var page = PageProtocol.returnPage(id)
      if(page != null) {
        if (clientid == page.ownerId) {
          sender ! PageProtocol.deletePage(id)
        } else {
          sender ! "Failure: Client cannot delete page, error code=-1"
        }
      } else{ sender ! "Page does not exist, error code=-1" }
    }
  }
}

class PhotoWorker extends Actor{
  def receive = {
    case getPhoto(id, clientid) => {
      var photo = List[Photo]()
      var pic = PhotoProtocol.returnPhoto(id)
      if(pic != null) {
        if (clientid == pic.ownerId || FriendListProtocol.isFriend(pic.ownerId, clientid)) {
          pic.url = pic.url + " " + KeyProtocol.returnPhotoKey(id, clientid)+" "+KeyProtocol.PublicKeyMapping(pic.ownerId)
          photo = pic :: photo
        }
      }
//      if(photo != null){
//        photo(0).caption + " " + FriendListProtocol.returnKey(photo(0).ownerId, clientid)}
      sender ! photo
    }
    case putPhoto(id, newPhoto) =>{
      sender ! PhotoProtocol.addPhoto(newPhoto)
    }
    case postPhoto(id, clientid, updatePhoto) =>{
      var pic = PhotoProtocol.returnPhoto(id)
      if(pic != null) {
        if (pic.ownerId == clientid) {
          sender ! PhotoProtocol.updatePhoto(id, updatePhoto)
        } else { sender ! "Client does not have acceess=-1" }
      } else { sender ! "Photo does not exist=-1"}
    }
    case deletePhoto(id, clientid) =>{
      var pic = PhotoProtocol.returnPhoto(id)
      if(pic != null) {
        if (pic.ownerId == clientid) {
          sender ! PhotoProtocol.deletePhoto(id)
        } else { sender ! "Client does not have acceess, error code=-1" }
      } else { sender ! "Photo does not exist, error code=-1" }
    }
  }
}

class AlbumWorker extends Actor{
  def receive ={
    case getAlbum(id, clientid)=>{
      var album = List[Album]()
      var alb = AlbumProtocol.returnAlbum(id)
      if(alb != null) {
        if (clientid == alb.ownerId || FriendListProtocol.isFriend(alb.ownerId, clientid)) {
          alb.description = alb.description + " " +KeyProtocol.returnAlbumKey(id, clientid)
          album = alb :: album
        }
      }
//      if(album != null){
//        album(0).description + " " + FriendListProtocol.returnKey(album(0).ownerId, clientid)}
      sender ! album
    }
    case putAlbum(id, clientid, album) =>{
      sender ! AlbumProtocol.createAlbum(album)
    }
    case postAlbum(id, clientid, listofphotos) =>{
      var alb = AlbumProtocol.returnAlbum(id)
      if(alb != null) {
        if (clientid == alb.ownerId) {
         sender ! AlbumProtocol.addPhotosToAlbum(id, listofphotos)
        } else { sender ! "Client does not have acceess=-1" }
      }else{ sender ! "Album with that id does not exist=-1"}
    }
    case deleteAlbum(id, clientid) =>{
      var alb = AlbumProtocol.returnAlbum(id)
      if(alb != null) {
        if (clientid == alb.ownerId) {
          sender ! AlbumProtocol.deleteAlbum(id)
        }  else { sender ! "Client does not have acceess, error code=-1" }
      } else { sender ! "Album with that id does not exists, error code=-1"}
    }
  }
}

case class getFriendList(id:Int, clientid:Int)
case class putFriendList(id:Int)
case class postFriendList(id:Int, clientid:Int)
case class deleteFriendList(id:Int, clientid:Int)

case class getUser(id:Int, clientid:Int)
case class putUser(new_userid:Int, user_new:User)
case class postUser(userid:Int, updated_user:User)
case class deleteUser(id:Int, clientid:Int)

case class getPost(id:Int, clientid:Int, token:String)
case class putPost(id:Int, post:Post)
case class postPost(id:Int, post:Post)
case class deletePost(id:Int, clientid:Int, token:String)

case class getPage(id:Int, clientid:Int)
case class putPage(id:Int, page:Page)
case class postPage(id:Int, clientid:Int, page:Page)
case class deletePage(id:Int, clientid:Int)

case class getPhoto(id:Int, clientid:Int)
case class putPhoto(id:Int, photo:Photo)
case class postPhoto(id:Int, clientid:Int, photo:Photo)
case class deletePhoto(id:Int, client:Int)

case class getAlbum(id:Int, clientid:Int)
case class putAlbum(id:Int, clientid:Int, album:Album)
case class postAlbum(id:Int, clientid:Int, listofphotos:Album)
case class deleteAlbum(id:Int, clientid:Int)



