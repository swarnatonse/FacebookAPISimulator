/**
 * Created by canara on 11/25/2015.
 */

import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPairGenerator, SecureRandom}
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.apache.commons.codec.binary.Base64
import spray.client.pipelining._
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

//import scala.concurrent.{Await, Future}

import java.util.concurrent.TimeUnit

import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ClientObjects extends DefaultJsonProtocol {

  case class ClientInfo(requesterid: Int)

  case class User(id: Int, name: String, about: String, age: String, education: String,var gender: String, location: String)

  case class FbPost(ownerId: Int, caption: String, content: String, pic: String,token:String)

  case class Page(ownerId: Int, name: String, description: String, pagetype: String, location: String)

  case class Photo(ownerId: Int, url: String, caption: String, albumId: String)

  case class Album(ownerId: Int, photos: List[AlbumPhoto], description: String)

  case class AlbumPhoto( url: String)

  case class Id(id: String)

  implicit val UserFormat = jsonFormat7(User)
  implicit val PostFormat = jsonFormat5(FbPost)
  implicit val ClientFormat = jsonFormat1(ClientInfo)
  implicit val PageFormat = jsonFormat5(Page)
  implicit val AlbumPhotoFormat = jsonFormat1(AlbumPhoto)
  implicit val IdFormat = jsonFormat1(Id)
  implicit val PhotoFormat = jsonFormat4(Photo)
  implicit val AlbumFormat = jsonFormat3(Album)
}

import ClientObjects._

object Client extends App {
  // initialize variables according to statistics
  var numOfClients: Int = args(0).toInt
  var numOfFriends = (numOfClients * 0.25).toInt
  var totalFriends = numOfClients * numOfFriends
  var male = (numOfClients * 0.55).toInt
  var female = numOfClients - male
  var activeUsers = (numOfClients * 0.65).toInt
  var passiveUsers=numOfClients-activeUsers
  var count = 0
  var age = 0
  var postsCount=1
  var pagesCount=1
  var photosCount=1
  var albumsCount=1
  implicit val system = ActorSystem("Client")
  var clientList: List[ActorRef] = Nil
  implicit val timeouts = Timeout(Duration(60, TimeUnit.SECONDS))
  //implicit val gettimeouts = Timeout(Duration(30, TimeUnit.SECONDS))

  //initialize network

  //create client nodes
  for (i <- 0 to numOfClients - 1) {
    clientList ::= system.actorOf(Props[ClientWorker], name = "clientNode" + i)
  }
  //create users
  while (count < numOfClients) {
    age = 0
    while (age > 12) {
      age = Random.nextInt(80)
    }
    if (count <= male) {
      var newUser = User(count, "Client" + count, "I am a facebook user", age.toString, "University", "male", "USA")
      Await.result(clientList(count) ? createUser(count, newUser), timeouts.duration).asInstanceOf[String]
    }
    else {
      var newUser = User(count, "Client" + count, "I am a facebook user", age.toString, "University", "female", "USA")
      Await.result(clientList(count) ? createUser(count, newUser), timeouts.duration).asInstanceOf[String]
    }

    count = count + 1
  }
  //log in
  count=0
  while (count < numOfClients) {
    Await.result(clientList(count) ? logIn(count), timeouts.duration).asInstanceOf[String]
    count=count+1
  }
    // initialize friend list
  for (i <- 0 to numOfClients - 1) {
    Await.result(clientList(i) ? initFriendList(i), timeouts.duration).asInstanceOf[String]
  }
//   add friends
  count = 0
  while (count <= totalFriends) {
    var friendId = Random.nextInt(numOfClients)
    var clientId = Random.nextInt(numOfClients)
    if (friendId != clientId) {
      Await.result(clientList(clientId) ? addFriend(clientId, friendId,clientList), timeouts.duration).asInstanceOf[String]
      Await.result(clientList(friendId) ? addFriend(friendId, clientId,clientList), timeouts.duration).asInstanceOf[String]
      count = count + 1
    }
  }

 //schedule functions
  try {
    //active users behaviour
    //active users scheduling: creating posts,pages,photos and albums
    system.scheduler.schedule(0 seconds, 0.5 seconds)(sendCreatePost(Random.nextInt(activeUsers)))
    system.scheduler.schedule(0 seconds, 3 seconds)(sendCreatePage(Random.nextInt(activeUsers)))
    system.scheduler.schedule(0 seconds, 1 seconds)(sendUploadPic(Random.nextInt(activeUsers)))
    system.scheduler.schedule(0 seconds, 1 seconds)(sendUploadAlbum(Random.nextInt(activeUsers)))

    //active users scheduling: add friends,get friend list, unfriend
    system.scheduler.schedule(0 seconds, 5 seconds)(sendGetFriendList(Random.nextInt(activeUsers), Random.nextInt(numOfClients - 1)))
    system.scheduler.schedule(0 seconds, 10 seconds)(sendUnFriend(Random.nextInt(activeUsers), Random.nextInt(numOfClients - 1)))

    //active users scheduling: get, update and delete posts
    system.scheduler.schedule(10 seconds, 0.05 seconds)(sendGetPost(Random.nextInt(activeUsers), Random.nextInt(postsCount)))
    system.scheduler.schedule(10 seconds, 5 seconds)(sendUpdatePost(Random.nextInt(activeUsers)))
    system.scheduler.schedule(10 seconds, 10 seconds)(sendDeletePost(Random.nextInt(activeUsers)))

    //active users scheduling:get, update and delete page
    system.scheduler.schedule(10 seconds, 0.5 seconds)(sendGetPage(Random.nextInt(activeUsers), Random.nextInt(pagesCount)))
    system.scheduler.schedule(10 seconds, 10 seconds)(sendDeletePage(Random.nextInt(activeUsers)))
    system.scheduler.schedule(10 seconds, 5 seconds)(sendUpdatePage(Random.nextInt(activeUsers)))

    //active users scheduling:get, update and delete photo
    system.scheduler.schedule(10 seconds, 0.5 seconds)(sendGetPic(Random.nextInt(activeUsers), Random.nextInt(photosCount)))
  system.scheduler.schedule(10 seconds, 10 seconds)(sendDeletePic(Random.nextInt(activeUsers)))
  system.scheduler.schedule(10 seconds, 5 seconds)(sendUpdatePic(Random.nextInt(activeUsers)))

    //active users scheduling:get, update and delete album
    system.scheduler.schedule(10 seconds, 0.5 seconds)(sendGetAlbum(Random.nextInt(activeUsers), Random.nextInt(albumsCount)))
    system.scheduler.schedule(10 seconds, 10 seconds)(sendDeleteAlbum(Random.nextInt(activeUsers)))
    system.scheduler.schedule(10 seconds, 5 seconds)(sendUpdateAlbum(Random.nextInt(activeUsers)))

    //passive users behaviour
    //passive users scheduling: creating posts,pages,photos and albums
    system.scheduler.schedule(0 seconds, 10 seconds)(sendCreatePost(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(0 seconds, 30 seconds)(sendCreatePage(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(0 seconds, 15 seconds)(sendUploadPic(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(0 seconds, 15 seconds)(sendUploadAlbum(activeUsers+Random.nextInt(passiveUsers-1)))

    //passive users scheduling: add friends,get friend list, unfriend
    system.scheduler.schedule(0 seconds, 15 seconds)(sendGetFriendList(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(numOfClients - 1)))
   system.scheduler.schedule(0 seconds, 30 seconds)(sendUnFriend(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(numOfClients - 1)))

    // passive users scheduling: get, update and delete posts
    system.scheduler.schedule(30 seconds, 3 seconds)(sendGetPost(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(postsCount)))
    system.scheduler.schedule(30 seconds, 30 seconds)(sendUpdatePost(activeUsers+Random.nextInt(passiveUsers-1)))
   system.scheduler.schedule(30 seconds, 40 seconds)(sendDeletePost(activeUsers+Random.nextInt(passiveUsers-1)))

    // passive users scheduling:get, update and delete page
    system.scheduler.schedule(30 seconds, 5 seconds)(sendGetPage(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(pagesCount)))
    system.scheduler.schedule(30 seconds, 60 seconds)(sendDeletePage(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(30 seconds, 40 seconds)(sendUpdatePage(activeUsers+Random.nextInt(passiveUsers-1)))

//    // passive users scheduling:get, update and delete photo
    system.scheduler.schedule(30 seconds, 8 seconds)(sendGetPic(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(photosCount)))
    system.scheduler.schedule(30 seconds, 40 seconds)(sendDeletePic(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(30 seconds, 25 seconds)(sendUpdatePic(activeUsers+Random.nextInt(passiveUsers-1)))

    // passive users scheduling:get, update and delete album
    system.scheduler.schedule(40 seconds, 8 seconds)(sendGetAlbum(activeUsers+Random.nextInt(passiveUsers-1), Random.nextInt(albumsCount)))
    system.scheduler.schedule(40 seconds, 40 seconds)(sendDeleteAlbum(activeUsers+Random.nextInt(passiveUsers-1)))
    system.scheduler.schedule(40 seconds, 25 seconds)(sendUpdateAlbum(activeUsers+Random.nextInt(passiveUsers-1)))


    //calling add friends,get friend list, unfriend
  def sendAddFriend(id:Int, friendid:Int)={
    Await.result(clientList(id) ? addFriend(id, friendid,clientList), timeouts.duration).asInstanceOf[String]
      Await.result(clientList(friendid) ? addFriend(friendid, id,clientList), timeouts.duration).asInstanceOf[String]
  }
  def sendGetFriendList(clientId:Int, requestedId:Int)={
    Await.result(clientList(clientId) ? getFriendList(clientId, requestedId), timeouts.duration).asInstanceOf[String]
  }
  def sendUnFriend(clientId: Int, requestedId: Int)={
    Await.result(clientList(clientId) ? unFriend(clientId, requestedId), timeouts.duration).asInstanceOf[String]
  }

  //calling get, update and delete users
  def sendDeleteAccount(clientId: Int)={
    Await.result(clientList(clientId) ? deleteAccount(clientId), timeouts.duration).asInstanceOf[String]
  }
  def sendUpdateUserProfile(clientId: Int)={
    var updatedProfile = User(clientId, "Client"+clientId, "I have an updated profile, name:client"+clientId, 30.toString, "Masters",  "male", "UK" )
    Await.result(clientList(clientId) ? updateUserProfile(clientId, updatedProfile), timeouts.duration).asInstanceOf[String]
  }
  def sendGetUserProfile(clientId: Int, requestedId: Int)={
    Await.result(clientList(clientId) ? getUserProfile(clientId, requestedId), timeouts.duration).asInstanceOf[String]
  }
  def sendCreateUser(clientId: Int)={
    numOfClients=numOfClients+1
    var userProf = User(clientId, "Client"+clientId, "I am a new user, name:client"+clientId, 15.toString, "High School",  "Female", "India" )
    clientList ::= system.actorOf(Props[ClientWorker], name = "clientNode" + clientId)
    Await.result(clientList(clientId) ? createUser(clientId, userProf), timeouts.duration).asInstanceOf[String]

  }
  //calling get, update and delete post
  def sendCreatePost(id:Int): Unit =
  {
    var newPost = FbPost(id,"My wishes","Happy thanks giving by client"+id,"https://cdn4.dogonews.com/images/b7c9fc4e-8cbf-492b-bc9d-df6f29f76700/happy-thanksgiving.jpg","")
    var num= Await.result(clientList(id) ? createPost(id, newPost), timeouts.duration).asInstanceOf[Int]
    if(num>0){
      postsCount=num
    }
  }
  def sendGetPost(clientId:Int, postId:Int)={
    Await.result(clientList(clientId) ? getPost(clientId, postId), timeouts.duration).asInstanceOf[String]
  }
  def sendUpdatePost(clientId:Int)={
    var postContent=FbPost(clientId,"Santa Time!!!","Happy Christmas by client:"+clientId,"http://visitnorthplatte.com/wp-content/uploads/2014/12/merry-christmas.jpg","")
    Await.result(clientList(clientId) ? updatePost(clientId, postContent), timeouts.duration).asInstanceOf[String]
  }
  def sendDeletePost(clientId:Int)={
    Await.result(clientList(clientId) ? deletePost(clientId), timeouts.duration).asInstanceOf[String]
  }

  //calling get, update and delete page
  def sendGetPage(clientId:Int, pageId:Int)={
    Await.result(clientList(clientId) ? getPage(clientId, pageId), timeouts.duration).asInstanceOf[String]
  }
  def sendDeletePage(clientId:Int)={
    Await.result(clientList(clientId) ? deletePage(clientId), timeouts.duration).asInstanceOf[String]
  }
  def sendUpdatePage(clientId:Int)={
    var pageContent =  Page(clientId,"Nexus 6p","Nexus updated page, Admin:client"+clientId,"Electronics&Gadgets","China,United States of America")
    Await.result(clientList(clientId) ? updatePage(clientId, pageContent), timeouts.duration).asInstanceOf[String]
  }
  def sendCreatePage(clientId:Int)={
    var newPage =  Page(clientId,"Nexus 6p","Nexus, Admin:client"+clientId,"Electronics","United States of America")
   var num=Await.result(clientList(clientId) ? createPage(clientId, newPage), timeouts.duration).asInstanceOf[Int]
    if(num>0){
      pagesCount=num
    }
  }

  //calling get, update and delete photo
  def sendGetPic(clientId:Int,picId:Int)={
    Await.result(clientList(clientId) ? getPic(clientId, picId), timeouts.duration).asInstanceOf[String]
  }
  def sendDeletePic(clientId:Int)={
    Await.result(clientList(clientId) ? deletePic(clientId), timeouts.duration).asInstanceOf[String]
  }
  def sendUpdatePic(clientId:Int)={
    var picContent =  Photo(clientId,"http://cdn.listaka.com/wp-content/uploads/2015/06/siwzerland-jokose.jpg","Switzerland","0")
    Await.result(clientList(clientId) ? updatePic(clientId, picContent), timeouts.duration).asInstanceOf[String]
  }
  def sendUploadPic(clientId:Int)={
    var newPic =  Photo(clientId,"http://drupal.in-cdn.net/cdn/article/public/moving_to_norway_0.jpg","Norway","0")
    var num=Await.result(clientList(clientId) ? uploadPic(clientId, newPic), timeouts.duration).asInstanceOf[Int]
    if(num>0){
      photosCount=num
    }
  }

  //calling get, update and delete album
  def sendGetAlbum(clientId:Int,albumId:Int)={
    Await.result(clientList(clientId) ? getAlbum(clientId, albumId), timeouts.duration).asInstanceOf[String]
  }
  def sendUpdateAlbum(clientId:Int)={
    var photoList=List[AlbumPhoto]()
    photoList=new AlbumPhoto("https://cdn4.dogonews.com/images/b7c9fc4e-8cbf-492b-bc9d-df6f29f76700/happy-thanksgiving.jpg") :: photoList
    photoList=new AlbumPhoto("https://www.westbuxtonpubliclibrary.org/wp-content/uploads/2015/08/Labour-Day-Clip-Art-3.gif") :: photoList
    photoList=new AlbumPhoto("http://visitnorthplatte.com/wp-content/uploads/2014/12/merry-christmas.jpg") :: photoList
    var description = "This is an updated album!"
    var album = new Album(clientId, photoList, description)
    Await.result(clientList(clientId) ? updateAlbum(clientId, album), timeouts.duration).asInstanceOf[String]
  }
  def sendUploadAlbum(clientId:Int)={
    var photoList=List[AlbumPhoto]()
    photoList=new AlbumPhoto("https://cdn4.dogonews.com/images/b7c9fc4e-8cbf-492b-bc9d-df6f29f76700/happy-thanksgiving.jpg") :: photoList
    photoList=new AlbumPhoto("https://www.westbuxtonpubliclibrary.org/wp-content/uploads/2015/08/Labour-Day-Clip-Art-3.gif") :: photoList
    val albumContent =  Album(clientId,photoList,"wishes album")
    var num=Await.result(clientList(clientId) ? uploadAlbum(clientId, albumContent), timeouts.duration).asInstanceOf[Int]
    if(num>0){
      albumsCount=num
    }
  }
  def sendDeleteAlbum(clientId:Int)={
    Await.result(clientList(clientId) ? deleteAlbum(clientId), timeouts.duration).asInstanceOf[String]
  }
  } catch{
    case _: Throwable => println("Failure: Not able to access!")
  }
 // system.shutdown()

}

object Security{
  //Initialization Vector
  protected def initializationVector(bytes:Array[Byte]) = new IvParameterSpec(bytes)

  //generate salt
  def generateSalt():String = {
    var randomString=Random.alphanumeric.take(16).mkString
    return randomString
  }
  //encrypt data aes
  def encryptDataAES(input:Array[Byte],salt:Array[Byte],secretKeyEncoded:String)(implicit cipher:Cipher) = {
    var initVector = (initializationVector(salt))
    var key=new SecretKeySpec(Base64.decodeBase64(secretKeyEncoded), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, key, initVector)
    Base64.encodeBase64String(cipher.doFinal(input))

  }
  //decrypt data aes
  def decryptDataAES(input:String,salt:Array[Byte],decryptedSecretKey:String)(implicit cipher:Cipher) = {
    var initVector = (initializationVector(salt))
    var key=new SecretKeySpec(Base64.decodeBase64(decryptedSecretKey), "AES")
    cipher.init(Cipher.DECRYPT_MODE, key, initVector)
    new String(cipher.doFinal(Base64.decodeBase64(input)))
  }
  //encrypt aes key
  def encryptKeyRSA(aesSecretKey:Array[Byte],cipher: Cipher,frndPublicKey:String) = {
    var publicbytes=Base64.decodeBase64(frndPublicKey)
    var keyFactory = KeyFactory.getInstance("RSA");
    var publicKeySpec = new X509EncodedKeySpec(publicbytes)
    var publickey = keyFactory.generatePublic(publicKeySpec)
    cipher.init(Cipher.ENCRYPT_MODE,publickey)
    Base64.encodeBase64String(cipher.doFinal(aesSecretKey))
  }
  //decrypt aes key
  def decryptKeyRSA(mysteryKey:String,cipher: Cipher,privateKeyBytes:Array[Byte]) = {
    var keyFactory = KeyFactory.getInstance("RSA");
    var privateKeySpec = new PKCS8EncodedKeySpec(privateKeyBytes)
    var privateKey = keyFactory.generatePrivate(privateKeySpec)
    cipher.init(Cipher.DECRYPT_MODE, privateKey )
    //new String(cipher.doFinal(Base64.decodeBase64(mysteryKey)))
    new String(cipher.doFinal(Base64.decodeBase64(mysteryKey)))
  }

}
class ClientWorker extends Actor {
  //import ClientObjects._
  val apiLocation = "http://localhost:8080"
  val timeout = 60.seconds
  implicit val timeouts = Timeout(Duration(60, TimeUnit.SECONDS))
  var postList: ArrayBuffer[Int]=ArrayBuffer[Int]()
  var pageList: ArrayBuffer[Int]=ArrayBuffer[Int]()
  var photoList: ArrayBuffer[Int]=ArrayBuffer[Int]()
  var albumList: ArrayBuffer[Int]=ArrayBuffer[Int]()
  var token:String=new String()
  //cipher
  implicit val cipherAes:Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
  implicit val cipherRsa:Cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")

  //RSA
  var keyPairGenerator = KeyPairGenerator.getInstance("RSA");
  keyPairGenerator.initialize(1024);
  var keyPair = keyPairGenerator.genKeyPair()
  var privateKey= keyPair.getPrivate()
  var publicKey=keyPair.getPublic()

  // Public key encoding to string
  var x509EncodedKeySpec = new X509EncodedKeySpec(publicKey.getEncoded())
  var publicKeyBytes=x509EncodedKeySpec.getEncoded()
  var publicKeyStr= Base64.encodeBase64String(publicKeyBytes)

  // Private key encoding to string
  var pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(privateKey.getEncoded());
  var privateKeyBytes=pkcs8EncodedKeySpec.getEncoded()
  var privateKeyStr= Base64.encodeBase64String(privateKeyBytes)

  //AES
  //secretKey

def generateSecretKey(): String ={
  var random=new SecureRandom()
  var aesKey = new Array[Byte](16)
  random.nextBytes(aesKey)
  return Base64.encodeBase64String(aesKey)
}
  def receive = {
    //initialize friend list
    case initFriendList(clientId) => {
      println("Client"+clientId+": Initialize my friend list")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Put(s"$apiLocation/friendlist/$clientId/$clientId"))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }
    // add friend
    case addFriend(clientId, friendId,clientList) => {
      println("Client"+clientId+": Add Client"+ friendId + " as my friend")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$apiLocation/friendlist/$friendId/$clientId"))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }
    // get friend list
    case getFriendList(clientId, requestedId) => {
      println("Client"+clientId+": Get friend list of Client"+ requestedId)
      val pipeline: HttpRequest => Future[List[User]] = sendReceive ~> unmarshal[List[User]]
      val f: Future[List[User]] = pipeline(Get(s"$apiLocation/friendlist/$requestedId/$clientId"))
      val friendList = Await.result(f, timeout)
      if(friendList.size==0){
        println("Failure:Does not have access to friend list")
      }
      else {
        for (i <- 0 to friendList.size - 1) {
          println(friendList(i))
        }
      }
      sender ! "Request processed"
    }
    //unfriend a person
    case unFriend(clientId, requestedId) => {
      println("Client"+clientId+": Unfriend Client"+ requestedId)
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Delete(s"$apiLocation/friendlist/$requestedId/$clientId"))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }
    //create an account
    case createUser(clientId,newUser) => {
      println("Client"+clientId+": Create my profile")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      newUser.gender=newUser.gender+" "+publicKeyStr
      val f: Future[String] = pipeline(Put(s"$apiLocation/user/$clientId/$clientId", newUser))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }
    //login
    case logIn(clientId)=>{
      println("Client"+clientId+": Logging In...")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Get(s"$apiLocation/login/$clientId"))
      val encryptedToken = Await.result(f, timeout)
      token=Security.decryptKeyRSA(encryptedToken,cipherRsa,privateKeyBytes)
     // println(token)
      println(s"Server: Login of client $clientId successful")
      sender ! "Request processed"
    }
    //get user profile
    case getUserProfile(clientId, requestedId) => {
      println("Client"+clientId+": Get profile of Client"+ requestedId)
      val pipeline: HttpRequest => Future[List[User]] = sendReceive ~> unmarshal[List[User]]
      val f: Future[List[User]] = pipeline(Get(s"$apiLocation/user/$requestedId/$clientId"))
      val userProfile = Await.result(f, timeout)
      if(userProfile.size==0){
        println("Failure:Does not have access to user profile")
      }
      else {
        for (i <- 0 to userProfile.size - 1) {
          println(userProfile(i))
        }
      }
      sender ! "Request processed"
    }
    //update user profile
    case updateUserProfile(clientId,updatedProfile) => {
      println("Client"+clientId+": Update my profile")
      updatedProfile.gender=updatedProfile.gender+" "+publicKeyStr
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$apiLocation/user/$clientId/$clientId", updatedProfile))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }

    //delete account from fb
    case deleteAccount(clientId) => {
      println("Client"+clientId+": Delete my account")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Delete(s"$apiLocation/user/$clientId/$clientId"))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      sender ! "Request processed"
    }
    //get post
    case getPost(clientId,postId) => {
      println("Client" + clientId + ": Get post" + postId)
      val pipeline: HttpRequest => Future[List[FbPost]] = sendReceive ~> unmarshal[List[FbPost]]
      val f: Future[List[FbPost]] = pipeline(Get(s"$apiLocation/post/$postId/$clientId/$token"))
      val fbPost = Await.result(f, timeout)
      if (fbPost.size == 0) {
        println("Failure:Does not have access to the post")
      }
      else {
        for (i <- 0 to fbPost.size - 1) {
          var picField:String=fbPost(i).pic.toString
          var tokens = picField.split(" ")
          var pic=tokens(0)
          var saltString = tokens(1)
          var encryptedSecretKey=tokens(2)

            var decryptedSecretKey=Security.decryptKeyRSA(encryptedSecretKey,cipherRsa,privateKeyBytes)
            var decryptedpic = Security.decryptDataAES(pic, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            var decryptedcontent = Security.decryptDataAES(fbPost(i).content, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            var decryptedcaption = Security.decryptDataAES(fbPost(i).caption, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            var decryptedpost = new FbPost(fbPost(i).ownerId, decryptedcaption, decryptedcontent, decryptedpic,"")
            println(decryptedpost)

        }
      }
      sender ! "Request processed"
    }
    //create post
    case createPost(clientId,newPost) => {
      var saltString=Security.generateSalt()
      var mySecretKey=generateSecretKey()
      var encryptedPost = FbPost(clientId,Security.encryptDataAES(newPost.caption.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes),Security.encryptDataAES((newPost.content+clientId).getBytes(),saltString.getBytes(),mySecretKey)(cipherAes),Security.encryptDataAES(newPost.pic.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes)+" "+saltString,token)
      var postCount=0
      println("Client" + clientId + ": Create new post")
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Put(s"$apiLocation/post/$clientId/$clientId/foobar", encryptedPost))
      val msg = Await.result(f, timeout)
      println(s"Server: $msg")
      var tokens=msg.split("=")
      var id=tokens(1).toInt-1

      //if post creation success
      if(id>0) {
        postList+=id
        postCount=id

        //request public keys of friends from server
        val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
        val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/postkey/$id/$clientId"))
        val publicKeyList = Await.result(fGetKey, timeout)

        //encrypt secret key with public keys
        if(publicKeyList.size>0){
          var encSecretKeys = List[Tuple2[Int, String]]()
          for(i<-0 to publicKeyList.size-1){
            var encryptedSecretKey=Security.encryptKeyRSA(mySecretKey.getBytes(),cipherRsa,publicKeyList(i)._2)
            encSecretKeys=new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
          }

          //send the encrypted secret key list to server
          val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/postkey/$id/$clientId", encSecretKeys))
          val msg = Await.result(fSendKeys, timeout)
        }
      }
      sender ! postCount
    }

    //update post
      case updatePost(clientId,postContent) => {
        if(postList.size==0){
          println("Client" + clientId + ": update post")
          println("No posts exists")
        }
        else {
          var postId = postList(Random.nextInt(postList.size))
          println("Client" + clientId + ": Update post" + postId)
          var saltString = Security.generateSalt()
          var mySecretKey = generateSecretKey()
          var encryptedPost = FbPost(clientId, Security.encryptDataAES(postContent.caption.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes), Security.encryptDataAES(postContent.content.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes), Security.encryptDataAES(postContent.pic.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes) + " " + saltString,token)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Post(s"$apiLocation/post/$postId/$clientId/foobar", encryptedPost))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt

          //if post updation success
          if (id > 0) {
            //request public keys of friends from server
            val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
            val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/postkey/$postId/$clientId"))
            val publicKeyList = Await.result(fGetKey, timeout)

            //encrypt secret key with public keys
            if (publicKeyList.size > 0) {
              var encSecretKeys = List[Tuple2[Int, String]]()
              for (i <- 0 to publicKeyList.size - 1) {
                var encryptedSecretKey = Security.encryptKeyRSA(mySecretKey.getBytes(), cipherRsa, publicKeyList(i)._2)
                encSecretKeys = new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
              }

              //send the encrypted secret key list to server
              val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
              val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/postkey/$postId/$clientId", encSecretKeys))
              val msg = Await.result(fSendKeys, timeout)
            }
          }
        }
          sender ! "Request processed"

      }
    //delete post
      case deletePost(clientId)=>{
        if(postList.size==0){
          println("Client" + clientId + ": Delete post")
          println("No posts exists")
        }
        else {
          var postId = postList(Random.nextInt(postList.size))
          println("Client" + clientId + ": Delete post" + postId)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Delete(s"$apiLocation/post/$postId/$clientId/$token"))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt
          if (id == 1) {
            postList -= postId
          }
        }
        sender ! "Request processed"
      }
      //get page
      case getPage(clientId,pageId) => {
        println("Client" + clientId + ": Get page"+pageId)
        val pipeline: HttpRequest => Future[List[Page]] = sendReceive ~> unmarshal[List[Page]]
        val f: Future[List[Page]] = pipeline(Get(s"$apiLocation/page/$pageId/$clientId"))
        val fbPage = Await.result(f, timeout)
        if (fbPage.size == 0) {
          println("Failure:Does not have access to the page")
        }
        else {
          for (i <- 0 to fbPage.size - 1) {
            println(fbPage(i))
          }
        }
        sender ! "Request processed"
      }
    //delete page
      case deletePage(clientId)=>{
        if(pageList.size==0){
          println("Client" + clientId + ": Delete page")
          println("No pages exists")
        }
        else {
          var pageId = pageList(Random.nextInt(pageList.size))
          println("Client" + clientId + ": Delete page" + pageId)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Delete(s"$apiLocation/page/$pageId/$clientId"))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt
          if (id == 1) {
            pageList -= pageId
          }
        }
          sender ! "Request processed"

      }
    //update page
      case updatePage(clientId,pageContent) => {
        if(pageList.size==0){
          println("Client" + clientId + ": update page")
          println("No pages exists")
        }
        else {
          var pageId = pageList(Random.nextInt(pageList.size))
          println("Client" + clientId + ": Update page" + pageId)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Post(s"$apiLocation/page/$pageId/$clientId", pageContent))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
        }
        sender ! "Request processed"
      }
    //create page
      case createPage(clientId,newPage) => {
        var pagesCount=0
        println("Client" + clientId + ": Create a new page")
        val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
        val f: Future[String] = pipeline(Put(s"$apiLocation/page/$clientId/$clientId", newPage))
        val msg = Await.result(f, timeout)
        println(s"Server: $msg")
        var tokens=msg.split("=")
        var id=tokens(1).toInt-1
        if(id>0) {
          pageList+=id
          pagesCount=id
        }
        sender ! pagesCount
      }
      //get pic
      case getPic(clientId,picId) => {
        println("Client" + clientId + ": Get pic"+picId)
        val pipeline: HttpRequest => Future[List[Photo]] = sendReceive ~> unmarshal[List[Photo]]
        val f: Future[List[Photo]] = pipeline(Get(s"$apiLocation/photo/$picId/$clientId"))
        val picture = Await.result(f, timeout)
        if (picture.size == 0) {
          println("Failure:Does not have access to the photo")
        }
        else {
          for (i <- 0 to picture.size - 1) {
            var urlField:String=picture(i).url.toString
            var tokens = urlField.split(" ")
            var url=tokens(0)
            var saltString = tokens(1)
            var encryptedSecretKey=tokens(2)

            var decryptedSecretKey=Security.decryptKeyRSA(encryptedSecretKey,cipherRsa,privateKeyBytes)
            var decryptedurl = Security.decryptDataAES(url, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            //var decryptedalbum = Security.decryptDataAES(picture(i).albumId, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            var decryptedcaption = Security.decryptDataAES(picture(i).caption, saltString.getBytes(), decryptedSecretKey)(cipherAes)
            var decryptedphoto = new Photo(picture(i).ownerId, decryptedurl, decryptedcaption, picture(i).albumId)
            println(decryptedphoto)
          }
        }
        sender ! "Request processed"
      }
    //delete pic
      case deletePic(clientId)=>{
        if(photoList.size==0){
          println("Client" + clientId + ": Delete photo")
          println("No photos exists")
        }
        else {
          var picId = photoList(Random.nextInt(photoList.size))
          println("Client" + clientId + ": Delete pic" + picId)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Delete(s"$apiLocation/photo/$picId/$clientId"))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt
          if (id == 1) {
            photoList -= picId
          }
        }
        sender ! "Request processed"
      }
      //update pic
      case updatePic(clientId,picContent) => {
        if(photoList.size==0){
          println("Client" + clientId + ": update photo")
          println("No photos exists")
        }
        else {
          var picId = photoList(Random.nextInt(photoList.size))
          println("Client" + clientId + ": Update pic" + picId)
          var saltString=Security.generateSalt()
          var mySecretKey=generateSecretKey()
          var encryptedPic = Photo(clientId,Security.encryptDataAES(picContent.url.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes)+" "+saltString,Security.encryptDataAES((picContent.caption).getBytes(),saltString.getBytes(),mySecretKey)(cipherAes), picContent.albumId)//Security.encryptDataAES(picContent.albumId.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes))
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Post(s"$apiLocation/photo/$picId/$clientId", encryptedPic))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt

          if(id>0) {
            //request public keys of friends from server
            val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
            val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/pickey/$picId/$clientId"))
            val publicKeyList = Await.result(fGetKey, timeout)

            //encrypt secret key with public keys
            if (publicKeyList.size > 0) {
              var encSecretKeys = List[Tuple2[Int, String]]()
              for (i <- 0 to publicKeyList.size - 1) {
                var encryptedSecretKey = Security.encryptKeyRSA(mySecretKey.getBytes(), cipherRsa, publicKeyList(i)._2)
                encSecretKeys = new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
              }

              //send the encrypted secret key list to server
              val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
              val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/pickey/$picId/$clientId", encSecretKeys))
              val msg = Await.result(fSendKeys, timeout)
            }
          }
        }
        sender ! "Request processed"
      }
    //upload pic
      case uploadPic(clientId,newPic) => {
        var saltString=Security.generateSalt()
        var mySecretKey=generateSecretKey()
        var encryptedPic = Photo(clientId,Security.encryptDataAES(newPic.url.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes)+" "+saltString,Security.encryptDataAES((newPic.caption).getBytes(),saltString.getBytes(),mySecretKey)(cipherAes), newPic.albumId)//Security.encryptDataAES(newPic.albumId.getBytes(),saltString.getBytes(),mySecretKey)(cipherAes))
        var picsCount=0
        println("Client" + clientId + ": Upload new pic")
        val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
        val f: Future[String] = pipeline(Put(s"$apiLocation/photo/$clientId/$clientId", encryptedPic))
        val msg = Await.result(f, timeout)
        println(s"Server: $msg")
        var tokens=msg.split("=")
        var id=tokens(1).toInt-1
        if(id>0) {
          photoList += id
          picsCount = id

          //request public keys of friends from server
          val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
          val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/pickey/$id/$clientId"))
          val publicKeyList = Await.result(fGetKey, timeout)

          //encrypt secret key with public keys
          if (publicKeyList.size > 0) {
            var encSecretKeys = List[Tuple2[Int, String]]()
            for (i <- 0 to publicKeyList.size - 1) {
              var encryptedSecretKey = Security.encryptKeyRSA(mySecretKey.getBytes(), cipherRsa, publicKeyList(i)._2)
              encSecretKeys = new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
            }

            //send the encrypted secret key list to server
            val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
            val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/pickey/$id/$clientId", encSecretKeys))
            val msg = Await.result(fSendKeys, timeout)
          }
        }
        sender ! picsCount
      }
    //get album
      case getAlbum(clientId,albumId) => {
        println("Client" + clientId + ": Get album"+albumId)
        val pipeline: HttpRequest => Future[List[Album]] = sendReceive ~> unmarshal[List[Album]]
        //println("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"+token)
        val f: Future[List[Album]] = pipeline(Get(s"$apiLocation/album/$albumId/$clientId"))
        val albums = Await.result(f, timeout)
        if (albums.size == 0) {
          println("Failure:Does not have access to the album")
        }
        else {
          for (i <- 0 to albums.size - 1) {
            var descField:String=albums(i).description
            var tokens = descField.split(" ")
            var desc=tokens(0)
            var saltString = tokens(1)
            var encryptedSecretKey=tokens(2)

              var decryptedSecretKey=Security.decryptKeyRSA(encryptedSecretKey,cipherRsa,privateKeyBytes)
              var photolist = albums(i).photos
              var decryptedphotos = List[AlbumPhoto]()
              for(pic <- photolist){
                var photostring = new AlbumPhoto(Security.decryptDataAES(pic.url, saltString.getBytes(), decryptedSecretKey)(cipherAes))
                decryptedphotos = photostring :: decryptedphotos
              }
              var decrypteddescription = Security.decryptDataAES(desc, saltString.getBytes(), decryptedSecretKey)(cipherAes)
              var decryptedalbum = new Album(albums(i).ownerId, decryptedphotos, decrypteddescription)
              println(decryptedalbum)

          }
        }
        sender ! "Request processed"
      }
    //update album
      case updateAlbum(clientId,albumContent) => {
        if(albumList.size==0){
          println("Client" + clientId + ": update album")
          println("No albums exists")
        }
        else {
          var saltString=Security.generateSalt()
          var mySecretKey=generateSecretKey()
          var albumId = albumList(Random.nextInt(albumList.size))
          println("Client" + clientId + ": Update album" + albumId)
          var photolist = albumContent.photos
          var encryptedphotos = List[AlbumPhoto]()
          for(pic <- photolist){
            var photostring = new AlbumPhoto(Security.encryptDataAES(pic.url.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes))
            encryptedphotos = photostring :: encryptedphotos
          }
          var description = Security.encryptDataAES(albumContent.description.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes)+" "+saltString
          var encryptedAlbum = new Album(albumContent.ownerId, encryptedphotos, description)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Post(s"$apiLocation/album/$albumId/$clientId", encryptedAlbum))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt

          if(id > 0) {
            //request public keys of friends from server
            val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
            val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/albkey/$albumId/$clientId"))
            val publicKeyList = Await.result(fGetKey, timeout)

            //encrypt secret key with public keys
            if (publicKeyList.size > 0) {
              var encSecretKeys = List[Tuple2[Int, String]]()
              for (i <- 0 to publicKeyList.size - 1) {
                var encryptedSecretKey = Security.encryptKeyRSA(mySecretKey.getBytes(), cipherRsa, publicKeyList(i)._2)
                encSecretKeys = new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
              }

              //send the encrypted secret key list to server
              val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
              val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/albkey/$albumId/$clientId", encSecretKeys))
              val msg = Await.result(fSendKeys, timeout)
            }
          }
        }
        sender ! "Request processed"
      }
    //upload album
      case uploadAlbum(clientId,albumContent) => {
          var albumsCount=0
        var saltString=Security.generateSalt()
        var mySecretKey=generateSecretKey()
          println("Client" + clientId + ": Upload album")
        var photolist = albumContent.photos
        var encryptedphotos = List[AlbumPhoto]()
        for(pic <- photolist){
          var photostring = new AlbumPhoto(Security.encryptDataAES(pic.url.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes))
          encryptedphotos = photostring :: encryptedphotos
        }
        var encrypteddescription = Security.encryptDataAES(albumContent.description.getBytes(), saltString.getBytes(), mySecretKey)(cipherAes)+" "+saltString
        var encryptedAlbum = new Album(albumContent.ownerId, encryptedphotos, encrypteddescription)
        val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Put(s"$apiLocation/album/$clientId/$clientId", encryptedAlbum))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt - 1
          if(id>0) {
          albumList+=id
          albumsCount=id

            //request public keys of friends from server
            val pipelineGetKeys: HttpRequest => Future[List[Tuple2[Int, String]]] = sendReceive ~> unmarshal[List[Tuple2[Int, String]]]
            val fGetKey: Future[List[Tuple2[Int, String]]] = pipelineGetKeys(Get(s"$apiLocation/albkey/$id/$clientId"))
            val publicKeyList = Await.result(fGetKey, timeout)

            //encrypt secret key with public keys
            if (publicKeyList.size > 0) {
              var encSecretKeys = List[Tuple2[Int, String]]()
              for (i <- 0 to publicKeyList.size - 1) {
                var encryptedSecretKey = Security.encryptKeyRSA(mySecretKey.getBytes(), cipherRsa, publicKeyList(i)._2)
                encSecretKeys = new Tuple2(publicKeyList(i)._1, encryptedSecretKey) :: encSecretKeys
              }

              //send the encrypted secret key list to server
              val pipelineSendEncKeys: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
              val fSendKeys: Future[String] = pipelineSendEncKeys(Post(s"$apiLocation/albkey/$id/$clientId", encSecretKeys))
              val msg = Await.result(fSendKeys, timeout)
            }
          }

        sender ! albumsCount
      }
      //delete album
      case deleteAlbum(clientId)=>{
        if(albumList.size==0){
          println("Client" + clientId + ": Delete album")
          println("No albums exists")
        }
        else {
          var albumId = albumList(Random.nextInt(albumList.size))
          println("Client" + clientId + ": Delete album" + albumId)
          val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
          val f: Future[String] = pipeline(Delete(s"$apiLocation/album/$albumId/$clientId"))
          val msg = Await.result(f, timeout)
          println(s"Server: $msg")
          var tokens = msg.split("=")
          var id = tokens(1).toInt
          if (id == 1) {
            albumList -= albumId
          }
        }
        sender ! "Request processed"
      }
  }
}


case class getFriendList(clientId: Int, requestedId: Int)

case class addFriend(clientId: Int, friendId: Int,clientList: List[ActorRef])

case class initFriendList(clientId: Int)

case class unFriend(clientId: Int, requestedId: Int)

case class createUser(clientId: Int,newUser: User)

case class deleteAccount(clientId: Int)

case class updateUserProfile(clientId: Int,updatedProfile:User)

case class getUserProfile(clientId: Int, requestedId: Int)

case class getPost(clientId:Int, postId:Int)

case class updatePost(clientId:Int, postContent:FbPost)

case class createPost(clientId:Int,newPost:FbPost)

case class deletePost(clientId:Int)

case class getPage(clientId:Int, pageId:Int)

case class deletePage(clientId:Int)

case class updatePage(clientId:Int,pageContent:Page)

case class createPage(clientId:Int,newPage:Page)

case class getPic(clientId:Int,picId:Int)

case class deletePic(clientId:Int)

case class updatePic(clientId:Int,picContent:Photo)

case class uploadPic(clientId:Int,newPic:Photo)

case class getAlbum(clientId:Int,albumId:Int)

case class updateAlbum(clientId:Int,albumContent:Album)

case class uploadAlbum(clientId:Int,albumContent:Album)

case class deleteAlbum(clientId:Int)

case class getPublicKey()

case class logIn(clientId:Int)
