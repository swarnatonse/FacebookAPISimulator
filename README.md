# FacebookAPISimulator
Facebook API Simulator with Security 

A REST API simulating Facebook API has been implemented. GET, PUT, POST and DELETE operations for the following functionalities have been implemented and tested:
1. User		- Handles user profiles. Operations include creating a user, deleting a user, displaying a user and updating user profile. Creating, deleting and updating can only be done by the owner. Friends of user can access the full profile and non-friends can access only some fields.
2. Friend List	- Handles friend list. Operations include creating a friend list, adding friends, deleting friends and displaying friends. Creating, adding and deleting friends is done based on the client IDs. Only the owning client and friends of a client can view the client's friend list.
3. Post		- Handles posts. Operations include creating a post, deleting a post, displaying a post and updating a post. Creating, deleting and updating can only be done by the owner. Posts are displayed to only the owning client and friends of the owning client.
4. Page		- Handles public pages. Operations include creating a page, deleting a page, displaying a page and updating a page. Creating, deleting and updating can only be done by the owner. Pages are public and can be viewed to any client who requests it.
5. Photo	- Handles photos. Operations include uploading a photo, deleting a photo, displaying a photo and updating details of the photo. Uploading, deleting and updating can only be done by the owner. Only the owning client and friends of the owning client can view the photo. In the Facebook API photos are stored either directly or as URLs. In our project we have used URLs for pictures. 
6. Album	- Handles albums. Operations include creating an album, deleting an album, viewing an album and adding photos to the album. Creating, deleting and adding photos can be done only by the owning client. Viewing of the album can only be done by the owning client or friends of the owning client. In the albums also we have used photo URLs.

The following stats were used for our project:
Around 65% percent of FB users were found to be Daily Active Users. Therefore in our project 65% of the total users were made active users and the remaining users were made passive (not-so-active) users. Active users use the API more frequently while the passive users use it less frequently.
Average number of friends was found out to be 245. We scaled it to the number of users and decided on a value of the average number of friends being 0.25% of total users.
The ratio of male:female users was kept at 55:45.
An age restriction of 13 was used for each user.
Sources:
http://expandedramblings.com/index.php/by-the-numbers-17-amazing-facebook-stats/
http://sproutsocial.com/insights/new-social-media-demographics/
http://www.jeffbullas.com/2011/04/28/50-fascinating-facebook-facts-and-figures/

Implementation Notes:
The server runs with 8 actors, 2 actors exclusively for initializing the network and 6 actors exclusively handling every functionality of User, FriendList, Post, Page, Photo and Album respectively.
The client runs as many actors as there are users on the network. Number of users is given as input. In the beginning initialization is done by creating the users and adding friends according to the stats given above.
The highest number of users managed on our network was 700,000
