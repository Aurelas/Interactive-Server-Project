
# Project Title: Interactive-Server-Project
# Team Name: Ken's Bold Team
###Update: Milestone 2
To run our tool, open and run the code in InteractiveTestJB.rkt.
It will automatically open the application in a new window.

New features of this version:
  4 Pages of Information Pulled from the site
  Click on player names to view their individual stats.
  Click on games to view more detailed stats *WIP*
  "Trending Players" - Display on first page. This is a complex algorithm that tracks how well AND consistant players have been recently. The 3 most consistant players recently are shown on the first page.



### Statement
In this project, we will create an interactive display of hockey-stats for JBrown's beer-league team. 
Basics include, displaying indivudual stats, statistics from games, aswell as displaying who the point leaders of the team are.
To expand on this basic simple idea, we are also going to implement several algorithms to detect who the "trending" players are, and display facts about them.
In ex. if a player has been 'hot' recently, the algorithm should identify them, and display their name in a "Players to Watch" list. Beneath their name, our program should use a tree to decide what 2 "facts" are most relevant to the player.

So if they've scored 5 goals in the last 10 games, but three were in the last game, it will choose "Hat-trick Scorer" over "5 in Last 10"

We plan to make this application fully interactive, meaning that users will be able to sort data with simple mouse clicks.
Additionally, they should be able to update game stats easily within the program, which will then in turn update player stats.

Our main goal is learning how to build an effective GUI in Racket, but also to work on parsing and morphing data.

### Analysis
There are two data sets we are going to use, detailed below.
For one of the data sets, poorly written raw html code will lead to extensive use of recursion. Slowly parsing through the file to grab and use the data held in the document will require heavy use of recursion.

Additionally, heavy use of list functions will be required for dealing with lists holding players, lists holding goalies, and lists holding games.

Object-orientation will be used in conjuction with 'images' to hopefully create 'clickable' objects.

Currently we have a working GUI with a couple of buttons that we can click to change the image of our GUI and have a pretty clear cut idea on how to make the program fully interactive. We will use (m.jdbjohnbrown.net) as a template and temper our racket program to mimic the functionality of the template. 


### Data set or other source materials
There will be two main data sets we will work with in this project. The first is from a SQL database on my own personal site (m.jdbjohnbrown.net) and the second is from hockeytownsaugus.com, html code riddled with errors.

We will need to write new PHP code for handling the SQL interaction, as this is going to be stored in a public repository, and using the SQL database libraries would require making hosting the passwords publically.

The data will be sorted into neatly formed lists, from where we will alter the data to our liking using various parsing methods, to then be displayed graphically.  


### Deliverable and Demonstration
At the end of this project we should have a program that not only pulls then displays data from a pair of websites, but also allows users to change the data in the SQL database. The program will also run various tests on the data sets and compliment players who are doing well, and bring attention to them for recent good performances.

Again, this is all the background work behind the real goal, of making an interactive GUI which will use basic shapes to display and manipulate the data without using the command line.

### Evaluation of Results
We will know when we are successful when someone brand-new to our program can easily utilize our tool to view and update statistics.
This should be easily testable in a presentation, as well in testing phases, as asking for a voulenteer will be brief.

## Architecture Diagram

![alt text](https://raw.githubusercontent.com/oplS16projects/Interactive-Server-Project/master/OPL%20Architecture%20interactive%20server.png)

Create several paragraphs of narrative to explain the pieces and how they interoperate

**Net/url and racket:**

Using the Net/url library we will be able to write code which will grab data from our SQL server and transform it into a string, this string will then be read into a racket object for storing.

**2htdp/Universe and world:**

Once we have the racket object with our database data inside, we can use the net/url functions to access specific pieces of data. This data will mostly be comprised of numbers which in turn can be interpreted interpretted as a 'world state' by the 2htdp/Universe library. For example our object may contain a field called 'Number of goals' which will have a number associated with it. We can take this number, transform it into a 'world state' then we can transform the world using mouse clicks and keyboard keys to change the number. After changing the number the 'world' will report back a new 'world state', we will take this new 'world state' and load it into the object ultimately changing the information of our database.

**2hdtp/image:**

Hopefully by utilizing 2hdtp/image function we will be able to make the 'worlds' look more appealing to eye and possibly add some cool effects.


## Schedule

### First Milestone (Fri Apr 15)
Basic mouse-click interactive program. Clickable buttons which execute functions.

### Second Milestone (Fri Apr 22)
Algorithm results display on screen. -> Fun facts about "trending" players will appear.
Numerous 'pages' within the application will display different sets of data.

Finish creating the rest of the images and implementing them. Will also add arrow key functionality to specific images to mimic an image slideshow.

### Final Presentation (last week of semester)
Sending data back to the server for updates.
Possibly addition smooth graphical transitions between pages.

## Group Responsibilities

### Michael Antrobus @Aurelas
Will work on implementing the rest of the UI and screens using (m.jdbjohnbrown.net) as a template, will also add up arrow
and down arrow functionality to limited screens.

### John Brown @JDBJohnBrown
John will work heavily on the retrieval and manipulation of data, as well as writing any PHP code neccesary for interacting with the SQL server.   Will assist with interactivity in the GUI.

