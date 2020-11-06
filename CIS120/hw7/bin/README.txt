=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=
CIS 120 Homework 7 README
PennKey: ailic
=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=

============
=: Task 2 :=
============

- Do you plan on creating any new classes in your design? If so, what classes
  are you making, what do they store, and what behaviors do they have?
  
  I wanted to make accessing everything by its ID a default behavior, so I made 
  two classes: User and Channel. User stores nick and a channel list, whereas 
  Channel stores channel name and owner's ID. 

- How do you plan on storing what users are registered on the server?

  I plan on having a Map from user IDs to users, and map from Channel IDs to
  channels. This makes long access syntax, but ensures effective data usage and
  ensures my wanted "default by id" approach.

- How do you plan on keeping track of which user has which user ID, considering
  the fact that the user's nickname can change over the course of the program?

  This is ensured with the User class.
  
- How do you plan on storing what users are in a channel?
  
  I don't; I will store a list of channels for every user. It would be more 
  effective if I made an additional map, but I wanted to make everything in one 
  place.

- How do you plan on keeping track of which user is the owner of each channel?

  Channel class has ownerID field.

- Justify your choice of collections (TreeSet, TreeMap, or LinkedList) for the
  collections you use in your design.
  
  I needed the maps for the association between IDs and their objects; I used 
  LinkedList for channels as it has a clean and simple interface and doesn't 
  need comparable functionality.


============
=: Task 3 :=
============

- Did you make any changes to your design while doing this task? Why?
 I didn't make any major design changes, maybe added a few function here and there.

============
=: Task 4 :=
============

- Did you make any changes to your design while doing this task? Why?

Changed the key of channels to their name because i figured out ID would 
not be used. Adjusted User and ServerModel classes accordingly.

============
=: Task 5 :=
============

- How do you plan on keeping track of which channels are invite-only?

I made a flag in the channel class.

- Will you make any changes to your work from before in order to make
  implementing invite-only channels easier?

I added object returning functions, to make everything cleaner and
in order not to re-use too much code.

============
=: Task 6 :=
============

- Did you have to make any changes to your design in Task 6? Why?
  
  I switched channel list to channel set in User class, because I
  think it will increase efficacy, and because channels can have no duplicates.

- If you were to redo this assignment, what changes (if any) would you make in
  how you designed your code?
  
  I would be more elegant with handling String return values in Command class.
