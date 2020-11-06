=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=
CIS 120 Game Project README
PennKey: ailic
=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=

===================
=: Core Concepts :=
===================

- List the four core concepts, the features they implement, and why each feature
  is an approprate use of the concept. You may copy and paste from your proposal
  document if you did not change the features you are implementing.

  1. OO design: (implemented for obstacles, also for the fractal tree)

  2. custom IO: (scores.meow, IO class)

  3. collections: (primarilly queues in ObstacleGenerator.java; also in IO for scores (maps)

  4. recursion/complex data structure (not sure which is more applicable): FractalTree.java


=========================
=: Your Implementation :=
=========================

- Provide an overview of each of the classes in your code, and what their
  function is in the overall game.
  
  *some of the specific methods are documented in the classes themselves*

  Cat               - organizes cat-related methods and states; extends GameObj
  Direction         - from the mushroom game, needed for GameObj
  FallingObstacle   - a subtype of obstacle, is basically a square with const. downward velocity
  FractalTree       - a class that generates a Pythagorean tree with a given depth and initial size; it's drawable and movable
  Game              - basically the same Game.java from the mushroom game, added labels and removed unnecessary ugly components
  GameModel         - equivalent to GameCourt; central to the game; contains the games logic and drawing methods
  GameObj           - from the mushroom game
  IO                - implements a scoreboard load and save methods, sorts them; uses a map; combines strings of the users with 
                      the same score
  MouseObstacle     - an obstacle that generates an mouse-shaped enemy, which distracts the cat and makes it accidentally burn itself
                      blinded by greed
  Obstacle          - abstract class for obstacles, extends GameObj, provides an abstract method sideEffect() which is dynamically 
                      dispatched depending on the subtype of the obstacle
  ObstacleGenerator - a wrapper around several queues central to game's dynamic; generates obstacles in a pseudo-random manner, keeps
                      them from overloading the memory by deleting them; has a lot of hardcoded constants 
  Poison            - actually is fire. equally as bad for the cat    
  SimpleObstalce    - just a rectangle-shaped obstacle, providing a simple obstacle for the cat             
  WaterObstacle     - an obstacle which can actually help the cat, by giving her an adrenaline rush; it makes her jump involuntarily 
                      (much higher than normal) and speeds up the obstacles for a brief period easening the traversal
   
  
  
- Revisit your proposal document. What components of your plan did you end up
  keeping? What did you have to change? Why?

I ended up diverging a lot from the original plan; I kept the side-scrolling, the obstacle generation and types, because I thought
they ended up functioning quite nicely, and were (at least to me) fun to jump through. I removed the bird because it would have been
a huge mess, and it didn't fulfill any of the concepts (I was mistaken). I stayed away from the JUnit tests, because I heard that there
were some problems about running them, and also, because my classes got more and more intertwined :c (passing refferences around). Still
I think the game wouldn't benefit from a more modular approach in any aspect.

- Were there any significant stumbling blocks while you were implementing your
  game (related to your design, or otherwise)?

Yes, I've found the concepts biased towards grid based/simulation based games, and I had a hard time figuring out what should my 4th 
concept be. Finally I chose fractals, as they look cool, break the monotony of the game, and are sufficiently complex to implement.

The majority of my dev-ing went smoothly, but I had struggled a bit about fractals (I had to revisit my complex numbers&geometry).
Moreover I wasn't quite sure about the style of the scoreboard, so I went with one that shows ALL scores(in a compacted manner).


- Evaluate your design. Is there a good separation of functionality? How well is
  private state encapsulated? What would you refactor, if given the chance?
  
  I think that there is a good degree of separation, but there is some data-smuggling happening around. As I progressed I was more focused
  on the new things to add than refactoring the current code (#no_regrets). Same goes for private state; I tried using getters/setters as 
  much as I could, but in the end, time was of the essence, so some states were left public.
  
  I would refactor the way the cat reacts to the obstacles because currently there is a cascade of methods that jump from one class to another
  (gammodel->obstalce->cat) and the way the reaction is handled is confusing. 
  
  Other than that, there are some minor bugs, regarding the pausing the timers while drawing splash screens (instructions and over). 
  There may also be some bugs present due to timers interval setting but i chose do leave it on the default value. 
  
  Finally, I found
  the Swing's default dialog/messagebox/whatever behavior really frustrating, but I didn't want to waste time on extending those; so
  multiple onsets of scores message box can occur, a user can leave a blank username if he presses OK when prompted; game may not pause
  after dying because for some reason the message box additionally to being shown, gets painted to the JPanel (!?). I am sure that a more
  modular design could have helped in these instances, though!



========================
=: External Resources :=
========================

- Cite any external resources (libraries, images, tutorials, etc.) that you may
  
  No plug-ins were used;
  
  ALL graphics used in the game I made personally (and took the most pleasure in!). I used a website called piskel, to generate animated GIFs for
  the objects;
  
  I may have used a number of syntax/library-related google queries as my language of preference is not Java ( I am more of a C++ person), but it was a nice
  experience because it reminds me of my C#/VBasic days. I used the description of the Pythagorean tree on Wikipedia, and came up
  with an algorithm for my purposes; I did all of the game's logic/math/etc. alone.


