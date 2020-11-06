import java.util.LinkedList;
import java.util.Queue;
import java.util.Random;

public class ObstacleGenerator {

	// generates all of the obstacles
	private Queue<Obstacle> obstacles;
	private Queue<Obstacle> mice;
	private Queue<Obstacle> falling;
	public Queue<Obstacle> warning;
	public Queue<FractalTree> trees;
	
	private int courtWidth, courtHeight;
	
	//these two are used to keep the smoothness of terrain when in adrenalin mode
	private int miceSpeed;
	private int minSpeed;
	
	
	private int cnt;

	public ObstacleGenerator(int size, int courtWidth, int courtHeight) {
		// regular
		obstacles = new LinkedList<Obstacle>();

		// irregular
		mice = new LinkedList<Obstacle>();
		falling = new LinkedList<Obstacle>();
        warning = new LinkedList<Obstacle>();
        
        //trees
        trees = new LinkedList<FractalTree>();
           
		miceSpeed = -7;
		minSpeed = -5;

		this.courtHeight = courtHeight;
		this.courtWidth = courtWidth;
		cnt = 1;

		// first two obstacles (1 column) should be empty to provide the player
		// some time
		obstacles.add(flipCoin(0, 0, -5));
		obstacles.add(new SimpleObstacle(0, courtWidth, courtHeight, 0, 0, -5));
		
		
		
		// randomly generate further terrain
		for (int i = 1; i < 4; i++) {
			
			
			if (rnd(0, 10) > 1)
				obstacles.add(flipCoin(i * 300, rnd(30, 75), -5));
			else
				obstacles.add(flipCoin(i * 300, 0, -5));

			if (rnd(0, 10) > 1)
				obstacles.add(new SimpleObstacle((i * 300), courtWidth, courtHeight, 0, rnd(30, 75), -5));
			else
				obstacles.add(new SimpleObstacle(i * 300, courtWidth, courtHeight, 0, 0, -5));

			if (rnd(0, 10) > 8)
				mice.add(new MouseObstacle(i * 300, courtWidth, courtHeight, -7));

			if (rnd(0, 10) > 6)
				falling.add(new FallingObstacle(rnd(courtWidth/3,courtWidth), courtWidth, courtHeight));
		}

	}

	// equal chance of water or simple obstacle
	Obstacle flipCoin(int x, int size, int minSpeed) {
		if (rnd(1, 2) > 1 || size == 0) {
			return new SimpleObstacle(x, courtWidth, courtHeight, 1, size, minSpeed);
		} else {
			return new WaterObstacle(x, courtWidth, courtHeight, minSpeed);
		}
	}

	// just a quick random generator
	public int rnd(int min, int max) {
		Random r = new Random();
		return r.nextInt((max - min) + 1) + min;
	}

	// removes the obstacle which crossed the left border and replaces it by a
	// new one; also, adds mice :)
	public void add() {

		// add new column of obstacles
		if(--cnt <= 0){
		trees.add(new FractalTree(rnd(40,100),rnd(7,15), courtWidth));
		cnt = 12;
		}
		
		if (rnd(0, 10) > 2)
			obstacles.add(flipCoin(courtWidth, rnd(30, 75), minSpeed));
		else
			obstacles.add(flipCoin(courtWidth, 0, minSpeed));

		if (rnd(0, 10) > 2)
			obstacles.add(new SimpleObstacle(courtWidth, courtWidth, courtHeight, 0, rnd(30, 75), minSpeed));
		else
			obstacles.add(new SimpleObstacle(courtWidth, courtWidth, courtHeight, 0, 0, minSpeed));

		// remove column
		obstacles.poll();
		obstacles.poll();

		// add mice
		if (mice.size() < 3) {
			if (rnd(0, 10) > 8) {
				mice.add(new MouseObstacle(courtWidth, courtWidth, courtHeight, miceSpeed));
			}
		}
        //add falling
		if (falling.size() < 3) {
			if (rnd(0, 10) > 1) {
				falling.add(new FallingObstacle(rnd(courtWidth/3,courtWidth), courtWidth, courtHeight));
			}
		}
	}

	// remove a stray mouse
	public void removeMice() {
		mice.poll();
	}

	public Queue<Obstacle> get() {
		return obstacles;
	}

	public Queue<Obstacle> getMice() {
		return mice;
	}

	public Queue<Obstacle> getFalling() {
		return falling;
	}

	public void setMiceSpeed(int minSpeed) {
		miceSpeed = minSpeed;
	}

	public void removeFalling() {
		falling.poll();
	}

	public void setMinSpeed(int minSpeed2) {
		minSpeed = minSpeed2;
		
	}
}
