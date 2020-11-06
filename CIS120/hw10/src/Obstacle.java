
public abstract class Obstacle extends GameObj {

	public Obstacle(int v_x, int v_y, int pos_x, int pos_y, int width, int height, int court_width, int court_height) {
		super(v_x, v_y, pos_x, pos_y, width, height, court_width, court_height);
	
	}
	
	abstract int getType();
	
	abstract void sideEffect(GameModel gm, Cat c);

}
