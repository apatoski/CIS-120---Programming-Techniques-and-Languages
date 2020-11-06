import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;

public class SimpleObstacle extends Obstacle {

	public static final int INIT_X = 0;
	public static final int INIT_Y = 30;
	public static final int INIT_VEL_X = -5;
	public static final int INIT_VEL_Y = 0;
	public static final int INIT_WIDTH = 30;

	public SimpleObstacle(int x, int courtWidth, int courtHeight, int up, int hght, int minSpeed) {
		super(minSpeed, INIT_VEL_Y, x, INIT_Y, INIT_WIDTH, hght, courtWidth, courtHeight);

		if (up == 1)
			pos_y = courtHeight - hght - 30;

	}

	@Override
	public void draw(Graphics g) {

		g.setColor(Color.BLACK);
		g.fillRect(pos_x, pos_y, width, height);
	}

	@Override
	int getType() {
		return 0;
	}

	@Override
	void sideEffect(GameModel gm, Cat c) {
		if (this.height != 0) {
			
			c.die();
		}

	}
}
