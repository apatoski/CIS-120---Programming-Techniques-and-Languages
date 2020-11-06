import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;

public class FallingObstacle extends Obstacle {

	public static final int INIT_X = 0;
	public static final int INIT_Y = 30;
	public static final int INIT_VEL_X = 0;
	public static final int INIT_VEL_Y = 3;
	public static final int INIT_WIDTH = 30;

	Image ii;

	public FallingObstacle(int pos_x, int court_width, int court_height) {
		super(INIT_VEL_X, INIT_VEL_Y, pos_x, 0, INIT_WIDTH, INIT_WIDTH, court_width, court_height);
		ii = new ImageIcon("falling.gif").getImage();
	}

	@Override
	int getType() {
		return 4;
	}

	@Override
	public void draw(Graphics g) {

		g.setColor(Color.BLACK);
		g.fillRect(pos_x, pos_y, width, width);
	}


	@Override
	void sideEffect(GameModel gm, Cat c) {
		c.die();
		gm.playing = false;
	}

}
