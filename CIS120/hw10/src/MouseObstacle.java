import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;

public class MouseObstacle extends Obstacle {

	public MouseObstacle(int x, int courtWidth, int courtHeight, int miceSpeed) {
		super(miceSpeed, 0, x, 400 - 30 - 44, 44, 44, courtWidth, courtHeight);

	}


	@Override
	int getType() {

		return 2;
	}


	@Override
	void sideEffect(GameModel gm, Cat c) {
		c.getDistracted();
		this.v_x = -10;
	}

	@Override

	public void draw(Graphics g) {

		Image ii = new ImageIcon("mouse.gif").getImage();
		g.drawImage(ii, pos_x, pos_y, 44, 44, null);
	}

}
