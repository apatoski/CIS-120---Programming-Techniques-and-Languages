import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;

public class WaterObstacle extends Obstacle {

	Image ii = new ImageIcon("water.gif").getImage();
	public WaterObstacle(int x, int courtWidth, int courtHeight, int minSpeed) {
		super(minSpeed, 0, x, 400 - 30 - 40, 40, 40, courtWidth, courtHeight);
		ii = new ImageIcon("water.gif").getImage();;
	}

	@Override
	int getType() {
		return 1;
	}

	@Override
	void sideEffect(GameModel gm, Cat c) {
		c.getCrazy();
		gm.setBackground(Color.RED);
	}

	@Override
	public void draw(Graphics g) {
		g.drawImage(ii, pos_x, pos_y, 40, 40, null);
	}
}
