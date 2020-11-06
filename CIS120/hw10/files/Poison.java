
/**
 * CIS 120 Game HW
 * (c) University of Pennsylvania
 * @version 2.0, Mar 2013
 */

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

public class Poison extends GameObj {

	public static final int INIT_X = -15;
	public static final int INIT_Y = 0;
	public static final int INIT_VEL_X = 0;
	public static final int INIT_VEL_Y = 0;
	Image ii;

	public Poison(int courtWidth, int courtHeight) {
		super(INIT_VEL_X, INIT_VEL_Y, INIT_X, INIT_Y, 53, 400, courtWidth, courtHeight);
		ii = new ImageIcon("f.gif").getImage();
	}

	@Override
	public void draw(Graphics g) {
		g.drawImage(ii, pos_x, pos_y, width, height, null);
	}

}
