import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

public class Cat extends GameObj {
	// consts
	public static final int SIZE = 128;
	public static final int INIT_X = 40;
	public static final int INIT_Y = 400 - 30 - 128;
	public static final int INIT_VEL_X = 0;
	public static final int INIT_VEL_Y = 0;

	private GameObj bound;
	private GameModel gm;
	private int lastJump;
	private int lastWater;
	private int lastMouse;

	Image ii;
	ImageIcon i;
	private int gravity = 1;
	public boolean jumped, watered, moused;
	public boolean dead = false;

	public Cat(int courtWidth, int courtHeight, GameModel gm) {
		super(INIT_VEL_X, INIT_VEL_Y, INIT_X, INIT_Y, SIZE, SIZE, courtWidth * 2 / 5, courtHeight - 30);

		i = new ImageIcon("cropped.gif");
		ii = i.getImage();

		jumped = false;
		watered = false;
		moused = false;
		dead = false;
		lastJump = 0;
		lastWater = 0;
		lastMouse = 0;

		bound = new GameObj(0, 0, pos_x + 35, pos_y + 25, width - 30, height - 20, courtWidth, courtHeight);

		this.gm = gm;

	}

	@Override
	public void move() {

		pos_x += v_x;
		pos_y -= v_y;

		if (pos_y < INIT_Y) {
			v_y -= gravity;
		}

		clip();
		if (!moused) {

			// update bound
			bound = new GameObj(0, 0, pos_x + 38, pos_y + 51, width - 38 - 20, height - 30 - 51, 1200, 400);

			// refresh states
			if ((int) System.currentTimeMillis() - lastJump > 1000) {
				jumped = false;
			}

			if (watered && (int) System.currentTimeMillis() - lastWater > 2000
					|| System.currentTimeMillis() - lastWater < 0) {
				watered = false;
				gm.setBackground(Color.LIGHT_GRAY);
				v_x = 0;
				for (Obstacle x : gm.gen.get()) {
					x.v_x = -5;
				}
				for (Obstacle x : gm.gen.getMice()) {
					x.v_x = -7;
				}
			}
		} else {
			v_x = -6;
			v_y = 0;
			pos_y = INIT_Y;
		}

	}

	// pre-set jump command
	public void jump(int strenght) {
		if (moused) {
			return;
		}
		if (!jumped) {
			this.v_y = strenght;
			lastJump = (int) System.currentTimeMillis();
			jumped = true;
		}
	}

	// reaction to a rat
	public void getDistracted() {
		if (moused) {
			return;
		}
		i = new ImageIcon("cat_flipped.gif");
		ii = i.getImage();
		moused = true;

	}

	// reaction to a water pipe
	public void getCrazy() {
		if (moused) {
			return;
		}
		jumped = false;
		jump(16);
		lastJump += 300;

		for (Obstacle x : gm.gen.get()) {
			x.v_x = -7;
		}
		for (Obstacle x : gm.gen.getMice()) {
			x.v_x = -9;
		}

		watered = true;
		lastWater = (int) System.currentTimeMillis();

	}

	// reaction to an obstacle
	public void die() {

		dead = true;

		IO x = new IO();
		x.saveEntry(gm.score, gm.username);
		

		Map<Integer, String> scores = new TreeMap<Integer, String>();
		scores = (new IO()).loadEntries();
		String message = "*SCORES*\n";
		for (Map.Entry<Integer, String> ee : scores.entrySet()) {
			message += String.valueOf(ee.getKey()) + ": " + ee.getValue() + "\n";
		}
		JOptionPane.showMessageDialog(null, message);
		gm.playing = false;
	}

	@Override
	public void draw(Graphics g) {

		g.drawImage(ii, pos_x, pos_y, 128, 128, gm);
	}

	// bound instead of sprite, used to smooth out the experience
	public boolean intersects2(GameObj obj) {
		return bound.intersects(obj);
	}

	/// setters and getters
	public void setVX(int squareVelocity) {
		if (moused) {
			return;
		}

		v_x = squareVelocity;

	}

	public GameObj getBound() {
		return bound;
	}

	public void setBound(GameObj bound) {
		this.bound = bound;
	}

}
