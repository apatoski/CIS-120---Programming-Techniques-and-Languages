
/**
 * CIS 120 Game HW
 * (c) University of Pennsylvania
 * @version 2.0, Mar 2013
 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;

/**
 * GameCourt
 * 
 * This class holds the primary game logic for how different objects interact
 * with one another. Take time to understand how the timer interacts with the
 * different methods and how it repaints the GUI on every tick().
 * 
 */
@SuppressWarnings("serial")
public class GameModel extends JPanel {

	// the state of the game logic

	private Cat cat; // the Black cat, keyboard control
	// the Golden Snitch, bounces
	private Poison poison; // the Poison Mushroom, doesn't move
	public ObstacleGenerator gen;
	public boolean playing = false; // whether the game is running
	private JLabel status; // Current status text (i.e. Running...)
	public int score;
	public String username;
	private JLabel scoreLabel;
	// Game constants
	public static final int COURT_WIDTH = 1200;
	public static final int COURT_HEIGHT = 400;
	public static final int cat_VELOCITY = 5;
	// Update interval for timer, in milliseconds
	public static final int INTERVAL = 35;
	private boolean showInstructions = false;
	public Timer scoreT,timer;

	private boolean help1 = false, help2 = false; // for trees color

	public GameModel(JLabel status, JLabel username, JLabel scr) {
		// creates border around the court area, JComponent method
		setBorder(BorderFactory.createMatteBorder(30, 0, 30, 0, Color.BLACK));
		this.setBackground(Color.LIGHT_GRAY);
		gen = new ObstacleGenerator(4, COURT_WIDTH, COURT_HEIGHT);
		scoreLabel = scr;
		score = 0;
		this.username = username.getText().substring(10);

		 timer = new Timer(INTERVAL, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tick();
			}
		});

		scoreT = new Timer(1000, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateScore();
			}

			private void updateScore() {
				if (playing) {
					scoreLabel.setText("Score: " + ++score);
				}
			}
		});

		timer.start();
		scoreT.start();
		setFocusable(true);

		addKeyListener(new KeyAdapter() {

			public void keyPressed(KeyEvent e) {

				if (e.getKeyCode() == KeyEvent.VK_LEFT)
					cat.setVX(-cat_VELOCITY);
				else if (e.getKeyCode() == KeyEvent.VK_RIGHT)
					cat.setVX(cat_VELOCITY);
				else if (e.getKeyCode() == KeyEvent.VK_O) {
					showInstructions = true;
				}

			}

			public void keyReleased(KeyEvent e) {

				cat.setVX(0);
				if (e.getKeyCode() == KeyEvent.VK_UP) {

					cat.jump(12);

				}
				if (e.getKeyCode() == KeyEvent.VK_R) {
					reset();
				} else if (e.getKeyCode() == KeyEvent.VK_O) {
					showInstructions = false;
					playing = true;
				}

			}
		});

		this.status = status;
	}

	/**
	 * (Re-)set the game to its initial state.
	 */
	public void reset() {

		cat = new Cat(COURT_WIDTH, COURT_HEIGHT, this);
		poison = new Poison(COURT_WIDTH, COURT_HEIGHT);
		// snitch = new Circle(COURT_WIDTH, COURT_HEIGHT);
		gen = new ObstacleGenerator(4, 1200, 400);
		playing = true;
		this.setBackground(Color.LIGHT_GRAY);
		score = 0;
		scoreT.start();
		status.setText("Help the kitty!");
		showInstructions = false;

		// Make sure that this component has the keyboard focus
		requestFocusInWindow();
	}

	/**
	 * This method is called every time the timer defined in the constructor
	 * triggers.
	 */
	void tick() {
		if (playing) {
			cat.move();
			handleObstacleGeneration();
			handleCollisions();
			repaint();
		}
	}

	private void handleObstacleGeneration() {
		// add and remove obstacles
		int min = 1210;
		int minSpeed = -5;
		if (!gen.warning.isEmpty()) {
			for (GameObj x : gen.warning) {
				x.move();
				;
			}
			if (gen.warning.peek().pos_x <= 1) {
				gen.warning.poll();
			}
		}
		for (GameObj x : gen.get()) {
			x.move();
			if (x.pos_x < min) {
				min = x.pos_x;
				minSpeed = x.v_x;
			}
		}
		if (min <= 0) {
			gen.add();
		}
		if (cat.watered) {
			gen.setMinSpeed(-7);
		} else {
			gen.setMinSpeed(-5);
		}

		// add and remove mice
		min = 1210;
		minSpeed = -7;
		for (Obstacle x : gen.getMice()) {
			x.move();
			if (x.pos_x < min) {
				min = x.pos_x;
				minSpeed = x.v_x;
			}
		}
		if (min <= 0) {
			gen.removeMice();
		}
		if (cat.watered) {
			gen.setMiceSpeed(-9);
		} else {
			gen.setMiceSpeed(-7);
		}

		// add and remove falling obstacles
		int max = 0;

		for (Obstacle x : gen.getFalling()) {
			x.move();
			if (x.pos_y > max) {
				max = x.pos_y;
			}
		}
		if (max >= COURT_HEIGHT - 40) {
			gen.removeFalling();
		}
		// add trees

		min = 1500;
		for (FractalTree x : gen.trees) {
			x.move();
			if (x.xTemp[0] + 1000 < min) {
				min = x.xTemp[0] + 1000; // ensures that the trees will be
											// deleted, eventually!
			}
		}
		if (min <= 1) {
			gen.trees.poll();
		}

	}

	private void handleCollisions() {
		// intersect with obstacle

		if (cat.intersects2(poison) || cat.pos_x <= 1) {
			cat.die();
			status.setText("You lose!");
		}

		for (Obstacle x : gen.get()) {
			if (cat.intersects2(x) && !cat.moused) {
				x.sideEffect(this, cat);
			}
		}
		for (Obstacle x : gen.getMice()) {
			if (cat.intersects2(x)) {
				x.sideEffect(this, cat);
			}
		}
		for (Obstacle x : gen.getFalling()) {
			if (cat.intersects2(x)) {
				x.sideEffect(this, cat);
			}
		}

	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		for (FractalTree x : gen.trees) {
			if (cat.watered) {

				x.paintTree(g, Color.black, Color.black);
			} else {

				x.paintTree(g, Color.darkGray, Color.gray);
			}

		}
		g.setColor(Color.BLACK);
		for (GameObj x : gen.get()) {
			x.draw(g);
		}
		for (GameObj x : gen.getMice()) {
			x.draw(g);
		}
		for (GameObj x : gen.getFalling()) {
			x.draw(g);
		}

		cat.draw(g);
		poison.draw(g);

		// show the isntructions page

		if (showInstructions) {
			ImageIcon i = new ImageIcon("instructions.gif");
			Image ii = i.getImage();
			g.drawImage(ii, 0, 0, 1200, 400, null);
			playing = false;
			
		} else if (cat.dead) {
			ImageIcon i = new ImageIcon("over.gif");
			Image ii = i.getImage();
			g.drawImage(ii, 0, 0, 1200, 400, null);
			playing = false;
			scoreT.stop();
			
		}

	}

	@Override
	public Dimension getPreferredSize() {
		return new Dimension(COURT_WIDTH, COURT_HEIGHT);
	}

	public void gameOver() {

	}
}
