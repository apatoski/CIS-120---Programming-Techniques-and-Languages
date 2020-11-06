import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/**
 * CIS 120 Game HW
 * (c) University of Pennsylvania
 * @version 2.0, Mar 2013
 */

// imports necessary libraries for Java swing

/**
 * Game Main class that specifies the frame and widgets of the GUI
 */

public class Game implements Runnable {
	public boolean isOkay(String username) {
		for (char x : username.toCharArray()) {
			if (!Character.isLetterOrDigit(x)) {
				return false;
			}

		}
		return true;
	}

	public void run() {
		// NOTE : recall that the 'final' keyword notes inmutability
		// even for local variables.

		// Top-level frame in which game components live
		// Be sure to change "TOP LEVEL FRAME" to the name of your game
		final JFrame frame = new JFrame("Run, Kitty!");
		frame.setLocation(400, 1200);
		frame.setResizable(false);
		frame.setBackground(Color.BLACK);

		// Status panel
		final JPanel status_panel = new JPanel();
		frame.add(status_panel, BorderLayout.SOUTH);

		final JPanel score_panel = new JPanel();
		frame.add(score_panel, BorderLayout.NORTH);
		score_panel.setBackground(Color.BLACK);
		score_panel.setForeground(Color.ORANGE);

		final JLabel commands = new JLabel("Reset - R, Options - O");
		commands.setForeground(Color.ORANGE);
		final JLabel status = new JLabel("");
		status.setForeground(Color.ORANGE);
		String user;

		do {
			user = JOptionPane.showInputDialog("Enter your nick (letters and numbers only)");
		} while (user == null || !isOkay(user));

		final JLabel username = new JLabel("Username: " + user);

		username.setForeground(Color.ORANGE);
		final JLabel score = new JLabel("Score: 0");
		score.setForeground(Color.ORANGE);
		status_panel.add(commands);
		status_panel.add(new JLabel("                                   "));
		status_panel.add(status);
		status_panel.add(new JLabel("                                   "));
		status_panel.add(username);
		status_panel.add(score);
		status_panel.setBackground(Color.BLACK);

		final GameModel court = new GameModel(status, username, score);
		frame.add(court, BorderLayout.CENTER);

		// Reset button
		// final JPanel control_panel = new JPanel();
		// frame.add(control_panel, BorderLayout.NORTH);

		// Note here that when we add an action listener to the reset
		// button, we define it as an anonymous inner class that is
		// an instance of ActionListener with its actionPerformed()
		// method overridden. When the button is pressed,
		// actionPerformed() will be called.
		// final JButton reset = new JButton("Reset");
		// reset.addActionListener(new ActionListener() {
		// public void actionPerformed(ActionEvent e) {
		// court.reset();
		// }
		// });
		// control_panel.add(reset);

		// Put the frame on the screen
		frame.pack();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);

		// Start game
		court.reset();
	}

	/*
	 * Main method run to start and run the game Initializes the GUI elements
	 * specified in Game and runs it IMPORTANT: Do NOT delete! You MUST include
	 * this in the final submission of your game.
	 */
	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Game());
	}
}
