import java.awt.Color;
import java.awt.Graphics;
import java.awt.Polygon;

// implements recursion / complex data structure
public class FractalTree {

	private int initSize;
	private int depth;
	public int pos_x;
	private RectNode root;
	private final int q = 370;
	private final int velocity = -1;
	public int[] xTemp;
	public Color c1, c2;

	public FractalTree(int init, int depth, int x) {

		initialize(init, depth, x);

	}

	private void initialize(int init, int depth, int x) {
		initSize = init;
		this.depth = depth;
		pos_x = x;
		root = new RectNode(depth, new int[] { x - initSize / 2, x + initSize / 2, x + initSize / 2, x - initSize / 2 },
				new int[] { q - 0, q - 0, q - initSize, q - initSize });
	}

	class TriangleNode extends Polygon {

		public RectNode childLeft, childRight;

		public TriangleNode(int depth, int[] xpoints, int[] ypoints) {
			this.xpoints = xpoints;
			this.ypoints = ypoints;
			this.npoints = 3;

			childLeft = null;
			childRight = null;

			// doncha worry, doncha woryyy child
			if (depth > 1) {
				int[] help4 = nextPoint(xpoints[0], ypoints[0], xpoints[2], ypoints[2]); // could
																							// have
																							// been
																							// done
																							// with
																							// a
																							// Point
																							// class,but..
				int[] help3 = nextPoint(help4[0], help4[1], xpoints[0], ypoints[0]);
				childLeft = new RectNode(depth - 1, new int[] { xpoints[0], xpoints[2], help3[0], help4[0] },
						new int[] { ypoints[0], ypoints[2], help3[1], help4[1] });

				help4 = nextPoint(xpoints[2], ypoints[2], xpoints[1], ypoints[1]);
				help3 = nextPoint(help4[0], help4[1], xpoints[2], ypoints[2]);
				childRight = new RectNode(depth - 1, new int[] { xpoints[2], xpoints[1], help3[0], help4[0] },
						new int[] { ypoints[2], ypoints[1], help3[1], help4[1] });
			}

		}

		private int[] nextPoint(int x1, int y1, int x2, int y2) {
			int x = x1 + (y2 - y1);
			int y = y1 + (x1 - x2);
			return new int[] { x, y };
		}

	}

	class RectNode extends Polygon {

		public TriangleNode child = null;

		public RectNode(int depth, int[] xpoints, int[] ypoints) {
			this.xpoints = xpoints;
			this.ypoints = ypoints;
			this.npoints = 4;

			if (depth > 1) {
				int[] third = thirdPoint(xpoints[3], xpoints[2], ypoints[3], ypoints[2]);

				child = new TriangleNode(--depth, new int[] { xpoints[3], xpoints[2], third[0] },
						new int[] { ypoints[3], ypoints[2], third[1] });
			}
		}

		private int[] thirdPoint(int x1, int x2, int y1, int y2) {
			// complex nums
			int x = (x1 + x2) / 2 + (y2 - y1) / 2;
			int y = (y1 + y2) / 2 + (x1 - x2) / 2;
			return new int[] { x, y };
		}
	}

	public void paintTree(Graphics g, Color col1, Color col2) {

		paintTreeRec(g, root, col1, col2);
	}

	private void paintTreeRec(Graphics g, Polygon node, Color col1, Color col2) {
		if (node == null) {
			return;
		}

		if (node instanceof TriangleNode) {
			g.setColor(col1);
		} else {
			g.setColor(col2);
		}

		g.fillPolygon(node);
		if (node instanceof TriangleNode) {
			paintTreeRec(g, ((TriangleNode) node).childLeft, col1, col2);
			paintTreeRec(g, ((TriangleNode) node).childRight, col1, col2);
		} else {
			paintTreeRec(g, ((RectNode) node).child, col1, col2);
		}

	}

	private void decreaseX(Polygon node) {
		if (node == null) {
			return;
		}
		// decreases
		xTemp = node.xpoints;

		for (int i = 0; i < xTemp.length; i++) {
			xTemp[i] += velocity;
		}

		if (node instanceof TriangleNode) {
			decreaseX(((TriangleNode) node).childLeft);
			decreaseX(((TriangleNode) node).childRight);
		} else {
			decreaseX(((RectNode) node).child);
		}

	}

	public void move() {
		decreaseX(root);
	}

	public void setColor(Graphics g, Color col1, Color col2) {
		c1 = col1;
		c2 = col2;
		paintTree(g, col1, col2);
	}
}
