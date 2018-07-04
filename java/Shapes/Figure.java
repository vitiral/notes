import java.awt.*;

/**
 * Simple abstract class for graphic figures.
 */

abstract class Figure {
    // x and y coordinates
    protected int x, y;

    /**
     * Constructor: takes two parameters, the X and Y coordinates.
     */
    public Figure(int inX, int inY) {
        x = inX;
        y = inY;
    }

    /**
     * Abstract method for drawing this thing.
     *
     * The `g` parameter is a 'pen' that can be used to draw things in the
     * window.
     */
    public abstract void draw(Graphics g);

    /**
     * Move the figure to `(newX, newY)`.
     */
    public void move(int newX, int newY) {
        x = newX;
        y = newY;
    }
}
