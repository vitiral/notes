import java.awt.*;

/**
 * A Square that can be drawn in the window.
 *
 * The coordinates represent the upper left corner of the square.
 */
class Square extends Figure {
    private int side;

    public Square(int inX, int inY, int inSide) {
        super(inX, inY);
        side = inSide;
    }

    public void draw(Graphics g) {
        g.drawRect(x, y, side, side);
    }
}

