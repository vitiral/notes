import java.awt.*;


/**
 * Circle Class. The coordinates represent the circle's center.
 */
class Circle extends Figure {
    private int diam;

    public Circle(int inX, int inY, int inDiam) {
        super(inX, inY);
        diam = inDiam;
    }

    public void draw(Graphics g) {
        g.drawOval(x, y, diam, diam);
    }
}
