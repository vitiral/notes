import java.awt.*;

/**
 * A simple window to display graphic figures in.
 *
 * The window is a subclass of the java `Frame` class, which describes graphic
 * windows. The window keeps it's figures in the array.
 *
 * The Java window System (AWT) automatically calls the paint method in the
 * Frame class whenever the window's contents need to be redrawn. A new
 * implementation of paint is provided in FigureWindow to handle the drawing.
 */
class FigureWindow extends Frame {
    private Figure[] figures;
    private int nbrOfFigures;

    public static void main(String[] args) {
        FigureWindow w = new FigureWindow(10);
        w.setSize(400, 300);
        w.addFigure(new Square(50, 50, 200));
        w.addFigure(new Circle(200, 100, 150));
        w.addFigure(new Circle(300, 200, 200));
        w.show();
    }

    /**
     * Constructor: the parameters indicate the maximal # of figures.
     */
    public FigureWindow(int max) {
        super("Fabulous Figures"); // window title
        figures = new Figure[max];
        nbrOfFigures = 0;
    }

    /**
     * Add the figure f to the window.
     *
     * If the maximal number of figures has been reached, nothing happens.
     */
    public void addFigure(Figure f) {
        if (nbrOfFigures < figures.length) {
            figures[nbrOfFigures] = f;
            nbrOfFigures++;
        }
    }

    /**
     * This method is called automatically by the system.
     *
     * Draws the graphic figures associated with the window.
     *
     * The `g` parameter is the drawing 'pen' provided by the system.
     */
    public void paint(Graphics g) {
        for (int i = 0; i < nbrOfFigures; i++) {
            figures[i].draw(g);
        }
    }

}
