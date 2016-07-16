import java.util.Scanner;
import java.io.File;

final float CUBE_SIZE = 50f;
final float GRASS_HEIGHT = 2f;
final float ROAD_HEIGHT = 3.4f;

final String TEST_CITY = 
    "25 25\n"+
    "..........................\n"+
    "..........................\n"+
    "..........................\n"+
    ".555555.3333.1111111.2222.\n"+
    ".555555.3333..............\n"+
    ".555555.3333.1111111.6666.\n"+
    ".555555.3333.1111111.6666.\n"+
    ".555555.3333.1111111.6666.\n"+
    ".555555.3333.1111111.6666.\n"+
    ".555555.3333.1111111.6666.\n"+
    ".............1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    ".555555.GGGG.1111111.6666.\n"+
    "..........................\n";

class Cell {
    float x, y, z;
    float w, h, d;
    int r, g, b;
    
    Cell(float x, float y, float z,
         float w, float h, float d,
         int r, int g, int b) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.w = w;
        this.h = h;
        this.d = d;
        this.r = r;
        this.g = g;
        this.b = b;
    }
    
    void draw() {
        pushMatrix();
        //noStroke();
        strokeWeight(1);
        fill(color(r, g, b));
        translate(x, y, z);
        box(w, h, d);
        popMatrix();
    }
}

Cell newGrassCell(float x, float y) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, 0, CUBE_SIZE, CUBE_SIZE, GRASS_HEIGHT, 35, 198, 87);
}

Cell newBuildingCell(float x, float y, float d) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, 0, CUBE_SIZE, CUBE_SIZE, CUBE_SIZE*d, 176, 146, 208);
}

Cell newRoadCell(float x, float y) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, 0, CUBE_SIZE, CUBE_SIZE, ROAD_HEIGHT, 24, 24, 24);
}

Cell[][] cellsFromScanner(Scanner s) {
    final int w = s.nextInt();
    final int h = s.nextInt();
    Cell[][] cells = new Cell[w][h];
    
    s.nextLine();
    for(int y=0; y<h; y++) {
        String line = s.nextLine();
        for(int x=0; x<w; x++) {
            char c = line.charAt(x);
            switch(c) {
            case 'G':
                cells[x][y] = newGrassCell(x,y);
                break;
            case '.':
                cells[x][y] = newRoadCell(x,y);
                break;
            default:
                int d = c-'0';
                cells[x][y] = newBuildingCell(x,y,d);
                break;
            }
        }
    }

    return cells;
}

Cell[][] cellsFromFile(String path) {
    Scanner s = null;
    path = dataPath(path);
    try {        
        File f = new File(path);
        s = new Scanner(f);
    } catch(Exception e) {
        println("Exception in opening file: " + path + "\n" + e);
        return null;
    }
     
    Cell[][] cells = cellsFromScanner(s);
    s.close();
    return cells;
}

Cell[][] cellsFromString(String str) {
    Scanner s = new Scanner(str);
    Cell[][] cells = cellsFromScanner(s);
    s.close();
    return cells;
}

Cell[][] cells;

void setup() {
    size(600, 600, P3D);
    cells = cellsFromFile("example_cities/city1.txt");
}

void draw() {
    background(255);
    lights();
    
    camera(-200,-200,800, 635, 625, 0, 0, 0, -1);
    
    for(Cell[] row : cells) {
        for(Cell cell : row) {
            cell.draw();
        }
    }
}