import java.util.Scanner;
import java.io.File;
import peasy.*;

final float CUBE_SIZE = 50f;
final float GRASS_HEIGHT = 2f;
final float ROAD_HEIGHT = 3.4f;
final float WATER_DEPTH = 4f;

class Cell {
    float x, y, z;
    float w, h, d;
    int r, g, b;
    
    Cell(float x, float y, float z,
         float w, float h, float d,
         int r, int g, int b) {
        this.x = x; this.y = y; this.z = z;
        this.w = w; this.h = h; this.d = d;
        this.r = r; this.g = g; this.b = b;
    }
    
    void draw() {
        pushMatrix();
        fill(color(r, g, b));
        translate(x, y, z);
        box(w, h, d);
        popMatrix();
    }
}

PVector findCenter(Cell[][] cells) {
    final int w = cells.length;
    final int h = cells[0].length;
    
    final Cell topLeft = cells[0][0];
    final Cell bottomRight = cells[w-1][h-1];
    
    final float originX = (topLeft.x + bottomRight.x)/2f;
    final float originY = (topLeft.y + bottomRight.y)/2f;
    
    return new PVector(originX, originY, 0f);
}

Cell newGrassCell(float x, float y) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, GRASS_HEIGHT/2f, CUBE_SIZE, CUBE_SIZE, GRASS_HEIGHT, 35, 198, 87);
}

Cell newWaterCell(float x, float y) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, -WATER_DEPTH/2f, CUBE_SIZE, CUBE_SIZE, WATER_DEPTH, 60, 139, 238);
}

Cell newBuildingCell(float x, float y, float d) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, CUBE_SIZE*d/2f, CUBE_SIZE, CUBE_SIZE, CUBE_SIZE*d, 176, 146, 208);
}

Cell newRoadCell(float x, float y) {
    return new Cell(x*CUBE_SIZE, y*CUBE_SIZE, ROAD_HEIGHT/2f, CUBE_SIZE, CUBE_SIZE, ROAD_HEIGHT, 24, 24, 24);
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
            case 'g':
                cells[x][y] = newGrassCell(x,y);
                break;
            case '.':
                cells[x][y] = newRoadCell(x,y);
                break;
            case 'w':
                cells[x][y] = newWaterCell(x,y);
                break;
            default:
                int d;
                if(c<='9')
                    d = c-'0';
                else
                    d = c-'A'+10;
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


// Global variables
PeasyCam cam;
Cell[][] cells;
PVector center;

void setup() {
    size(600, 600, P3D);
    cells = cellsFromFile("example_cities/city2.txt");
    
    center = findCenter(cells);
    
    cam = new PeasyCam(this, center.x, center.y, center.z, 1500f);
    cam.rotateX(-0.7f);
    cam.setSuppressRollRotationMode();
    
    noStroke();
}

void draw() {
    background(255);

    pointLight(64, 64, 64, center.x, center.y, 1500);
    pointLight(64, 64, 64, 2*center.x, 2*center.y, 1500);
    pointLight(64, 64, 64, 0, 0, 1500);
    pointLight(64, 64, 64, center.x, 2*center.y+CUBE_SIZE, 1500);
    pointLight(64, 64, 64, 2*center.x+CUBE_SIZE, center.y, 1500);
    
    for(Cell[] row : cells) {
        for(Cell cell : row) {
            cell.draw();
        }
    }
}