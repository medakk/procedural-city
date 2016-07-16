import java.util.Scanner;
import java.io.File;
import peasy.*;

final float CUBE_SIZE = 10f;
final float GAP = CUBE_SIZE/3f;
final float TOTAL_SIZE = CUBE_SIZE+GAP;
final float GRASS_HEIGHT = CUBE_SIZE/10f;
final float ROAD_HEIGHT = CUBE_SIZE/15f;
final float WATER_HEIGHT = CUBE_SIZE/10f;

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

PVector findCenter(ArrayList<Cell> cells) {
    final float w = cells.get(0).w;
    final float h = cells.get(0).h;
    
    return new PVector(w/2f, h/2f, 0f);
}

Cell grassBlock(float x1, float y1, float x2, float y2) {
    final float w = (x2-x1)*CUBE_SIZE;
    final float h = (y2-y1)*CUBE_SIZE;
    final float mx = (x1+x2)/2f;
    final float my = (y1+y2)/2f;
    return new Cell(mx*TOTAL_SIZE, my*TOTAL_SIZE, GRASS_HEIGHT/2f, w, h, GRASS_HEIGHT, 35, 198, 87);
}

Cell waterBlock(float x1, float y1, float x2, float y2) {
    final float w = (x2-x1)*CUBE_SIZE;
    final float h = (y2-y1)*CUBE_SIZE;
    final float mx = (x1+x2)/2f;
    final float my = (y1+y2)/2f;
    return new Cell(mx*TOTAL_SIZE, my*TOTAL_SIZE, WATER_HEIGHT/2f, w, h, WATER_HEIGHT, 60, 139, 238);
}

Cell buildingBlock(float x1, float y1, float x2, float y2, float d) {
    final float w = (x2-x1)*CUBE_SIZE;
    final float h = (y2-y1)*CUBE_SIZE;
    final float mx = (x1+x2)/2f;
    final float my = (y1+y2)/2f;

    return new Cell(mx*TOTAL_SIZE, my*TOTAL_SIZE, CUBE_SIZE*d/2f, w, h, CUBE_SIZE*d, 176, 146, 208);
}

Cell roadBlock(float x1, float y1, float x2, float y2) {
    final float w = (x2-x1)*TOTAL_SIZE;
    final float h = (y2-y1)*TOTAL_SIZE;
    final float mx = (x1+x2)/2f;
    final float my = (y1+y2)/2f;
    return new Cell(mx*TOTAL_SIZE, my*TOTAL_SIZE, ROAD_HEIGHT/2f, w, h, ROAD_HEIGHT, 24, 24, 24);
}

ArrayList<Cell> cellsFromScanner(Scanner s) {
    ArrayList<Cell> cells = new ArrayList<Cell>();
    
    final int w = s.nextInt();
    final int h = s.nextInt();
    cells.add(roadBlock(0, 0, w, h));
    
    while(s.hasNextInt()) {
        int x1 = s.nextInt();
        int y1 = s.nextInt();
        int x2 = s.nextInt();
        int y2 = s.nextInt();
        
        String cellStr = s.next();

        switch(cellStr) {
        case "g": //grass
            cells.add(grassBlock(x1, y1, x2, y2));
            break;
        case "w": //water
            cells.add(waterBlock(x1, y1, x2, y2));
            break;
        default: //buildings
            int d;
            char c = cellStr.charAt(0);
            if(c<='9')
                d = c-'0';
            else
                d = c-'A'+10;
            cells.add(buildingBlock(x1, y1, x2, y2, d));
            break;
        }
        
    }

    return cells;
}

ArrayList<Cell> cellsFromFile(String path) {
    Scanner s = null;
    path = dataPath(path);
    try {        
        File f = new File(path);
        s = new Scanner(f);
    } catch(Exception e) {
        println("Exception in opening file: " + path + "\n" + e);
        return null;
    }
     
    ArrayList<Cell> cells = cellsFromScanner(s);
    s.close();
    return cells;
}


// Global variables
PeasyCam cam;
ArrayList<Cell> cells;
PVector center;

void setup() {
    size(600, 600, P3D);
    cells = cellsFromFile("example_cities/city3.txt");
    smooth(2);
    center = findCenter(cells);
    
    cam = new PeasyCam(this, center.x, center.y, center.z, TOTAL_SIZE*100f);
    cam.rotateX(-0.7f);
    cam.setSuppressRollRotationMode();
    
    noStroke();
}

void draw() {
    background(75, 79, 83);

    final float intensity = 48f;
    pointLight(intensity, intensity, intensity, center.x, center.y, TOTAL_SIZE*100f);
    pointLight(intensity, intensity, intensity, 2*center.x, 2*center.y, TOTAL_SIZE*100f);
    pointLight(intensity, intensity, intensity, 0, 0, TOTAL_SIZE*100f);
    pointLight(intensity, intensity, intensity, center.x, 2*center.y+TOTAL_SIZE, TOTAL_SIZE*100f);
    pointLight(intensity, intensity, intensity, 2*center.x+TOTAL_SIZE, center.y, TOTAL_SIZE*100f);
    
    for(Cell cell : cells) {
        cell.draw();
    }
}