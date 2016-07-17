# Procedural Cities
The meshes for the cities are generated using [Haskell](https://www.haskell.org/). These meshes are then rendered using [Processing](https://www.processing.org).

<center><img src="https://raw.githubusercontent.com/medakk/procedural-city/master/readme_imgs/1.png" alt="Image of procedural city" width=70% height=70%/></center>

<br>

<center><img src="https://raw.githubusercontent.com/medakk/procedural-city/master/readme_imgs/2.png" alt="Image of procedural city" width=70% height=70%/></center>

<br>

## Build
* Clone this repository
* Run `stack build`
* Open Processing, open Sketch > Import Library > Add Library. Search for [PeasyCam](http://www.mrfeinberg.com/peasycam/) and click Install.

## Run
    Usage:
    ./procedural-city.sh -w WIDTH -h HEIGHT -d ITERATIONS
Use `WIDTH` and `HEIGHT` to specify the size of the city in unit cubes. A higher value of `ITERATIONS` will create more dense cities.

Example:

    ./procedural-city.sh -w 50 -h 50 -d 6
